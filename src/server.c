/* server.c -- client/server backend
   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawmill.

   sawmill is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawmill is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawmill; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include "sawmill.h"
#include "server.h"

#ifdef HAVE_UNIX

#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdarg.h>
#include <errno.h>

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

/* fd of the socket which clients connect to, or zero. */
static int socket_fd = -1;

/* pathname of the socket. */
static repv socket_name;

DEFSTRING(io_error, "server_make_connection:io");
DEFSTRING(val_fmt, "%S");
DEFSYM(server, "server");

static void
server_handle_request(int fd)
{
    u_char req;
    if(read(fd, &req, 1) != 1)
	goto disconnect;
    /* XXX this is a bit lame */
    last_event_time = CurrentTime;
    switch(req)
    {
	u_long len;
	repv val;

    case req_eval:
    case req_eval_async:
	/* 1. read length field
	   2. read LENGTH bytes of FORM
	   3. eval and print FORM
	   4. write length of result-string
	   5. write LENGTH bytes of result string */
	if(read(fd, &len, sizeof(u_long)) != sizeof(u_long)
	   || (val = rep_make_string(len + 1)) == rep_NULL
	   || read(fd, rep_STR(val), len) != len)
	    goto io_error;
	rep_STR(val)[len] = 0;
	val = Fread(Fcons(rep_MAKE_INT(0), val));
	if(val != rep_NULL)
	    val = Feval(val);
	if (req != req_eval_async)
	{
	    if(val != rep_NULL)
		val = Fformat(rep_LIST_3(Qnil, rep_VAL(&val_fmt), val));
	    if(val != rep_NULL && rep_STRINGP(val))
	    {
		len = rep_STRING_LEN(val);
		if(write(fd, &len, sizeof(u_long)) != sizeof(u_long)
		   || write(fd, rep_STR(val), len) != len)
		    goto io_error;
	    }
	    else
	    {
		len = 0;
		if(write(fd, &len, sizeof(u_long)) != sizeof(u_long))
		    goto io_error;
	    }
	}
	break;

    io_error:
	Fsignal(Qerror, rep_LIST_1(rep_VAL(&io_error)));
	return;

    case req_end_of_session:
    disconnect:
	rep_deregister_input_fd(fd);
	close(fd);
    }
    XFlush (dpy);
}

static void
server_accept_connection(int unused_fd)
{
    int confd = accept(socket_fd, NULL, NULL);
    if(confd >= 0)
    {
	/* Once upon a time, I started reading commands here. I think
	   it's cleaner to just register CONFD as an input source */
	rep_register_input_fd(confd, server_handle_request);

	/* CONFD will inherit the properties of SOCKET-FD, i.e. non-
	   blocking. Make it block.. */
	rep_unix_set_fd_blocking(confd);
    }
}

DEFUN("server-open-p", Fserver_open_p, Sserver_open_p, (void), rep_Subr0) /*
::doc:Sserver-open-p::
server-open-p

t if the edit-server is open.
::end:: */
{
    if(socket_fd >= 0)
	return(Qt);
    return(Qnil);
}

DEFSTRING(no_name, "Can't make socket name");

DEFUN_INT("server-open", Fserver_open, Sserver_open, (void), rep_Subr0, "") /*
::doc:Sserver-open::
server-open

Creates the socket (or whatever) so that the editor's client program can
send us messages.
::end:: */
{
    char namebuf[256];
    repv name;
    if(socket_fd >= 0)
	return(Qt);
    name = Fsystem_name();
    if(!name || !rep_STRINGP(name))
	return rep_NULL;
#ifdef HAVE_SNPRINTF
    snprintf(namebuf, sizeof(namebuf), "~/" SAWMILL_SOCK_NAME, rep_STR(name));
#else
    sprintf(namebuf, "~/" SAWMILL_SOCK_NAME, rep_STR(name));
#endif
    name = Flocal_file_name(rep_string_dup(namebuf));
    if(name && rep_STRINGP(name))
    {
	if(access(rep_STR(name), F_OK) == 0)
	{
	    /* Socket already exists. See if it's live */
	    struct sockaddr_un addr;
	    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
	    if(sock >= 0)
	    {
		strcpy(addr.sun_path, rep_STR(name));
		addr.sun_family = AF_UNIX;
		if(connect(sock, (struct sockaddr *)&addr,
                   sizeof(addr.sun_family) + strlen(addr.sun_path)) == 0)
		{
		    close(sock);
		    (*rep_message_fun)(rep_message,
				       "A server is already open.");
		    return(Qnil);
		}
		close(sock);
		/* Socket is probably parentless; delete it */
		(*rep_message_fun)(rep_message, "Deleted stale socket.");
		unlink(rep_STR(name));
	    }
	}
	socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if(socket_fd >= 0)
	{
	    struct sockaddr_un addr;
	    addr.sun_family = AF_UNIX;
	    strcpy(addr.sun_path, rep_STR(name));
	    if(bind(socket_fd, (struct sockaddr *)&addr,
		    sizeof(addr.sun_family) + strlen(addr.sun_path)) == 0)
	    {
		if(listen(socket_fd, 5) == 0)
		{
		    rep_unix_set_fd_nonblocking(socket_fd);
		    rep_register_input_fd(socket_fd, server_accept_connection);

		    socket_name = name;
		    return(Qt);
		}
	    }
	    rep_signal_file_error(Qnil);
	}
	else
	{
	    Fsignal(Qerror, rep_LIST_1(rep_VAL(&no_name)));
	}
	close(socket_fd);
	socket_fd = -1;
    }
    else
	rep_signal_file_error(Qnil);
    return rep_NULL;
}

DEFUN_INT("server-close", Fserver_close, Sserver_close, (void), rep_Subr0, "") /*
::doc:Sserver-close::
server-close

Stops listening for client messages.
::end:: */
{
    if(socket_fd > 0)
    {
	rep_deregister_input_fd(socket_fd);
	close(socket_fd);
	socket_fd = -1;
	unlink(rep_STR(socket_name));
	socket_name = rep_NULL;
    }
    return(Qt);
}


/* dl hooks */

repv rep_dl_feature;

rep_xsubr *rep_dl_subrs[] = {
    &Sserver_open_p,
    &Sserver_open,
    &Sserver_close,
    0
};

repv
rep_dl_init(repv file_name)
{
    rep_INTERN(server);
    rep_dl_feature = Qserver;
    rep_mark_static(&socket_name);
    return Qt;
}

void
rep_dl_kill(void)
{
    Fserver_close();
}

#endif /* HAVE_UNIX */

/*
;;;###autoload (autoload 'server-open-p "server")
;;;###autoload (autoload 'server-open "server" t)
;;;###autoload (autoload 'server-reply "server")
*/
