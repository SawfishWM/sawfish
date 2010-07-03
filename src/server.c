/* server.c -- client/server backend
   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include "sawfish.h"
#include "server.h"

#ifdef HAVE_UNIX

#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
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
DEFSYM(server_eval, "server-eval");

#define SOCK_IO(op, sock, buf, len)			\
	char *buf__ = (char *)buf;			\
	int todo__ = len;				\
	while(todo__ > 0) {				\
	    int this__ = op (sock, buf__, todo__);	\
	    if(this__ < 0) {				\
		if (errno != EINTR)			\
		    return -1;				\
	    }						\
	    else if(this__ == 0)			\
		break;					\
	    else {					\
		todo__ -= this__;			\
		buf__ += this__;			\
	    }						\
	}						\
	return len - todo__;

static int
sock_write (int fd, void *buf, size_t len)
{
    SOCK_IO (write, fd, buf, len);
}

static int
sock_read (int fd, void *buf, size_t len)
{
    SOCK_IO (read, fd, buf, len);
}

static void
server_handle_request(int fd)
{
    unsigned char req;
    if(sock_read(fd, &req, 1) != 1)
	goto disconnect;

    /* Need this in case the client code tries to execute a grab */
    save_timestamp (get_server_timestamp ());

    switch(req)
    {
	unsigned long len;
	repv val;

    case req_eval:
    case req_eval_async:
	/* 1. read length field
	   2. read LENGTH bytes of FORM
	   3. eval and print FORM
	   4. write length of result-string
	   5. write LENGTH bytes of result string */
	if(sock_read(fd, &len, sizeof(unsigned long)) != sizeof(unsigned long)
	   || (val = rep_make_string(len + 1)) == rep_NULL
	   || sock_read(fd, rep_STR(val), len) != len)
	    goto io_error;
	rep_STR(val)[len] = 0;
	if (req != req_eval_async){
	  val = rep_call_lisp2 (global_symbol_value (Qserver_eval),
				val, Qnil);
	}else{
	  val = rep_call_lisp2 (global_symbol_value (Qserver_eval),
				val, Qt);
	}
	if (req != req_eval_async)
	{
	    if(val && rep_STRINGP(val))
	    {
		len = rep_STRING_LEN(val);
		if(sock_write(fd, &len, sizeof(unsigned long)) != sizeof(unsigned long)
		   || sock_write(fd, rep_STR(val), len) != len)
		    goto io_error;
	    }
	    else
	    {
		len = 0;
		if(sock_write(fd, &len, sizeof(unsigned long)) != sizeof(unsigned long))
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
    int confd;
    struct sockaddr_un addr;
    socklen_t addr_len = sizeof(addr);

    /* Linux manpage states that we can pass NULL for addr parameters,
       but that has been reported to crash on some systems.. */

    confd = accept(socket_fd, (struct sockaddr *) &addr, &addr_len);

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

/* initialisation */

void
server_init (void)
{
    char namebuf[256];
    repv user, name;

    rep_INTERN(server_eval);
    rep_mark_static (&socket_name);

    if (batch_mode_p ())
	return;

    name = Fsymbol_value (Qcanonical_display_name, Qt);
    user = Fuser_login_name ();

    if(!name || !user || !rep_STRINGP(name) || !rep_STRING(user))
	return;

#ifdef HAVE_SNPRINTF
    snprintf (namebuf, sizeof(namebuf), SAWFISH_SOCK_DIR, rep_STR(user));
#else
    sprintf (namebuf, SAWFISH_SOCK_DIR, rep_STR(user));
#endif

    /* Make the socket directory trying to ensure that it hasn't
       been compromised. */
    if (mkdir (namebuf, 0700) != 0)
    {
	if (errno == EEXIST)
	{
	    struct stat st;
	    if (stat (namebuf, &st) == 0)
	    {
		if (st.st_uid != getuid ())
		{
		    fprintf (stderr, "Owner of %s is not the current user\n",
			     namebuf);
		    return;
		}
		if (st.st_mode & (S_IRWXG | S_IRWXO))
		{
		    fprintf (stderr, "Permissions for %s are too lax\n",
			     namebuf);
		    return;
		}
	    }
	    else
	    {
		perror (namebuf);
		return;
	    }
	}
	else
	{
	    perror (namebuf);
	    return;
	}
    }

    /* Add the socket name */
    strcat (namebuf, "/");
    strcat (namebuf, rep_STR(name));

    /* Delete the socket if it exists */
    if(access(namebuf, F_OK) == 0)
    {
	/* Socket already exists. Delete it */
	unlink(namebuf);

	if (access (namebuf, F_OK) == 0)
	{
	    fprintf (stderr, "Can't delete %s\n", namebuf);
	    return;
	}
    }

    socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if(socket_fd >= 0)
    {
	struct sockaddr_un addr;
	addr.sun_family = AF_UNIX;
	strcpy(addr.sun_path, namebuf);
	if(bind(socket_fd, (struct sockaddr *)&addr,
		sizeof(addr.sun_family) + strlen(addr.sun_path) + 1) == 0)
	{
	    chmod (namebuf, 0700);
	    if(listen(socket_fd, 5) == 0)
	    {
		rep_unix_set_fd_nonblocking(socket_fd);
		rep_register_input_fd(socket_fd, server_accept_connection);

		socket_name = rep_string_dup (namebuf);
		return;
	    }
	    else
		perror ("listen");
	}
	else
	    perror ("bind");
	close(socket_fd);
    }
    else
	perror ("socket");
    socket_fd = -1;
}

void
server_kill (void)
{
    if(socket_fd > 0)
    {
	rep_deregister_input_fd(socket_fd);
	close(socket_fd);
	socket_fd = -1;
	unlink(rep_STR(socket_name));
	socket_name = rep_NULL;
    }
}

#endif /* HAVE_UNIX */
