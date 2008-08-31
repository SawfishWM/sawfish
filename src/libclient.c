/* libclient.c -- shared code for client program to communicate with server
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
#include "libclient.h"
#include <X11/Xlib.h>
#include <X11/Xatom.h>

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#ifdef HAVE_UNIX
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <pwd.h>
# include <netdb.h>

# ifdef HAVE_FCNTL_H
#  include <fcntl.h>
# endif

# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif

# ifdef HAVE_SYS_UTSNAME_H
#  include <sys/utsname.h>
# endif

# ifndef PATH_MAX
#  define PATH_MAX 256
# endif
#endif /* HAVE_UNIX */

static void (*close_fun)(void);
static char * (*eval_fun)(char *form, int *lenp, int *errorp);

#define PROTOCOL_X11_VERSION 1



/* copied from src/unix_main.c */
static char *
system_name(void)
{
    unsigned char buf[256];
    struct hostent *h;

    static char *system_name;
    if(system_name)
	return system_name;

#ifdef HAVE_GETHOSTNAME
    if(gethostname((gpointer) buf, 256))
	return rep_NULL;
#else
    {
	struct utsname uts;
	uname(&uts);
	strncpy(buf, uts.nodename, 256);
    }
#endif
    h = gethostbyname((gpointer) buf);
    if(h)
    {
	if(!strchr(h->h_name, '.'))
	{
	    /* The official name is not fully qualified. Try looking
	       through the list of alternatives. */
	    char **aliases = h->h_aliases;
	    while(*aliases && !strchr(*aliases, '.'))
		aliases++;
	    system_name = strdup(*aliases ? *aliases : h->h_name);
	}
	else
	    system_name = strdup(h->h_name);
    }
    else
	system_name = strdup((gpointer) buf);
    return system_name;
}

static char *
canonical_host (char *host)
{
    static char buf[256];
    char *ptr;

    /* check that the name is fully qualified */
    if (!strchr (host, '.'))
    {
	struct hostent *h = gethostbyname (host);
	if (h != 0)
	{
	    if (!strchr (h->h_name, '.'))
	    {
		char **aliases = h->h_aliases;
		while (*aliases && !strchr (*aliases, '.'))
		    aliases++;
		host = *aliases ? *aliases : h->h_name;
	    }
	    else
		host = h->h_name;
	}
    }

    ptr = buf;
    while (*host != 0)
    {
	*ptr++ = tolower (*host);
	host++;
    }
    return buf;
}

static char *
canonical_display (char *name)
{
    static char buf[256];
    char *ptr = buf;
    if (strncmp ("unix:", name, 5) == 0)
	name += 4;
    if (*name == ':')
    {
	char *host = system_name ();
	if (host != 0)
	    strcpy (ptr, host);
	else
	    *ptr = 0;
	ptr += strlen (ptr);
    }
    else
    {
	char *fq;
	while (*name && *name != ':')
	    *ptr++ = *name++;
	*ptr = 0;
	fq = canonical_host (buf);
	if (fq != buf)
	{
	    strcpy (buf, fq);
	    ptr = buf + strlen (buf);
	}
    }
    *ptr++ = *name++;
    while (*name && *name != '.')
	*ptr++ = *name++;
    if (*name == 0)
	strcpy (ptr, ".0");
    else
	strcpy (ptr, name);
    return buf;
}

static char *
user_login_name (void)
{
    char *tmp = getlogin ();
    if(tmp == 0)
    {
	struct passwd *pwd = getpwuid(geteuid());
	if (pwd != 0)
	    tmp = pwd->pw_name;
    }
    return tmp;
}


/* using the X based server io */

Atom xa_sawfish_request, xa_sawfish_request_win;
Window portal, request_win;
Display *dpy;

static char *
net_server_eval (char *form, int *lenp, int *errorp)
{
    unsigned char *data = 0;
    unsigned long nitems;
    XEvent ev;

    XChangeProperty (dpy, portal, xa_sawfish_request, XA_STRING, 8,
                     PropModeReplace, (unsigned char *) form, strlen (form));
    /* swallow the event created by the above */
    XWindowEvent (dpy, portal, PropertyChangeMask, &ev);

    ev.xclient.type = ClientMessage;
    ev.xclient.window = DefaultRootWindow (dpy);
    ev.xclient.message_type = xa_sawfish_request;
    ev.xclient.format = 32;
    ev.xclient.data.l[0] = PROTOCOL_X11_VERSION;
    ev.xclient.data.l[1] = portal;
    ev.xclient.data.l[2] = xa_sawfish_request;
    ev.xclient.data.l[3] = (lenp != 0);
    XSendEvent (dpy, request_win, False, 0L, &ev);

    /* Wait for the wm to delete or update the results */
    XWindowEvent (dpy, portal, PropertyChangeMask, &ev);

    if (lenp != 0)
    {
	Atom type;
	int format;
        long long_length = 16;
	unsigned long bytes_after;

	while (1)
	{
	    if (data != 0)
		XFree (data);
	    if (XGetWindowProperty (dpy, portal, xa_sawfish_request, 0,
				    long_length, False, XA_STRING,
				    &type, &format, &nitems,
				    &bytes_after, &data) != Success)
		return 0;
	    if (type != XA_STRING || format != 8 )
		return 0;
	    if (bytes_after == 0)
		break;
	    long_length += (bytes_after / 4) + 1;
	}
	
	if(nitems > 0)
	{
	    char *ret = malloc (nitems - 1);
	    memcpy (ret, data + 1, nitems - 1);
	    *lenp = nitems - 1;
	    *errorp = (*data != '\001');
	    XFree (data);
	    return ret;
	}
    }
    return 0;
}

static void
net_server_close (void)
{
    XDestroyWindow (dpy, portal);
    XCloseDisplay (dpy);
}

/* returns 0 if ok, <0 if error, >0 if server doesn't exist */
static int
net_server_init (char *display)
{
    Atom type;
    int format;
    unsigned long bytes_after, nitems;
    unsigned char *data;

    dpy = XOpenDisplay (display);
    if (dpy == 0)
	return 1;

    xa_sawfish_request = XInternAtom (dpy, "_SAWFISH_REQUEST", False);
    xa_sawfish_request_win = XInternAtom (dpy, "_SAWFISH_REQUEST_WIN", False);

    if (XGetWindowProperty (dpy, DefaultRootWindow (dpy),
			    xa_sawfish_request_win, 0, 1, False,
			    XA_CARDINAL, &type, &format, &nitems,
			    &bytes_after, &data) != Success
        || type != XA_CARDINAL || format != 32 || nitems != 1)
    {
	return 1;
    }

    request_win = *(Window *) data;
    portal = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy),
				  -100, -100, 10, 10, 0, 0, 0);
    XSelectInput (dpy, portal, PropertyChangeMask);

    eval_fun = net_server_eval;
    close_fun = net_server_close;

    return 0;
}


/* unix domain socket server */

#ifdef HAVE_UNIX

int socket_fd = -1;

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

static char *
unix_server_eval (char *form, int *lenp, int *errorp)
{
    /* Protocol is; >req_eval:1, >FORM-LEN:4, >FORM:?, <RES-LEN:4, <RES:?
       in the local byte-order. */
    unsigned char req = (lenp != 0) ? req_eval : req_eval_async;
    unsigned long len = strlen(form);
    char *result;

    if(sock_write(socket_fd, &req, 1) != 1
       || sock_write(socket_fd, &len, sizeof(unsigned long)) != sizeof(unsigned long)
       || sock_write(socket_fd, form, len) != len
       || (req != req_eval_async
	   && sock_read(socket_fd, &len, sizeof(unsigned long)) != sizeof(unsigned long)))
    {
	perror("eval_req");
	return 0;
    }
    if(lenp != 0)
    {
	if(len > 0)
	{
	    char state;
	    result = malloc (len - 1);
	    if(result == 0
	       || sock_read(socket_fd, &state, 1) != 1
	       || sock_read(socket_fd, result, len - 1) != len - 1)
	    {
		perror("eval_req");
		free (result);
		return 0;
	    }
	    *lenp = len - 1;
	    *errorp = (state != '\001');
	    return result;
	}
    }
    return 0;
}

static void
unix_server_close (void)
{
    close(socket_fd);
}

/* returns 0 if ok, <0 if error, >0 if server doesn't exist */
static int
unix_server_init (char *display)
{
    struct sockaddr_un addr;
    sprintf(addr.sun_path, SAWMILL_SOCK_DIR "/%s",
	    user_login_name (), display);
    addr.sun_family = AF_UNIX;

    socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if(socket_fd >= 0)
    {
	if(connect(socket_fd, (struct sockaddr *)&addr,
		   sizeof(addr.sun_family) + strlen(addr.sun_path) + 1) == 0)
	{
	    eval_fun = unix_server_eval;
	    close_fun = unix_server_close;
	    return 0;
	}
	close (socket_fd);
	fprintf (stderr, "error: can't connect to socket %s\n", addr.sun_path);
	return 1;
    }
    perror ("socket");
    return -1;
}

#endif /* HAVE_UNIX */


/* entry points */

int
client_open (char *display)
{
    int ret;

    if (display == 0)
	display = getenv ("DISPLAY");
    if (display == 0)
    {
	fprintf (stderr, "no display specified\n");
	return -1;
    }

    display = canonical_display (display);

#ifdef HAVE_UNIX
    ret = unix_server_init (display);
    if (ret > 0)
#endif
	ret = net_server_init (display);
    return ret;
}

char *
client_eval (char *form, int *lenp, int *errorp)
{
    return (*eval_fun) (form, lenp, errorp);
}

void
client_close (void)
{
    (*close_fun) ();
}
