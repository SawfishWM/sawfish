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

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>

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
static char * (*eval_fun)(char *form, int async, int *lenp);



/* copied from src/unix_main.c */
static char *
system_name(void)
{
    u_char buf[256];
    struct hostent *h;

    static char *system_name;
    if(system_name)
	return system_name;

#ifdef HAVE_GETHOSTNAME
    if(gethostname(buf, 256))
	return rep_NULL;
#else
    {
	struct utsname uts;
	uname(&uts);
	strncpy(buf, uts.nodename, 256);
    }
#endif
    h = gethostbyname(buf);
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
	    system_name = strdup((u_char *)h->h_name);
    }
    else
	system_name = strdup(buf);
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

Atom xa_sawmill_request, xa_sawmill_request_win;
Window portal, request_win;
Display *dpy;

static char *
net_server_eval (char *form, int async, int *lenp)
{
    u_char *data = 0;
    u_long nitems;
    XEvent ev;

    XChangeProperty (dpy, portal, xa_sawmill_request, XA_STRING,
		     8, PropModeReplace, form, strlen (form));
    /* swallow the event created by the above */
    XWindowEvent (dpy, portal, PropertyChangeMask, &ev);

    ev.xclient.type = ClientMessage;
    ev.xclient.window = DefaultRootWindow (dpy);
    ev.xclient.message_type = xa_sawmill_request;
    ev.xclient.format = 32;
    ev.xclient.data.l[0] = portal;
    ev.xclient.data.l[1] = xa_sawmill_request;
    ev.xclient.data.l[2] = !async;
    XSendEvent (dpy, request_win, False, 0L, &ev);

    /* Wait for the wm to delete or update the results */
    XWindowEvent (dpy, portal, PropertyChangeMask, &ev);

    if (!async)
    {
	Atom type;
	int format;
        long long_length = 16;
	u_long bytes_after;

	while (1)
	{
	    if (data != 0)
		XFree (data);
	    if (XGetWindowProperty (dpy, portal, xa_sawmill_request, 0,
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
	    char *ret = malloc (nitems);
	    memcpy (ret, data, nitems);
	    XFree (data);
	    *lenp = nitems;
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
    u_long bytes_after, nitems;
    u_char *data;

    dpy = XOpenDisplay (display);
    if (dpy == 0)
	return 1;

    xa_sawmill_request = XInternAtom (dpy, "_SAWMILL_REQUEST", False);
    xa_sawmill_request_win = XInternAtom (dpy, "_SAWMILL_REQUEST_WIN", False);

    if (XGetWindowProperty (dpy, DefaultRootWindow (dpy),
			    xa_sawmill_request_win, 0, 1, False,
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

static char *
unix_server_eval (char *form, int async, int *lenp)
{
    /* Protocol is; >req_eval:1, >FORM-LEN:4, >FORM:?, <RES-LEN:4, <RES:?
       in the local byte-order. */
    u_char req = !async ? req_eval : req_eval_async;
    u_long len = strlen(form);
    char *result;

    if(write(socket_fd, &req, 1) != 1
       || write(socket_fd, &len, sizeof(u_long)) != sizeof(u_long)
       || write(socket_fd, form, len) != len
       || (req != req_eval_async
	   && read(socket_fd, &len, sizeof(u_long)) != sizeof(u_long)))
    {
	perror("eval_req");
	return 0;
    }
    if(!async)
    {
	if(len > 0)
	{
	    result = malloc(len);
	    if(result == 0 || read(socket_fd, result, len) != len)
	    {
		perror("eval_req");
		free (result);
		return 0;
	    }
	    *lenp = len;
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

    if(access(addr.sun_path, R_OK) != 0)
	return 1;

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
	else
	    return 1;
    }
    else
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
client_eval (char *form, int async, int *lenp)
{
    return (*eval_fun) (form, async, lenp);
}

void
client_close (void)
{
    (*close_fun) ();
}
