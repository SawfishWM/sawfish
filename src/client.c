/* client.c -- client program to communicate with server.c
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
#include <X11/Xlib.h>

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

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

int opt_quiet = 0;		/* don't print results */

void (*close_fun)(void);
u_long (*eval_fun)(char *form);




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
canonical_display (char *name)
{
    static char buf[256];
    char *ptr = buf;
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
	while (*name && *name != ':')
	    *ptr++ = *name++;
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


/* using the X based server io */

Atom xa_sawmill_request, xa_sawmill_request_win;
Window portal, request_win;
Display *dpy;

static u_long
net_server_eval (char *form)
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
    ev.xclient.data.l[2] = !opt_quiet;
    XSendEvent (dpy, request_win, False, 0L, &ev);

    /* Wait for the wm to delete or update the results */
    XWindowEvent (dpy, portal, PropertyChangeMask, &ev);

    if (!opt_quiet)
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
		return 10;
	    if (type != XA_STRING || format != 8 )
		return 10;
	    if (bytes_after == 0)
		break;
	    long_length += (bytes_after / 4) + 1;
	}
	if(nitems > 0)
	{
	    fwrite (data, nitems, 1, stdout);
	    fputc ('\n', stdout);
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

static u_long
unix_server_eval (char *form)
{
    /* Protocol is; >req_eval:1, >FORM-LEN:4, >FORM:?, <RES-LEN:4, <RES:?
       in the local byte-order. */
    u_char req = !opt_quiet ? req_eval : req_eval_async;
    u_long len = strlen(form);
    char *result;

    if(write(socket_fd, &req, 1) != 1
       || write(socket_fd, &len, sizeof(u_long)) != sizeof(u_long)
       || write(socket_fd, form, len) != len
       || (req != req_eval_async
	   && read(socket_fd, &len, sizeof(u_long)) != sizeof(u_long)))
    {
	perror("eval_req");
	return 10;
    }
    if(req != req_eval_async)
    {
	if(len > 0)
	{
	    result = malloc(len);
	    if(result == 0 || read(socket_fd, result, len) != len)
	    {
		perror("eval_req");
		return 10;
	    }
	    fwrite (result, len, 1, stdout);
	    fputc ('\n', stdout);
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
    struct passwd *pwd = getpwuid(getuid());
    if(pwd && pwd->pw_dir)
    {
	struct sockaddr_un addr;
	char *end;
	strcpy(addr.sun_path, pwd->pw_dir);
	end = addr.sun_path + strlen(addr.sun_path);
	if(end[-1] != '/')
	    *end++ = '/';
	sprintf(end, SAWMILL_SOCK_NAME, display);
	addr.sun_family = AF_UNIX;

	if(access(addr.sun_path, F_OK) != 0)
	    return 1;

	socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if(socket_fd >= 0)
	{
	    if(connect(socket_fd, (struct sockaddr *)&addr,
		       sizeof(addr.sun_family) + strlen(addr.sun_path)) == 0)
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
    }
    else
	perror ("getpwuid");
    return -1;
}

#endif /* HAVE_UNIX */



static void
usage(char *prog_name)
{
    fprintf(stderr, "usage: %s OPTIONS...\n\n\
where OPTIONS are any of:\n\n\
	-display X	Connect to the window manager on display X\n\
	-q		Be quiet (perform commands asynchronously)\n\
	-f FUNCTION	Call Lisp function FUNCTION on the server\n\
	-c COMMAND	Call the interactive Lisp function COMMAND\n\
	-e FORM		Evaluate Lisp form FORM on the server\n\
	-		Read lines of input until EOF, evaluating each\n\
			 one as it is read\n\
	--		Read forms from standard input until EOF, evaluating\n\
			 the whole lot in one go (inside a progn)\n",
	    prog_name);
}
		
int
main(int argc, char *argv[])
{
    char *prog_name = argv[0];
    char *display = 0;
    long result = 0;
    int i;

    argc--; argv++;

    for (i = 0; i < argc; i++)
    {
	if (strcmp ("-display", argv[i]) == 0 && i + 1 < argc)
	    display = argv[i+1];
	else if (strcmp ("-?", argv[i]) == 0 || strcmp ("-h", argv[i]) == 0)
	{
	    usage (prog_name);
	    return 0;
	}
    }

    if (display == 0)
	display = getenv ("DISPLAY");
    if (display == 0)
    {
	fprintf (stderr, "no display specified\n");
	return 5;
    }

    display = canonical_display (display);

#ifdef HAVE_UNIX
    i = unix_server_init (display);
    if (i > 0)
#endif
	i = net_server_init (display);
    if (i != 0)
    {
	fprintf (stderr, "can't connect to display: %s\n", display);
	return 10;
    }

    while(result == 0 && argc > 0)
    {
	result = 5;
	if(**argv == '-')
	{
	    switch((*argv)[1])
	    {
		char buf[512];

	    case '?': case 'h': case 'w':
		result = 0;
		break;

	    case 'd':
		argc--; argv++;
		result = 0;
		break;

	    case 'q':
		opt_quiet = 1;
		result = 0;
		break;

	    case 'f':			/* -f FUNCTION */
		if(argc < 2)
		    goto opt_error;
		buf[0] = '(';
		strcpy(buf + 1, argv[1]);
		strcat(buf, ")");
		result = (*eval_fun) (buf);
		argc--; argv++;
		break;

	    case 'c':			/* -c COMMAND */
		if(argc < 2)
		    goto opt_error;
		strcpy(buf, "(call-command '");
		strcat(buf, argv[1]);
		strcat(buf, ")");
		result = (*eval_fun) (buf);
		argc--; argv++;
		break;

	    case 'e':			/* -e FORM */
		if(argc < 2)
		    goto opt_error;
		result = (*eval_fun) (argv[1]);
		argc--; argv++;
		break;

	    case 0:
		do {
		    if(isatty(0))
			printf("sawmill%% "), fflush(stdout);
		    if(fgets(buf, sizeof(buf), stdin) == 0)
			result = 10;
		    else
			result = (*eval_fun) (buf);
		} while(result == 0);
		argc--; argv++;
		break;

	    case '-':
		{
		    int bufsiz = 1024, bufuse = 0;
		    char *input_buf = malloc(bufsiz);
		    if(input_buf == 0)
		    {
			perror("malloc");
			result = 10;
			break;
		    }
		    strcpy(input_buf, "(progn ");
		    bufuse = 7;

		    while(fgets(buf, sizeof(buf), stdin) != 0)
		    {
			int len = strlen(buf);
			if(bufuse + len + 1 >= bufsiz)
			{
			    bufsiz *= 2;
			    input_buf = realloc(buf, bufsiz);
			    if(input_buf == 0)
			    {
				perror("realloc");
				result = 10;
				break;
			    }
			}
			memcpy(input_buf + bufuse, buf, len);
			bufuse += len;
		    }
		    if(input_buf != 0)
		    {
			input_buf[bufuse] = ')';
			input_buf[bufuse+1] = 0;
			result = (*eval_fun) (input_buf);
			free(input_buf);
		    }
		}
		break;

	    default:
	    opt_error:
		fprintf(stderr, "unknown option `%s'; try `%s -h'\n",
			*argv, prog_name);
		result =  5;
	    }
	    argc--; argv++;
	}
    }

    (*close_fun) ();

    return result;
}
