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
#include <X11/Xlib.h>

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

int opt_quiet = 0;		/* don't print results */
int opt_nowait = 0;

Atom xa_sawmill_request, xa_sawmill_request_win;
Window portal, request_win;
Display *dpy;

static u_long
eval_lisp_form(char *form)
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
usage(char *prog_name)
{
    fprintf(stderr, "usage: %s OPTIONS...\n\n\
where OPTIONS are any of:\n\n\
	-display X	Connect to the window manager on display X\n\
        -w              Don't wait for server if not already running,\n\
                         return with exit code 1\n\
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
    Atom type;
    int format;
    u_long bytes_after, nitems;
    u_char *data;
    int done_message = 0;

    argc--; argv++;

    for (i = 0; i < argc; i++)
    {
	if (strcmp ("-display", argv[i]) == 0 && i + 1 < argc)
	    display = argv[i+1];
	if (strcmp ("-w", argv[i]) == 0)
	    opt_nowait = 1;
	else if (strcmp ("-?", argv[i]) == 0 || strcmp ("-h", argv[i]) == 0)
	{
	    usage (prog_name);
	    return 0;
	}
    }

    dpy = XOpenDisplay (display);
    if (dpy == 0)
    {
	fprintf (stderr, "can't open display: `%s'\n", display ? display : "");
	return 5;
    }

    xa_sawmill_request = XInternAtom (dpy, "_SAWMILL_REQUEST", False);
    xa_sawmill_request_win = XInternAtom (dpy, "_SAWMILL_REQUEST_WIN", False);

again:
    if (XGetWindowProperty (dpy, DefaultRootWindow (dpy),
			    xa_sawmill_request_win, 0, 1, False,
			    XA_CARDINAL, &type, &format, &nitems,
			    &bytes_after, &data) != Success
        || type != XA_CARDINAL || format != 32 || nitems != 1)
    {
	if (!opt_nowait)
	{
	    if (!done_message)
	    {
		fprintf(stderr, "server not running, waiting...\n");
		done_message = 1;
	    }
	    sleep (1);
	    goto again;
	}
	else
	{
	    fprintf(stderr, "server not running\n");
	    return 5;
	}
    }

    request_win = *(Window *) data;
    portal = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy),
				  -100, -100, 10, 10, 0, 0, 0);
    XSelectInput (dpy, portal, PropertyChangeMask);

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
		result = eval_lisp_form(buf);
		argc--; argv++;
		break;

	    case 'c':			/* -c COMMAND */
		if(argc < 2)
		    goto opt_error;
		strcpy(buf, "(call-command '");
		strcat(buf, argv[1]);
		strcat(buf, ")");
		result = eval_lisp_form(buf);
		argc--; argv++;
		break;

	    case 'e':			/* -e FORM */
		if(argc < 2)
		    goto opt_error;
		result = eval_lisp_form(argv[1]);
		argc--; argv++;
		break;

	    case 0:
		do {
		    if(isatty(0))
			printf("sawmill%% "), fflush(stdout);
		    if(fgets(buf, sizeof(buf), stdin) == 0)
			result = 10;
		    else
			result = eval_lisp_form(buf);
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
			result = eval_lisp_form(input_buf);
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

    XDestroyWindow (dpy, portal);
    XCloseDisplay (dpy);
    return result;
}
