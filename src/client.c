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

#include "libclient.h"
#include <config.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef HAVE_LIBREADLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

int opt_quiet = 0;		/* don't print results */


/* Symbol completion */

#ifdef HAVE_LIBREADLINE

static char *
completion_generator (char *word, int state)
{
    static char *last_buffer, *buffer_end, *next_word;

    if (state == 0)
    {
	/* first call; generate the list */
	int len;
	char *form = malloc (strlen (word) + 64);
	char *in, *out;
	if (last_buffer != 0)
	{
	    free (last_buffer);
	    last_buffer = 0;
	    next_word = 0;
	}
	strcpy (form, "(apropos \"^");
	in = word;
	out = form + strlen ("(apropos \"^");
	while (*in != 0)
	{
	    switch (*in)
	    {
	    case '*': case '+': case '?': case '.':
	    case '[': case ']': case '(': case ')':
	    case '|': case '^': case '$': case '\\':
		*out++ = '\\';
		*out++ = *in++;
		break;
	    default:
		*out++ = *in++;
	    }
	}
	strcpy (out, "\" boundp)");
	last_buffer = client_eval (form, 0, &len);
	if (last_buffer != 0)
	{
	    if (len == 3 && memcmp (last_buffer, "nil", 3) == 0)
	    {
		free (last_buffer);
		last_buffer = 0;
	    }
	    else
	    {
		buffer_end = last_buffer + len;
		next_word = last_buffer + 1;
	    }
	}
	free (form);
    }

    if (next_word != 0)
    {
	char *tem = next_word + strcspn (next_word, " )");
	if (tem != next_word)
	{
	    char *this = malloc (tem - next_word + 1);
	    memcpy (this, next_word, tem - next_word);
	    this[tem - next_word] = 0;

	    if (tem == buffer_end - 1)
	    {
		free (last_buffer);
		last_buffer = 0;
	        next_word = 0;
	    }
	    else
		next_word = tem + 1;

	    return this;
	}
    }
    return 0;
}

#endif



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

static u_long
do_eval (char *form)
{
    int length;
    char *result = client_eval (form, opt_quiet, &length);
    if (result != 0)
    {
	fwrite (result, length, 1, stdout);
	fputc ('\n', stdout);
	free (result);
    }
    return (opt_quiet || form != 0) ? 0 : 10;
}

int
main(int argc, char *argv[])
{
    char *prog_name = argv[0];
    char *display = 0;
    long result = 0;
    int i;
#ifdef HAVE_LIBREADLINE
    char *user_input = 0;

    rl_completion_entry_function = (void *)completion_generator;
    rl_basic_quote_characters = "\"";
#endif

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

    if (client_open (display) != 0)
    {
	fprintf (stderr, "can't connect to display: %s\n",
		 display ? display : "");
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
		result = do_eval (buf);
		argc--; argv++;
		break;

	    case 'c':			/* -c COMMAND */
		if(argc < 2)
		    goto opt_error;
		strcpy(buf, "(call-command '");
		strcat(buf, argv[1]);
		strcat(buf, ")");
		result = do_eval (buf);
		argc--; argv++;
		break;

	    case 'e':			/* -e FORM */
		if(argc < 2)
		    goto opt_error;
		result = do_eval (argv[1]);
		argc--; argv++;
		break;

	    case 0:
		do {
		    if(isatty(0))
#ifdef HAVE_LIBREADLINE
  		        user_input = readline("sawmill% ");
		    if (user_input == 0)
#else
		    {
			printf("sawmill%% ");
			fflush(stdout);
		    }
		    if(fgets(buf, sizeof(buf), stdin) == 0) 
#endif
			result = 10;
		    else
		    {
#ifdef HAVE_LIBREADLINE
			strcpy(buf, user_input);
			add_history(user_input);
#endif
			result = do_eval (buf);
		    }     
#ifdef HAVE_LIBREADLINE
		    free (user_input);
#endif
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
			    input_buf = realloc(input_buf, bufsiz);
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
			result = do_eval (input_buf);
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

    client_close ();

    return result;
}
