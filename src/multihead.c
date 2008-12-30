/* multihead.c -- Simple multihead support
   $Id$

   Copyright (C) 2001 John Harper <jsh@pixelslut.com>

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

/* AIX requires this to be the first thing in the file.  */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
   char *alloca ();
#   endif
#  endif
# endif
#endif
   
#include "sawfish.h"

#include <string.h>
#include <assert.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

static inline bool
argv_test_option (const char *option, int argc, char **argv,
		  int *positionp, char **value)
{
    int optlen = strlen (option);
    int position = (*positionp)++;

    if (strncmp (option, argv[position], optlen) != 0)
	return FALSE;

    if (value != 0)
    {
	/* Argument required */

	if (argv[position][optlen] == '=')
	    *value = argv[position] + optlen + 1;
	else if (argv[position][optlen] == 0 && position + 1 < argc)
	{
	    *value = argv[position + 1];
	    (*positionp)++;
	}
	else
	    return FALSE;
    }

    return TRUE;
}

static bool
argv_get_option (int argc, char **argv, const char *option, char **valuep)
{
    int i = 1;
    while (i < argc)
    {
	if (argv_test_option (option, argc, argv, &i, valuep))
	    return TRUE;
    }
    return FALSE;
}

static char **
copy_argv (int argc, char **argv)
{
    char **copy = malloc (sizeof (char *) * argc);
    if (copy == 0)
	abort ();
    memcpy (copy, argv, sizeof (char *) * argc);
    return copy;
}

static void
grow_argv (int *argcp, char ***argvp, int extra_slots)
{
    if (extra_slots > 0)
    {
	char **copy = malloc (sizeof (char *) * (*argcp + extra_slots));
	if (copy == 0)
	    abort ();
	memcpy (copy, *argvp, sizeof (char *) * *argcp);
	free (*argvp);
	*argvp = copy;
	*argcp += extra_slots;
    }
}

static void
argv_remove_option (int *argcp, char ***argvp, char *option, bool has_arg)
{
    int i = 1;

    while (i < *argcp)
    {
	char *value;
	int old_i = i;

	if (argv_test_option (option, *argcp, *argvp, &i, has_arg ? &value : 0))
	{
	    if (!has_arg || i - old_i == 1)
	    {
		/* Option only takes one slot, i.e. --foo=bar form.
		So just delete the single slot */
		memmove (*argvp + old_i, *argvp + old_i + 1,
			 sizeof (char *) * (*argcp - (old_i + 1)));
		*argcp -= 1;
	    }
	    else
	    {
		memmove (*argvp + old_i, *argvp + old_i + 2,
			 sizeof (char *) * (*argcp - (old_i + 2)));
		*argcp -= 2;
	    }

	    return;
	}
    }
}

static void
argv_set_option (int *argcp, char ***argvp, char *option, char *arg)
{
    int i = 1;

    while (i < *argcp)
    {
	char *value;
	int old_i = i;

	if (argv_test_option (option, *argcp, *argvp, &i, arg ? &value : 0))
	{
	    if (arg != 0)
	    {
		if (i - old_i == 1)
		{
		    /* Option only takes one slot, i.e. --foo=bar form.
		       So open up space for the second slot */
		    grow_argv (argcp, argvp, 1);
		    memmove (*argvp + i + 1, *argvp + i,
			     sizeof (char *) * (*argcp - (i + 1)));
		    (*argvp)[old_i] = option;
		    i++;
		}

		(*argvp)[old_i + 1] = arg;
	    }

	    return;
	}
    }

    /* No existing instance of this option. */
    if (arg != 0)
    {
	grow_argv (argcp, argvp, 2);
	(*argvp)[*argcp - 2] = option;
	(*argvp)[*argcp - 1] = arg;
    }
    else
    {
	grow_argv (argcp, argvp, 1);
	(*argvp)[*argcp - 1] = option;
    }
}

/* Spawn multiple copies, one per screen, if necessary */
void
multihead_init (int *argcp, char ***argvp)
{
    int argc = *argcp;
    char **argv = *argvp;
    char *dpy_name, *dpy_copy, *tem;
    Display *dpy;
    int i, master_screen, total_screens;

    if (!argv_get_option (argc, argv, "--multihead", NULL))
	return;

    fputs ("\n\
Warning: sawfish's --multihead option is known to have fundamental\n\
         design flaws, which may lead to sawfish behaving strangely, or\n\
         the files in your ~/.sawfish directory being corrupted. Use\n\
         this option at your own risk!\n\n", stderr);

    if (!argv_get_option (argc, argv, "--display", &dpy_name))
	dpy_name = getenv ("DISPLAY");

    dpy = XOpenDisplay (dpy_name);
    if (dpy == 0)
	return;

    argv = copy_argv (argc, argv);

    dpy_name = XDisplayString (dpy);
    dpy_copy = strdup (dpy_name);
    tem = strrchr (dpy_copy, '.');
    assert (tem != 0);
    tem[1] = 0;

    master_screen = DefaultScreen (dpy);
    total_screens = ScreenCount (dpy);

    /* For the master */
    argv_set_option (&argc, &argv, "--display", strdup (dpy_name));

    /* This will remove it from all future children as well.. */
    argv_remove_option (&argc, &argv, "--multihead", FALSE);

    XCloseDisplay (dpy);

    for (i = 0; i < total_screens; i++)
    {
	if (i != master_screen && fork () == 0)
	{
	    dpy_name = malloc (strlen (dpy_copy) + 6);
	    sprintf (dpy_name, "%s%d", dpy_copy, i);

	    /* Give each instance the correct display name */
	    argv_set_option (&argc, &argv, "--display", dpy_name);

	    break;
	}
    }

    *argcp = argc;
    *argvp = argv;
}
