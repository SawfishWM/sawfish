/* main.c -- Entry point for sawmill
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
   
#include "sawmill.h"
#include "build.h"
#include <string.h>
#include <limits.h>
#include <X11/Xlocale.h>		/* for setlocale () */

/* one of the ec_ values */
int exit_code = ec_exit;

/* Saved value of argv[0] */
static char *prog_name;

DEFSYM(sawfish_directory, "sawfish-directory");
DEFSYM(sawfish_lisp_lib_directory, "sawfish-lisp-lib-directory");
DEFSYM(sawfish_site_lisp_directory, "sawfish-site-lisp-directory");
DEFSYM(sawfish_exec_directory, "sawfish-exec-directory");
DEFSYM(sawfish_version, "sawfish-version"); /*
::doc:sawfish-directory::
The directory in which all of sawfish's installed data files live.
::end::
::doc:sawfish-lisp-lib-directory::
The name of the directory in which the standard lisp files live.
::end::
::doc:sawfish-site-lisp-directory::
The name of the directory in which site-specific Lisp files are stored.
::end::
::doc:sawfish-exec-directory::
The name of the directory containing sawfish's architecture specific files.
::end::
::doc:sawfish-version::
A string defining the version number of the current sawfish release.
::end:: */

/* some errors */
DEFSYM(window_error, "window-error");
DEFSTRING(err_window_error, "Window error");
DEFSYM(invalid_pos, "invalid-pos");
DEFSTRING(err_invalid_pos, "Invalid position");
DEFSYM(bad_event_desc, "bad-event-desc");
DEFSTRING(err_bad_event_desc, "Invalid event description");
DEFSTRING(version_string, SAWFISH_VERSION);

DEFSYM(saved_command_line_args, "saved-command-line-args");
DEFSYM(rep, "rep");
DEFSYM(fonts_are_fontsets, "fonts-are-fontsets");

static rep_bool
on_idle (int since_last)
{
    if (print_event_prefix ())
	return rep_TRUE;

    if (since_last == 0 && rep_recurse_depth == 0)
    {
	/* XXX There have been reports of sawmill locking the display,
	   XXX I've never seen it, but this may help, and shouldn't hurt.. */
	last_event_time = get_server_timestamp ();
	XUngrabPointer (dpy, last_event_time);
	XUngrabKeyboard (dpy, last_event_time);
	XFlush (dpy);
    }

    return rep_FALSE;
}

static void
on_termination (void)
{
}

DEFUN_INT("quit", Fquit, Squit, (void), rep_Subr0, "") /*
::doc:quit::
quit

Terminate the sawfish process.
::end:: */
{
    return Fthrow (Qquit, rep_MAKE_INT(0));
}

DEFUN_INT("restart", Frestart, Srestart, (void), rep_Subr0, "") /*
::doc:restart::
restart

Restart the sawfish process.
::end:: */
{
    exit_code = ec_restart;
    return Fquit ();
}

static void
sawfish_symbols (void)
{
    rep_INTERN_SPECIAL(sawfish_directory);
    if(getenv("SAWFISHDIR") != 0)
	Fset (Qsawfish_directory, rep_string_dup(getenv("SAWFISHDIR")));
    else
	Fset (Qsawfish_directory, rep_string_dup(SAWFISH_DIR));

    rep_INTERN_SPECIAL(sawfish_lisp_lib_directory);
    if(getenv("SAWFISHLISPDIR") != 0)
    {
	Fset (Qsawfish_lisp_lib_directory,
	      rep_string_dup(getenv("SAWFISHLISPDIR")));
    }
    else
	Fset (Qsawfish_lisp_lib_directory, rep_string_dup(SAWFISH_LISPDIR));

    rep_INTERN_SPECIAL(sawfish_site_lisp_directory);
    if(getenv("SAWFISHSITELISPDIR") != 0)
    {
	Fset (Qsawfish_site_lisp_directory,
	      rep_string_dup(getenv("SAWFISHSITELISPDIR")));
    }
    else
    {
	Fset (Qsawfish_site_lisp_directory,
	      rep_concat2 (rep_STR (Fsymbol_value (Qsawfish_directory, Qt)),
			   "/site-lisp"));
    }

    rep_INTERN_SPECIAL(sawfish_exec_directory);
    if(getenv("SAWFISHEXECDIR") != 0)
	Fset (Qsawfish_exec_directory, rep_string_dup(getenv("SAWFISHEXECDIR")));
    else
	Fset (Qsawfish_exec_directory, rep_string_dup(SAWFISH_EXECDIR));

    if(getenv("SAWFISHDOCFILE") != 0)
	Fset (Qdocumentation_file, rep_string_dup(getenv("SAWFISHDOCFILE")));
    else
    {
	Fset (Qdocumentation_file,
	      rep_concat2 (rep_STR (Fsymbol_value
				    (Qsawfish_exec_directory, Qt)), "/DOC"));
    }

    Fset (Qdocumentation_files, Fcons(Fsymbol_value (Qdocumentation_file, Qt),
				      Fsymbol_value (Qdocumentation_files, Qt)));

    Fset (Qload_path, Fcons (Fsymbol_value (Qsawfish_lisp_lib_directory, Qt),
			     Fcons (Fsymbol_value (Qsawfish_site_lisp_directory, Qt),
				    Fsymbol_value (Qload_path, Qt))));

    Fset (Qdl_load_path, Fcons (Fsymbol_value (Qsawfish_exec_directory, Qt),
				Fsymbol_value (Qdl_load_path, Qt)));

    rep_INTERN_SPECIAL(sawfish_version);
    Fset (Qsawfish_version, rep_VAL(&version_string));

    rep_INTERN(window_error); rep_ERROR(window_error);
    rep_INTERN(invalid_pos); rep_ERROR(invalid_pos);
    rep_INTERN(bad_event_desc); rep_ERROR(bad_event_desc);

    rep_on_idle_fun = on_idle;
    rep_on_termination_fun = on_termination;

    /* This should stop us getting trapped in any recursive edits */
    Fset (Qerror_mode, Qtop_level);
    Fset (Qinterrupt_mode, Qtop_level);

    rep_INTERN_SPECIAL(fonts_are_fontsets);
    Fset (Qfonts_are_fontsets, Qt);

    rep_ADD_SUBR_INT(Squit);
    rep_ADD_SUBR_INT(Srestart);
}

static void
stash_argv (int argc, char **argv)
{
    repv head, *last;

    head = Qnil;
    last = &head;
    while(argc > 0)
    {
	*last = Fcons(rep_string_dup(*argv), Qnil);
	last = &rep_CDR(*last);
	argc--;
	argv++;
    }
    rep_INTERN_SPECIAL(saved_command_line_args);
    Fset (Qsaved_command_line_args, head);
}

static void
do_restart (void)
{
    repv args = Fsymbol_value (Qsaved_command_line_args, Qt);
    if (rep_CONSP(args))
    {
	repv len = Flength (args);
	if (rep_INTP(len))
	{
	    int argc = rep_INT(len), i;
	    char **argv = alloca (sizeof (char *) * argc + 1);
	    for (i = 0; i < argc; i++)
	    {
		argv[i] = strdup (rep_STR(rep_CAR(args)));
		args = rep_CDR(args);
	    }
	    argv[i] = 0;

	    /* Only call this once we've finished accessing Lisp data. */
	    rep_kill ();

	    execvp (*argv, argv);
	}
    }
    exit (10);
}

static void
usage (void)
{
    fputs ("\
    --version		Print version details\n\
    --no-rc		Don't load rc or site-init files\n\
\n\
    --display=DPY	Connect to X display DPY\n\
    --visual=VISUAL	Preferred visual type\n\
    --depth=DEPTH	Preferred color depth\n\
    --disable-nls	Disable internationalization of messages\n\
\n\
    -f FUNCTION		Call the Lisp function FUNCTION\n\
    -l FILE		Load the file of Lisp forms called FILE\n\
    -q			Quit\n\
    FILE		Load the Lisp file FILE\n"
	   , stderr);
}

static repv
inner_main (repv arg)
{
    repv res = rep_load_environment(rep_string_dup ("sawmill"));
    if (res != rep_NULL)
    {
	if(!batch_mode_p ())
	{
	    /* final initialisation.. */
	    manage_windows ();

	    /* then jump into the event loop.. */
	    res = Frecursive_edit ();
	}
    }
    return res;
}

bool
batch_mode_p (void)
{
    repv tem = Fsymbol_value (Qbatch_mode, Qt);
    return tem != Qnil;
}

int
main(int argc, char **argv)
{
    volatile int rc = 5;
    char **old_argv = argv;
    int old_argc = argc;
    char *lang;

    prog_name = *argv++; argc--;
    lang = setlocale(LC_ALL, "");
    rep_init (prog_name, &argc, &argv, 0, usage);
    stash_argv (old_argc, old_argv);

    if (rep_get_option ("--version", 0))
    {
	printf ("sawfish version %s\n", SAWFISH_VERSION);
	return 0;
    }

    rep_push_structure ("sawfish");
    rep_structure_exports_all (rep_structure, rep_TRUE);
    rep_INTERN (rep);
    Frequire (Qrep);

    if (sys_init(prog_name))
    {
	sawfish_symbols();

	if (lang == 0
	    || strcmp (lang, "C") == 0
	    || strcmp (lang, "POSIX") == 0
	    || ! XSupportsLocale ())
	{
	    /* if setlocale fails, or returns an ASCII locale, or X just
	       doesn't support the lcoale, using fontsets fails to draw
	       8-bit characters. */
	    Fset (Qfonts_are_fontsets, Qnil);
	}

	/* call all init funcs... */
	session_init ();
	events_init ();
	images_init ();
	pixmap_cache_init ();
	colors_init ();
	fonts_init ();
	cursors_init ();
	frames_init ();
	windows_init ();
	commands_init ();
	keys_init ();
	functions_init ();
	server_init ();

	rep_call_with_barrier (inner_main, Qnil, rep_TRUE, 0, 0, 0);
	rc = rep_top_level_exit ();

	/* call all exit funcs... */
	server_kill ();
	functions_kill ();
	windows_kill ();
	frames_kill ();
	cursors_kill ();
	fonts_kill ();
	colors_kill ();
	images_kill ();
	events_kill ();
	session_kill ();

	sys_kill();

	if (exit_code == ec_restart)
	    /* This will call rep_kill () itself when ready */
	    do_restart ();

	rep_kill();
    }
    return rc;
}

void
add_hook (repv sym, repv fun)
{
    repv val = Fsymbol_value (sym, Qt);
    if (rep_VOIDP(val))
	val = Qnil;
    val = Fcons (fun, val);
    Fset (sym, val);
}

/* in rep 0.11 and earlier the gaol was broken, it was possible to read
   non-exported special variables; this was fixed in rep 0.12, so we
   need to read some symbols from a non-gaolled module */
repv
global_symbol_value (repv sym)
{
    repv value;
    repv tem = rep_push_structure ("sawfish");
    value = Fsymbol_value (sym, Qt);
    rep_pop_structure (tem);
    return value;
}
