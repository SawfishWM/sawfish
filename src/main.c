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

#include "sawmill.h"
#include "build.h"
#include <string.h>
#include <limits.h>

/* longjmp () to this with one of the ec_ exit codes to quit in some way */
jmp_buf clean_exit_jmp_buf;

/* Saved value of argv[0] */
static char *prog_name;

DEFSYM(sawmill_directory, "sawmill-directory");
DEFSYM(sawmill_lisp_lib_directory, "sawmill-lisp-lib-directory");
DEFSYM(sawmill_site_lisp_directory, "sawmill-site-lisp-directory");
DEFSYM(sawmill_exec_directory, "sawmill-exec-directory");
DEFSYM(sawmill_version, "sawmill-version"); /*
::doc:Vsawmill-directory::
The directory in which all of sawmill's installed data files live.
::end::
::doc:Vsawmill-lisp-lib-directory::
The name of the directory in which the standard lisp files live.
::end::
::doc:Vsawmill-site-lisp-directory::
The name of the directory in which site-specific Lisp files are stored.
::end::
::doc:Vsawmill-exec-directory::
The name of the directory containing sawmill's architecture specific files.
::end::
::doc:Vsawmill-version::
A string defining the version number of the current sawmill release.
::end:: */

/* some errors */
DEFSYM(window_error, "window-error");
DEFSTRING(err_window_error, "Window error");
DEFSYM(invalid_pos, "invalid-pos");
DEFSTRING(err_invalid_pos, "Invalid position");
DEFSYM(bad_event_desc, "bad-event-desc");
DEFSTRING(err_bad_event_desc, "Invalid event description");
DEFSTRING(version_string, SAWMILL_VERSION);

DEFSYM(before_exit_hook, "before-exit-hook");

static rep_bool
on_idle (int since_last)
{
    return rep_FALSE;
}

static void
on_termination (void)
{
}

DEFUN_INT("quit", Fquit, Squit, (void), rep_Subr0, "") /*
::doc:Squit::
quit
::end:: */
{
    return Fthrow (Qquit, rep_MAKE_INT(0));
}

DEFUN_INT("restart", Frestart, Srestart, (void), rep_Subr0, "") /*
::doc:Srestart::
restart
::end:: */
{
    longjmp (clean_exit_jmp_buf, ec_restart);
}

static void
sawmill_symbols (void)
{
    rep_INTERN(sawmill_directory);
    if(getenv("SAWMILLDIR") != 0)
	Fset (Qsawmill_directory, rep_string_dup(getenv("SAWMILLDIR")));
    else
	Fset (Qsawmill_directory, rep_string_dup(SAWMILL_DIR));

    rep_INTERN(sawmill_lisp_lib_directory);
    if(getenv("SAWMILLLISPDIR") != 0)
    {
	Fset (Qsawmill_lisp_lib_directory,
	      rep_string_dup(getenv("SAWMILLLISPDIR")));
    }
    else
	Fset (Qsawmill_lisp_lib_directory, rep_string_dup(SAWMILL_LISPDIR));

    rep_INTERN(sawmill_site_lisp_directory);
    if(getenv("SAWMILLSITELISPDIR") != 0)
    {
	Fset (Qsawmill_site_lisp_directory,
	      rep_string_dup(getenv("SAWMILLSITELISPDIR")));
    }
    else
    {
	Fset (Qsawmill_site_lisp_directory,
	      rep_concat2(rep_STR(rep_SYM(Qsawmill_directory)->value),
			  "/site-lisp"));
    }

    rep_INTERN(sawmill_exec_directory);
    if(getenv("SAWMILLEXECDIR") != 0)
	Fset (Qsawmill_exec_directory, rep_string_dup(getenv("SAWMILLEXECDIR")));
    else
	Fset (Qsawmill_exec_directory, rep_string_dup(SAWMILL_EXECDIR));

    if(getenv("SAWMILLDOCFILE") != 0)
	Fset (Qdocumentation_file, rep_string_dup(getenv("SAWMILLDOCFILE")));
    else
    {
	Fset (Qdocumentation_file,
	      rep_concat2(rep_STR(rep_SYM(Qsawmill_directory)->value),
			  "/" SAWMILL_VERSION "/DOC"));
    }

    Fset (Qdocumentation_files, Fcons(rep_SYM(Qdocumentation_file)->value,
				      rep_SYM(Qdocumentation_files)->value));

    Fset (Qload_path, Fcons(rep_SYM(Qsawmill_lisp_lib_directory)->value,
			    Fcons(rep_SYM(Qsawmill_site_lisp_directory)->value,
				  rep_SYM(Qload_path)->value)));

    Fset (Qdl_load_path, Fcons(rep_SYM(Qsawmill_exec_directory)->value,
			       rep_SYM(Qdl_load_path)->value));

    rep_INTERN(sawmill_version);
    Fset (Qsawmill_version, rep_VAL(&version_string));

    rep_INTERN(window_error); rep_ERROR(window_error);
    rep_INTERN(invalid_pos); rep_ERROR(invalid_pos);
    rep_INTERN(bad_event_desc); rep_ERROR(bad_event_desc);

    rep_on_idle_fun = on_idle;
    rep_on_termination_fun = on_termination;

    /* This should stop us getting trapped in any recursive edits */
    Fset (Qerror_mode, Qtop_level);
    Fset (Qinterrupt_mode, Qtop_level);

    rep_ADD_SUBR_INT(Squit);
    rep_ADD_SUBR_INT(Srestart);

    rep_INTERN(before_exit_hook);
}    

int
main(int argc, char **argv)
{
    volatile int rc = 5;
    volatile char **old_argv = (volatile char **)argv;

    prog_name = *argv++; argc--;
    rep_init (prog_name, &argc, &argv, 0, 0);

    if (sys_init(prog_name))
    {
	repv res;
	volatile int exit_code;

	sawmill_symbols();

	/* call all init funcs... */
	events_init ();
	colors_init ();
	images_init ();
	fonts_init ();
	cursors_init ();
	frames_init ();
	windows_init ();
	commands_init ();
	keys_init ();
	functions_init ();

	if ((exit_code = setjmp (clean_exit_jmp_buf)) == 0)
	{
	    res = Fload(rep_string_dup ("sawmill"), Qnil, Qnil, Qnil);
	    if (res != rep_NULL)
	    {
		repv tv;
		rep_GC_root gc_tv;
		rc = 0;

		/* final initialisation.. */
		if(rep_SYM(Qbatch_mode)->value == Qnil)
		    manage_windows ();

		/* then jump into the event loop.. */
		if(rep_SYM(Qbatch_mode)->value == Qnil)
		    res = Frecursive_edit ();

		tv = rep_throw_value;
		rep_throw_value = rep_NULL;
		rep_PUSHGC(gc_tv, tv);
		Fcall_hook (Qbefore_exit_hook, Qnil, Qnil);
		rep_POPGC;
		rep_throw_value = tv;
	    }
	    else if(rep_throw_value && rep_CAR(rep_throw_value) == Qquit)
	    {
		if(rep_INTP(rep_CDR(rep_throw_value)))
		    rc = rep_INT(rep_CDR(rep_throw_value));
		else
		    rc = 0;
		rep_throw_value = 0;
	    }

	    if(rep_throw_value && rep_CAR(rep_throw_value) == Qerror)
	    {
		/* If quitting due to an error, print the error cell if
		   at all possible. */
		repv stream = Fstderr_file();
		repv old_tv = rep_throw_value;
		rep_GC_root gc_old_tv;
		rep_PUSHGC(gc_old_tv, old_tv);
		rep_throw_value = rep_NULL;
		if(stream && rep_FILEP(stream))
		{
		    fputs("error--> ", stderr);
		    Fprin1(rep_CDR(old_tv), stream);
		    fputc('\n', stderr);
		}
		else
		    fputs("sawmill: error in initialisation\n", stderr);
		rep_throw_value = old_tv;
		rep_POPGC;
	    }
	}

	/* call all exit funcs... */
	windows_kill ();
	frames_kill ();
	cursors_kill ();
	fonts_kill ();
	images_kill ();
	colors_kill ();
	events_kill ();

	sys_kill();
	rep_kill();

	if (exit_code == ec_restart)
	    execvp ((char *)(old_argv[0]), (char **)old_argv);
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
