/* main.c -- Entry point for sawfish
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
#include "build.h"
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <X11/Xlocale.h>		/* for setlocale () */
#ifdef HAVE_GDK_PIXBUF
#include <glib-object.h>
#endif

/* one of the ec_ values */
int exit_code = ec_no_exit;

DEFSYM(sawfish_directory, "sawfish-directory");
DEFSYM(sawfish_lisp_lib_directory, "sawfish-lisp-lib-directory");
DEFSYM(sawfish_site_lisp_directory, "sawfish-site-lisp-directory");
DEFSYM(sawfish_exec_directory, "sawfish-exec-directory");
DEFSYM(sawfish_locale_directory, "sawfish-locale-directory");
DEFSYM(sawfish_user_lisp_directory, "sawfish-user-lisp-directory");
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
::doc:sawfish-locale-directory::
The name of the directory containing sawfish's message catalog files.
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
DEFSYM(fonts_are_fontsets, "fonts-are-fontsets");
DEFSYM(sawfish_wm, "sawfish.wm");

static rep_bool
on_idle (int since_last)
{
    if (print_event_prefix ())
	return rep_TRUE;

    if (since_last == 0 && rep_recurse_depth == 0)
    {
	/* XXX There have been reports of sawfish locking the display,
	   XXX I've never seen it, but this may help, and shouldn't hurt.. */
	last_event_time = get_server_timestamp ();
	ungrab_pointer ();
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
::doc:sawfish.wm.misc#quit::
quit

Terminate the sawfish process.
::end:: */
{
    if (exit_code == ec_no_exit)
	exit_code = ec_exit;
    return Fthrow (Qquit, rep_MAKE_INT(0));
}

DEFUN_INT("restart", Frestart, Srestart, (void), rep_Subr0, "") /*
::doc:sawfish.wm.misc#restart::
restart

Restart the sawfish process.
::end:: */
{
    exit_code = ec_restart;
    return Fquit ();
}

DEFUN ("exit-type", Fexit_type, Sexit_type, (void), rep_Subr0)
{
    DEFSTRING (s_user_quit, "user-quit");
    DEFSTRING (s_user_restart, "user-restart");
    DEFSTRING (s_session_quit, "session-quit");

    switch (exit_code)
    {
    case ec_exit:
	return Fintern (rep_VAL (&s_user_quit), Qnil);

    case ec_restart:
	return Fintern (rep_VAL (&s_user_restart), Qnil);

    case ec_session_died:
	return Fintern (rep_VAL (&s_session_quit), Qnil);

    default:
	return Qnil;
    }
}

static repv split_by_colon(char * str){
  // Split string at colon, and return lisp list of the strings.
  repv * elm;
  repv val;
  int length, i, from, elm_ind;

  length = 1;
  i = 0;
  // first, scan how many colons are there
  while (str[i] != 0){
    if(str[i] == ':'){
      length++;
    }
    i++;
  }
  // well, sawfish doesn't check malloc failure...
  elm = (repv *) malloc(sizeof(repv) * length);

  from = 0; i = 0; elm_ind = 0;
  while(1){
    if((str[i] == ':') || (str[i] == 0)){
      if(from < i){
	// Add it only when non-zero
	elm[elm_ind] = rep_string_dupn(str + from, i - from);
	elm_ind++;
      }
      from = i + 1;
    }
    if(str[i] == 0){
      break;
    }
    i++;
  }

  // Flist isn't exported...
  val = Qnil;
  for(elm_ind--; elm_ind >= 0; elm_ind--){
    val = Fcons(elm[elm_ind], val);
  }
  free(elm);
  return val;
}

static void
sawfish_symbols (void)
{
    repv tem, tem2;

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

    rep_INTERN_SPECIAL(sawfish_locale_directory);
    Fset (Qsawfish_locale_directory, rep_string_dup(SAWFISH_LOCALEDIR));

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

    rep_INTERN_SPECIAL(sawfish_user_lisp_directory);

    if(getenv("SAWFISH_USER_LISP_DIR") != 0){
      Fset(Qsawfish_user_lisp_directory,
	   split_by_colon(getenv("SAWFISH_USER_LISP_DIR")));
    }
    else{
      Fset(Qsawfish_user_lisp_directory,
	   split_by_colon("~/.sawfish/lisp"));
    }

    /* in lisp,
       (setq load-path
             (append sawfish-user-lisp-directory
                     (list sawfish-lisp-lib-directory
		           sawfish-site-lisp-directory)
		     load-path)) */

    /* rep_copy_list is not exported...
       tem =
         rep_copy_list(Fsymbol_value(Qsawfish_user_lisp_directory, Qt));
    */

    if(getenv("SAWFISH_USER_LISP_DIR") != 0){
      tem = split_by_colon(getenv("SAWFISH_USER_LISP_DIR"));
    }
    else{
      tem = split_by_colon("~/.sawfish/lisp");
    }

    // tem2 will point to load-path
    if(tem != Qnil){
      tem2 = tem;

      while(rep_CDR(tem) != Qnil){
	tem = rep_CDR(tem);
      }
      rep_CDR(tem) = Fcons(Fsymbol_value(Qsawfish_lisp_lib_directory, Qt),
			   Qnil);
      tem = rep_CDR(tem);
    }else{
      tem = Fcons(Fsymbol_value(Qsawfish_lisp_lib_directory, Qt),
		  Qnil);
      tem2 = tem;
    }
    rep_CDR(tem) = Fcons(Fsymbol_value(Qsawfish_site_lisp_directory, Qt),
			 Qnil);
    tem = rep_CDR(tem);
    rep_CDR(tem) = Fsymbol_value(Qload_path, Qt);
    Fset(Qload_path, tem2);

    Fset (Qdl_load_path, Fcons (Fsymbol_value (Qsawfish_exec_directory, Qt),
				Fsymbol_value (Qdl_load_path, Qt)));

    rep_INTERN_SPECIAL(sawfish_version);
    Fset (Qsawfish_version, rep_VAL(&version_string));

    rep_INTERN(window_error); rep_ERROR(window_error);
    rep_INTERN(invalid_pos); rep_ERROR(invalid_pos);
    rep_INTERN(bad_event_desc); rep_ERROR(bad_event_desc);
    rep_INTERN(sawfish_wm);

    rep_on_idle_fun = on_idle;
    rep_on_termination_fun = on_termination;

    /* This should stop us getting trapped in any recursive edits */
    Fset (Qerror_mode, Qtop_level);
    Fset (Qinterrupt_mode, Qtop_level);

    rep_INTERN_SPECIAL(fonts_are_fontsets);
    Fset (Qfonts_are_fontsets, Qt);

    tem = rep_push_structure ("sawfish.wm.misc");
    rep_ADD_SUBR_INT(Squit);
    rep_ADD_SUBR_INT(Srestart);
    rep_ADD_SUBR(Sexit_type);
    rep_pop_structure (tem);
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

static repv
inner_main (repv arg)
{
    repv res = rep_load_environment (Qnil);
    if (res != rep_NULL)
    {
	/* C modules that have also Lisp code in the filing system. */
	static const char *init[] = {
	    "sawfish.wm.misc",
	    "sawfish.wm.cursors",
	    "sawfish.wm",
	    0
	};
	const char **ptr;

	for (ptr = init; res != rep_NULL && *ptr != 0; ptr++)
	{
	    res = rep_bootstrap_structure (*ptr);
	}
    }

    if (res != rep_NULL && !batch_mode_p ())
    {
	/* final initialisation.. */
	manage_windows ();

	/* then jump into the event loop.. */
	res = rep_top_level_recursive_edit ();
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
    char **old_argv;
    int old_argc;
    char *lang;
    char *prog_name;

    /* This will fork multiple copies, if necessary */
    multihead_init (&argc, &argv);
    old_argv = argv;
    old_argc = argc;

#ifdef HAVE_GDK_PIXBUF
    g_type_init();
#endif

    prog_name = *argv++; argc--;
    lang = setlocale(LC_ALL, "");
    rep_init (prog_name, &argc, &argv, 0, 0);
    stash_argv (old_argc, old_argv);

    if (rep_get_option ("--version", 0))
    {
	printf ("sawfish version %s\n", SAWFISH_VERSION);
	return 0;
    }

    if (rep_get_option ("--help", 0))
    {
	printf ("\
usage: %s [OPTIONS...]\n\
\n\
where OPTIONS are any of:\n\
\n\
    --display=DPY	Connect to X display DPY\n\
    --multihead		Fork a copy of sawfish for each screen\n\
    --visual=VISUAL	Preferred visual type\n\
    --depth=DEPTH	Preferred color depth\n\
    --disable-nls	Disable internationalization of messages\n\
\n\
    FILE		load the Lisp file FILE (from the cwd if possible,\n\
			 implies --batch mode)\n\
\n\
    --batch		batch mode: process options and exit\n\
    --interp		interpreted mode: don't load compiled Lisp files\n\
\n\
    --call FUNCTION	call the Lisp function FUNCTION\n\
    --f FUNCTION\n\
\n\
    --load FILE		load the file of Lisp forms called FILE\n\
    -l FILE\n\
\n\
    --5-buttons		ignore buttons 6 - 8 to enable kbd layout switching\n\
    --version		print version details\n\
    --no-rc		don't load rc or site-init files\n\
    --quit, -q		terminate the interpreter process\n", prog_name);
	return 0;
   }

#if defined (DEBUG) && DEBUG == 1
   setvbuf (stdout, NULL, _IOLBF, BUFSIZ);
#endif

    rep_push_structure ("sawfish.wm");

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

repv
module_symbol_value (repv mod, repv sym)
{
    repv value;
    repv tem = rep_push_structure_name (mod);
    value = Fsymbol_value (sym, Qt);
    rep_pop_structure (tem);
    return value;
}

/* in rep 0.11 and earlier the gaol was broken, it was possible to read
   non-exported special variables; this was fixed in rep 0.12, so we
   need to read some symbols from a non-gaolled module */
repv
global_symbol_value (repv sym)
{
    return module_symbol_value (Qsawfish_wm, sym);
}
