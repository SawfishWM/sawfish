/* session.c -- session manager skeleton
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
#include <X11/SM/SMlib.h>

DEFSYM(save_session, "save-session");

static repv sm_client_id;		/* string or nil */

static IceConn ice_conn;
static SmcConn sm_conn;


/* Lisp functions */

DEFUN ("sm-client-id", Vsm_client_id, Ssm_client_id, (repv arg), rep_Var) /*
::doc:Vsm-client-id::
A string defining the current session client id, or nil if no connection
has been made with the session manager.
::end:: */
{
    if (arg && (rep_STRINGP(arg) || arg == Qnil))
	sm_client_id = arg;
    return sm_client_id;
}

DEFUN("sm-set-property", Fsm_set_property, Ssm_set_property,
      (repv prop, repv value), rep_Subr2) /*
::doc:Ssm-set-property::
sm-set-property PROPERTY-NAME VALUE

Sets the session manager property called PROPERTY-NAME (a string) to
VALUE. VALUE may be either an integer, a string, or a list of strings.
::end:: */
{
    SmProp sm_prop, *sm_ptr = &sm_prop;
    rep_DECLARE1 (prop, rep_STRINGP);

    if (sm_conn == 0)
	return Qnil;

    sm_prop.name = rep_STR(prop);
    if (rep_INTP(value))
    {
	u_char val = rep_INT(value);
	sm_prop.type = SmCARD8;
	sm_prop.num_vals = 1;
	sm_prop.vals = alloca (sizeof (SmPropValue));
	sm_prop.vals->length = 1;
	sm_prop.vals->value = &val;
    }
    else if (rep_STRINGP(value))
    {
	sm_prop.type = SmARRAY8;
	sm_prop.num_vals = 1;
	sm_prop.vals = alloca (sizeof (SmPropValue));
	sm_prop.vals->length = rep_STRING_LEN(value);
	sm_prop.vals->value = rep_STR(value);
    }
    else if (rep_CONSP(value))
    {
	repv len = Flength (value), tem;
	if (len && rep_INTP(len))
	{
	    int i;
	    sm_prop.type = SmLISTofARRAY8;
	    sm_prop.num_vals = rep_INT(len);
	    sm_prop.vals = alloca (sizeof (SmPropValue) * sm_prop.num_vals);
	    tem = value;
	    for (i = 0; i < sm_prop.num_vals; i++)
	    {
		if (!rep_STRINGP(rep_CAR(tem)))
		    return rep_signal_arg_error (value, 2);
		sm_prop.vals[i].length = rep_STRING_LEN(rep_CAR(tem));
		sm_prop.vals[i].value = rep_STR(rep_CAR(tem));
		tem = rep_CDR(tem);
	    }
	}
    }
    else
	return rep_signal_arg_error (value, 2);
    SmcSetProperties (sm_conn, 1, &sm_ptr);
    return Qt;
}

DEFUN("sm-delete-property", Fsm_delete_property, Ssm_delete_property,
      (repv prop), rep_Subr1) /*
::doc:Ssm-delete-property::
sm-delete-property PROPERTY-NAME

Deletes the session manager property called PROPERTY-NAME (a string).
::end:: */
{
    char *name;
    rep_DECLARE1(prop, rep_STRING);

    if (sm_conn == 0)
	return Qnil;

    name = rep_STR(prop);
    SmcDeleteProperties (sm_conn, 1, &name);
    return Qt;
}


/* SM callbacks */

static void
save_yourself_2 (SmcConn conn, SmPointer data)
{
    repv ret = rep_call_lisp1 (Qsave_session, sm_client_id);
    SmcSaveYourselfDone (conn, (ret && ret != Qnil) ? True : False);
}

static void
save_yourself (SmcConn conn, SmPointer data, int save_type, Bool shutdown,
	       int interact_style, Bool fast)
{
    SmcRequestSaveYourselfPhase2 (conn, &save_yourself_2, 0);
}

static void
die (SmcConn conn, SmPointer data)
{
    longjmp (clean_exit_jmp_buf, ec_session_died);
}

static void
save_complete (SmcConn conn, SmPointer data)
{
}

static void
shutdown_cancelled (SmcConn conn, SmPointer data)
{
}


/* ICE hooks */

static void
ICE_input_ready (int fd)
{
    IceProcessMessages (ice_conn, 0, 0);
}

static void
ICE_watch_callback (IceConn ice, IcePointer client_data,
		    Bool open, IcePointer *watch_data)
{
    if (open)
	rep_register_input_fd (IceConnectionNumber(ice), ICE_input_ready);
    else
	rep_deregister_input_fd (IceConnectionNumber(ice));
}


/* initialisation */

void
session_init (void)
{
    if (rep_SYM(Qbatch_mode)->value == Qnil
	&& !rep_get_option ("--sm-disable", 0)
	&& getenv ("SESSION_MANAGER") != 0)
    {
	SmcCallbacks call;
	char *ret_id;
	char err[256];

	IceAddConnectionWatch (ICE_watch_callback, 0);

	if (!rep_get_option ("--sm-client-id", &sm_client_id))
	    sm_client_id = Qnil;
	call.save_yourself.callback = save_yourself;
	call.die.callback = die;
	call.save_complete.callback = save_complete;
	call.shutdown_cancelled.callback = shutdown_cancelled;

	sm_conn = SmcOpenConnection (0, 0, 1, 0,
				     SmcSaveYourselfProcMask
				     | SmcDieProcMask
				     | SmcSaveCompleteProcMask
				     | SmcShutdownCancelledProcMask,
				     &call,
				     rep_STRINGP(sm_client_id)
				     ? rep_STR(sm_client_id) : 0,
				     &ret_id, sizeof (err), err);
	if (sm_conn != 0)
	{
	    sm_client_id = rep_string_dup (ret_id);
	    ice_conn = SmcGetIceConnection (sm_conn);
	}
	else
	{
	    sm_client_id = Qnil;
	    fprintf (stderr, "sm-open-connection: %s\n", err);
	}
    }
    else
	sm_client_id = Qnil;
    rep_ADD_SUBR(Ssm_client_id);
    rep_ADD_SUBR(Ssm_set_property);
    rep_ADD_SUBR(Ssm_delete_property);
    rep_INTERN(save_session);
    rep_mark_static (&sm_client_id);
}

void
session_kill (void)
{
    if (sm_conn != 0)
	SmcCloseConnection (sm_conn, 0, 0);
    sm_conn = 0;
    ice_conn = 0;
}
