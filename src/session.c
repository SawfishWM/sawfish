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

#ifdef HAVE_X11_SM_SMLIB_H
# include <X11/SM/SMlib.h>
#endif

DEFSYM(sm_save_yourself, "sm-save-yourself");

#ifdef HAVE_X11_SM_SMLIB_H
static IceConn ice_conn;
static SmcConn sm_conn;
#endif


/* Lisp functions */

DEFUN("sm-set-property", Fsm_set_property, Ssm_set_property,
      (repv prop, repv value), rep_Subr2) /*
::doc:sawfish.wm.session.util#sm-set-property::
sm-set-property PROPERTY-NAME VALUE

Sets the session manager property called PROPERTY-NAME (a string) to
VALUE. VALUE may be either an integer, a string, or a list of strings.
::end:: */
{
#ifdef HAVE_X11_SM_SMLIB_H
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
#endif
    return Qt;
}

DEFUN("sm-delete-property", Fsm_delete_property, Ssm_delete_property,
      (repv prop), rep_Subr1) /*
::doc:sawfish.wm.session.util#sm-delete-property::
sm-delete-property PROPERTY-NAME

Deletes the session manager property called PROPERTY-NAME (a string).
::end:: */
{
#ifdef HAVE_X11_SM_SMLIB_H
    char *name;
    rep_DECLARE1(prop, rep_STRING);

    if (sm_conn == 0)
	return Qnil;

    name = rep_STR(prop);
    SmcDeleteProperties (sm_conn, 1, &name);
#endif
    return Qt;
}


/* SM callbacks */

#ifdef HAVE_X11_SM_SMLIB_H

static bool outstanding_save_done;

static void
save_yourself_2 (SmcConn conn, SmPointer data)
{
    repv ret = rep_call_lisp0 (global_symbol_value (Qsm_save_yourself));
    SmcSaveYourselfDone (conn, (ret && ret != Qnil) ? True : False);
}

static void
save_yourself (SmcConn conn, SmPointer data, int save_type, Bool shutdown,
	       int interact_style, Bool fast)
{
    if (save_type == SmSaveLocal || save_type == SmSaveBoth)
    {
	outstanding_save_done = TRUE;
	SmcRequestSaveYourselfPhase2 (conn, &save_yourself_2, 0);
    }
    else
	SmcSaveYourselfDone (conn, True);
}

static void
die__ (void)
{
    exit_code = ec_session_died;
    Fquit ();
}

static void
die (SmcConn conn, SmPointer data)
{
    die__ ();
}

static void
save_complete (SmcConn conn, SmPointer data)
{
}

static void
shutdown_cancelled (SmcConn conn, SmPointer data)
{
    if (outstanding_save_done)
    {
	SmcSaveYourselfDone (conn, True);
	outstanding_save_done = FALSE;
    }
}

#endif /* HAVE_X11_SM_SMLIB_H */


/* ICE hooks */

#ifdef HAVE_X11_SM_SMLIB_H

static void
ICE_input_ready (int fd)
{
    if (IceProcessMessages (ice_conn, 0, 0) != IceProcessMessagesSuccess)
    {
	fputs ("ICE has broken?\n", stderr);
	die__ ();
    }
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

#endif /* HAVE_X11_SM_SMLIB_H */


/* initialisation */

DEFUN("sm-connect", Fsm_connect, Ssm_connect, (repv id), rep_Subr1)
{
#ifdef HAVE_X11_SM_SMLIB_H
    SmcCallbacks call;
    char *ret_id;
    char err[256];

    IceAddConnectionWatch (ICE_watch_callback, 0);

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
				 rep_STRINGP(id) ? rep_STR(id) : 0,
				 &ret_id, sizeof (err), err);
    if (sm_conn != 0)
    {
	ice_conn = SmcGetIceConnection (sm_conn);
	return rep_string_dup (ret_id);
    }
    else
    {
	return Fsignal (Qerror,
			rep_list_2 (rep_string_dup ("sm-open-connection"),
				    rep_string_dup (err)));
    }
#else
    return Qnil;
#endif
}

DEFUN("sm-disconnect", Fsm_disconnect, Ssm_disconnect, (void), rep_Subr0)
{
#ifdef HAVE_X11_SM_SMLIB_H
    if (sm_conn != 0)
	SmcCloseConnection (sm_conn, 0, 0);
    sm_conn = 0;
    ice_conn = 0;
#endif
    return Qt;
}

void
session_init (void)
{
    repv tem = rep_push_structure ("sawfish.wm.session.util");
    rep_ADD_SUBR(Ssm_set_property);
    rep_ADD_SUBR(Ssm_delete_property);
    rep_ADD_SUBR(Ssm_connect);
    rep_ADD_SUBR(Ssm_disconnect);
    rep_INTERN(sm_save_yourself);
    rep_pop_structure (tem);
}

void
session_kill (void)
{
    Fsm_disconnect ();
}
