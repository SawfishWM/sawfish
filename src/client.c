/* client.c -- plugin for sawfish communication

   $Id$

   Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>

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
#include <stdlib.h>
#include <rep.h>

static repv display;

DEFUN ("%sawfish-client-eval", F_sawfish_client_eval,
       S_sawfish_client_eval, (repv form, repv async), rep_Subr2)
{
    DEFSTRING (fmt, "%S");
    char *result;
    int length;

    form = Fformat (rep_list_3 (Qnil, rep_VAL (&fmt), form));
    if (form == rep_NULL)
	return rep_NULL;

    if (client_open (rep_STR (display)) != 0)
    {
	DEFSTRING (foo, "can't connect to sawfish on display");
	return Fsignal (Qerror, rep_list_2 (rep_VAL (&foo), display));
    }

    result = client_eval (rep_STR (form), async != Qnil, &length);

    client_close ();

    if (result != 0)
	return rep_string_dupn (result, length);
    else if (async == Qnil)
    {
	DEFSTRING (foo, "while communicating with sawfish");
	return Fsignal (Qerror, rep_LIST_1 (rep_VAL (&foo)));
    }
    else
	return Qnil;
}

DEFUN ("sawfish-client-eval", Fsawfish_client_eval,
       Ssawfish_client_eval, (repv form, repv no_read), rep_Subr2)
{
    repv out = F_sawfish_client_eval (form, Qnil);

    if (no_read == Qnil && out != rep_NULL)
	out = Fread (Fmake_string_input_stream (out, Qnil));

    return out;
}

DEFUN ("sawfish-client-eval-async", Fsawfish_client_eval_async,
       Ssawfish_client_eval_async, (repv form), rep_Subr1)
{
    return F_sawfish_client_eval (form, Qt);
}

DEFUN ("sawfish-client-display", Fsawfish_client_display,
       Ssawfish_client_display, (repv arg), rep_Subr1)
{
    repv old = display;
    if (rep_STRINGP (arg))
	display = arg;
    return old;
}

repv
rep_dl_init (void)
{
    repv tem;
    char *dpy = getenv ("DISPLAY");
    if (dpy == 0)
	dpy = ":0";
    display = rep_string_dup (dpy);
    rep_mark_static (&display);

    tem = rep_push_structure ("sawfish.client");
    rep_ADD_INTERNAL_SUBR (S_sawfish_client_eval);
    rep_ADD_SUBR (Ssawfish_client_eval);
    rep_ADD_SUBR (Ssawfish_client_eval_async);
    rep_ADD_SUBR (Ssawfish_client_display);
    return rep_pop_structure (tem);
}
