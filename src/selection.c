/* selection.c -- selection handling (only retrieving)
   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   Originally adapted from the Jade sources by Mark Probst.

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
#include <X11/Xatom.h>


/* selection functions */

static Atom sawmill_selection;

static inline Atom
symbol_to_atom (repv sym)
{
    return XInternAtom (dpy, rep_STR (rep_SYM (sym)->name), False);
}

DEFUN ("x-selection-active-p", Fx_selection_active_p,
       Sx_selection_active_p, (repv sel), rep_Subr1) /*
::doc:sawfish.wm.util.selection#x-selection-active-p::
x-selection-active-p SELECTION

Returns t if the X11 selection defined by the symbol SELECTION is
available for reading.
::end:: */
{
    Atom selection;
    rep_DECLARE1 (sel, rep_SYMBOLP);
    selection = symbol_to_atom (sel);
    return (XGetSelectionOwner (dpy, selection) != None) ? Qt : Qnil;
}

static Bool
selnotify_pred (Display *dpy, XEvent *ev, XPointer arg)
{
    return ev->type == SelectionNotify;
}

DEFUN ("x-get-selection", Fx_get_selection,
       Sx_get_selection, (repv sel), rep_Subr1) /*
::doc:sawfish.wm.util.selection#x-get-selection::
x-get-selection SELECTION

Returns the string corresponding to the current value of the X11
selection defined by the symbol SELECTION.

If the selection currently has no value, nil is returned.
::end:: */
{
    repv res = Qnil;
    Atom selection;
    Window owner;
    rep_DECLARE1 (sel, rep_SYMBOLP);
    selection = symbol_to_atom (sel);
    owner = XGetSelectionOwner (dpy, selection);
    if (owner != None)
    {
	XEvent ev;
	Window sel_window = no_focus_window;
	XConvertSelection (dpy, selection, XA_STRING,
			   sawmill_selection, sel_window, last_event_time);
	XIfEvent (dpy, &ev, selnotify_pred, (XPointer) 0);
	if (ev.xselection.property != None)
	{
	    /* First find the size of the property. */
	    Atom actual_type;
	    int actual_format;
	    unsigned long nitems, bytes_after;
	    unsigned char *prop;          
	    int r;
	    int offset;
	    r = XGetWindowProperty (dpy, sel_window, sawmill_selection,
				    0, 0, False, AnyPropertyType,
				    &actual_type, &actual_format,
				    &nitems, &bytes_after, &prop);
	    if (r != Success)
		return Qnil;
	    XFree (prop);
	    if (actual_type == None || actual_format != 8)
		return Qnil;
	    res = rep_make_string (bytes_after + 1);
	    if (!res)
		return rep_mem_error ();
	    offset = 0;
	    while (bytes_after > 0)
	    {
		r = XGetWindowProperty (dpy, sel_window, sawmill_selection,
					offset/4, (bytes_after / 4) + 1,
					False, AnyPropertyType,
					&actual_type, &actual_format,
					&nitems, &bytes_after, &prop);
		if (r != Success)
		    return Qnil;
		memcpy (rep_STR(res) + offset, prop, nitems);
		XFree (prop);
		offset += nitems;
	    }
	    XDeleteProperty (dpy, sel_window, sawmill_selection);
	    rep_STR(res)[offset] = 0;
	}
    }
    return res;
}


/* dl hooks */

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("sawfish.wm.util.selection");
    /* ::alias:selection sawfish.wm.util.selection:: */
    rep_alias_structure ("selection");
    rep_ADD_SUBR (Sx_selection_active_p);
    rep_ADD_SUBR (Sx_get_selection);

    if (dpy != 0)
	sawmill_selection = XInternAtom (dpy, "SAWMILL_SELECTION", False);

    return rep_pop_structure (tem);
}
