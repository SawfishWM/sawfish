/* windows.c -- window manipulation
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
#include <X11/extensions/shape.h>

Lisp_Window *window_list;
int window_type;

Lisp_Window *focus_window;

int pending_destroys;

static bool initialising;

DEFSYM(add_window_hook, "add-window-hook");
DEFSYM(before_add_window_hook, "before-add-window-hook");
DEFSYM(place_window_hook, "place-window-hook");
DEFSYM(placed, "placed");
DEFSYM(after_framing_hook, "after-framing-hook");
DEFSYM(after_initialization_hook, "after-initialization-hook");

/* for visibility-notify-hook */
DEFSYM(fully_obscured, "fully-obscured");
DEFSYM(partially_obscured, "partially-obscured");
DEFSYM(unobscured, "unobscured");

/* for window-size-hints */
DEFSYM(min_width, "min-width");
DEFSYM(min_height, "min-height");
DEFSYM(max_width, "max-width");
DEFSYM(max_height, "max-height");
DEFSYM(width_inc, "width-inc");
DEFSYM(height_inc, "height-inc");
DEFSYM(base_width, "base-width");
DEFSYM(base_height, "base-height");
DEFSYM(min_aspect, "min-aspect");
DEFSYM(max_aspect, "max-aspect");
DEFSYM(user_size, "user-size");
DEFSYM(program_size, "program-size");
DEFSYM(user_position, "user-position");
DEFSYM(program_position, "program-position");
DEFSYM(window_gravity, "window-gravity");

/* for window-gravity */
DEFSYM(forget, "forget");
DEFSYM(static, "static");
DEFSYM(north_west, "north-west");
DEFSYM(north, "north");
DEFSYM(north_east, "north-east");
DEFSYM(west, "west");
DEFSYM(center, "center");
DEFSYM(east, "east");
DEFSYM(south_west, "south-west");
DEFSYM(south, "south");
DEFSYM(south_east, "south-east");

static repv gravity_map[StaticGravity+1];


/* utilities */

/* Returns true if we should manage window ID */
bool
mapped_not_override_p (Window id)
{
    XWindowAttributes wa;

    XGetWindowAttributes(dpy, id, &wa);
    return ((wa.map_state != IsUnmapped) && (wa.override_redirect != True));
}

/* Give the input focus to window W, or to no window if W is null */
void
focus_on_window (Lisp_Window *w)
{
    if (w != 0 && (w->id != 0 || w->frame != 0))
    {
	Window focus;
	DB(("focus_on_window (%s)\n", rep_STR(w->name)));
	if (!w->client_unmapped)
	{
	    if (w->does_wm_take_focus)
	    {
		DB(("  sending WM_TAKE_FOCUS message\n"));
		send_client_message (w->id, xa_wm_take_focus, last_event_time);
	    }
	    focus = w->id;
	}
	else
	    focus = w->frame;
	XSetInputFocus (dpy, focus, RevertToParent, last_event_time);
	focus_window = w;
    }
    else
    {
	DB(("focus_on_window (nil)\n"));
	XSetInputFocus (dpy, no_focus_window, RevertToNone, last_event_time);
	focus_window = 0;
    }
}

/* Set flags in W relating to which window manager protocols are recognised
   by window W. */
void
get_window_protocols (Lisp_Window *w)
{
    Atom *prot;
    u_int n;
    if (XGetWMProtocols (dpy, w->id, &prot, &n) != 0)
    {
	int i;
	for (i = 0; i < n; i++)
	{
	    if (prot[i] == xa_wm_take_focus)
	    {
		w->does_wm_take_focus = 1;
		DB(("  WM_TAKE_FOCUS is set\n"));
	    }
	    if (prot[i] == xa_wm_delete_window)
	    {
		w->does_wm_delete_window = 1;
		DB(("  WM_DELETE_WINDOW is set\n"));
	    }
	}
	XFree (prot);
    }
}

/* These two functions are used to bracket Xlib requests that would map,
   unmap, or reparent the client window. They ensure that any
   StructureNotifymask events generated between calling before_local_map ()
   and after_local_map () are discarded, but no others (so that we don't
   lose client-generated events) */

void
before_local_map (Lisp_Window *w)
{
    Fgrab_server ();
    XSelectInput (dpy, w->id, CLIENT_EVENTS & ~StructureNotifyMask);
}

void
after_local_map (Lisp_Window *w)
{
    XSelectInput (dpy, w->id, CLIENT_EVENTS);
    Fungrab_server ();
}


/* manipulating the Lisp window structures */

/* Return the window object containing ID, or a null pointer */
Lisp_Window *
find_window_by_id (Window id)
{
    Lisp_Window *w;
    w = window_list;
    while (w != 0 && w->id != id && w->frame != id)
	w = w->next;
    if (w != 0 && w->id == 0)
	w = 0;
    if (w != 0)
    {
	DB(("find_window_by_id (%lx) --> %s\n", id,
	    (w->name && rep_STRINGP(w->name))
	    ? (char *) rep_STR(w->name) : ""));
    }
    return w;
}

/* This is different to the above in that it could return a window
   that doesn't have a client window. */
Lisp_Window *
x_find_window_by_id (Window id)
{
    Lisp_Window *w;
    w = window_list;
    while (w != 0 && w->saved_id != id && w->frame != id)
	w = w->next;
    if (w != 0)
    {
	DB(("x_find_window_by_id (%lx) --> %s\n", id,
	    (w->name && rep_STRINGP(w->name))
	    ? (char *) rep_STR(w->name) : ""));
    }
    return w;
}

void
install_window_frame (Lisp_Window *w)
{
    DB(("install_window_frame (%s)\n", rep_STR(w->name)));
    if (!w->reparented && w->frame != 0)
    {
	XSelectInput (dpy, w->frame, FRAME_EVENTS);

	before_local_map (w);
	XReparentWindow (dpy, w->id, w->frame, -w->frame_x, -w->frame_y);
	w->reparented = TRUE;
	after_local_map (w);

	XAddToSaveSet (dpy, w->id);
	restack_frame_parts (w);
	reset_frame_parts (w);

	DB(("  reparented to %lx [%dx%d%+d%+d]\n",
	    w->frame, w->frame_width, w->frame_height,
	    w->frame_x, w->frame_y));
    }
}

void
remove_window_frame (Lisp_Window *w)
{
    DB(("remove_window_frame (%s)\n", rep_STR(w->name)));
    if (w->reparented)
    {
	/* reparent the subwindow back to the root window */

	before_local_map (w);
	XReparentWindow (dpy, w->id, root_window, w->attr.x, w->attr.y);
	w->reparented = FALSE;
	after_local_map (w);

	if (!w->mapped)
	    XRemoveFromSaveSet (dpy, w->id);
    }
}

/* Add the top-level window ID to the manager's data structures */
Lisp_Window *
add_window (Window id)
{
    char *tem;
    Lisp_Window *w = rep_ALLOC_CELL(sizeof (Lisp_Window));
    if (w != 0)
    {
	rep_GC_root gc_win;
	repv win = rep_VAL(w);
	XWindowChanges xwc;
	u_int xwcm;
	long supplied;

	DB(("add_window (%lx)\n", id));

	if (id == root_window)
	    DB(("  ** adding root window!?\n"));

	rep_data_after_gc += sizeof (Lisp_Window);
	memset (w, 0, sizeof (Lisp_Window));

	/* First initialise the Lisp stuff.. */
	w->next = window_list;
	window_list = w;
	w->car = window_type;
	w->id = id;
	w->saved_id = id;
	w->plist = Qnil;
	w->frame_style = Qnil;;

	/* ..now do the X11 stuff */

	XSelectInput (dpy, id, CLIENT_EVENTS);
	XGetWindowAttributes (dpy, id, &w->attr);
	DB(("  orig: width=%d height=%d x=%d y=%d\n",
	    w->attr.width, w->attr.height, w->attr.x, w->attr.y));

	if (XFetchName (dpy, id, &tem))
	{
	    w->name = rep_string_dup (tem);
	    XFree (tem);
	}
	else
	    w->name = rep_null_string ();
	w->full_name = w->name;
	if (XGetIconName (dpy, id, &tem))
	{
	    w->icon_name = rep_string_dup (tem);
	    XFree (tem);
	}
	else
	    w->icon_name = w->name;

	w->wmhints = XGetWMHints (dpy, id);
	if (!XGetWMNormalHints (dpy, w->id, &w->hints, &supplied))
	    w->hints.flags = 0;
	get_window_protocols (w);
	if (!XGetTransientForHint (dpy, w->id, &w->transient_for_hint))
	    w->transient_for_hint = 0;

	{
	    /* Is the window shaped? */
	    int xws, yws, xbs, ybs;
	    u_int wws, hws, wbs, hbs;
	    int bounding, clip;
	    XShapeSelectInput (dpy, w->id, ShapeNotifyMask);
	    XShapeQueryExtents (dpy, w->id, &bounding, &xws, &yws, &wws, &hws,
				&clip, &xbs, &ybs, &wbs, &hbs);
	    w->shaped = bounding ? 1 : 0;
	}

	DB(("  name=`%s' x=%d y=%d width=%d height=%d\n",
	    rep_STR(w->name), w->attr.x, w->attr.y,
	    w->attr.width, w->attr.height));

	xwcm = CWX | CWX | CWWidth | CWHeight | CWBorderWidth;
	xwc.x = w->attr.x;
	xwc.y = w->attr.y;
	xwc.width = w->attr.width;
	xwc.height = w->attr.height;
	xwc.border_width = 0;
	XConfigureWindow (dpy, id, xwcm, &xwc);

        w->visible = TRUE;
	w->mapped = TRUE;		/* only called from map request */

	/* ..then call the add-window-hook's.. */
	rep_PUSHGC(gc_win, win);
	Fcall_window_hook (Qbefore_add_window_hook, rep_VAL(w), Qnil, Qnil);
	Fcall_window_hook (Qadd_window_hook, rep_VAL(w), Qnil, Qnil);
	rep_POPGC;

	/* In case the window disappeared during the hook call */
	if (w->id != 0)
	{
	    Fgrab_server ();

	    /* this is where we create and reparent the window frame */
	    create_window_frame (w);
	    install_window_frame (w);

	    /* this grabs bound events in the subwindow */
	    grab_window_events (w, TRUE);

	    Fungrab_server ();
	}
	else
	    emit_pending_destroys ();

	if (w->id != 0 && !initialising)
	{
	    repv tem = Fwindow_get (rep_VAL(w), Qplaced);
	    if (tem && tem == Qnil)
	    {
		/* ..then the place-window-hook.. */
		rep_PUSHGC(gc_win, win);
		Fcall_window_hook (Qplace_window_hook, rep_VAL(w), Qnil, Qor);
		rep_POPGC;
	    }
	}
    }
    return w;
}

/* Remove W from the managed windows. If DESTROYED is nil, then the
   window will be reparented back to the root window */
void
remove_window (Lisp_Window *w, repv destroyed, repv from_error)
{
    DB(("remove_window (%s, %s)\n",
	rep_STR(w->name), destroyed == Qnil ? "nil" : "t"));

    if (focus_window == w)
	focus_window = 0;

    if (w->id != 0)
    {
	if (destroyed == Qnil && from_error == Qnil)
	{
	    grab_window_events (w, FALSE);
	    remove_window_frame (w);
	}

	if (from_error == Qnil)
	{
	    destroy_window_frame (w, FALSE);

	    if (focus_window == w)
		focus_on_window (0);
	}

	w->id = 0;
	pending_destroys++;

	/* gc will do the rest... */
    }
    else if (w->frame != 0 && from_error == Qnil)
	destroy_window_frame (w, FALSE);

    /* We can lose the focus sometimes, notably after a was-focused
       window is closed while a keyboard grab exists.. (netscape) */
    if (from_error == Qnil)
    {
	Window focus;
	int revert_to;
	XGetInputFocus (dpy, &focus, &revert_to);
	if (focus == None || focus == PointerRoot)
	    focus_on_window (focus_window);
    }
}

void
fix_window_size (Lisp_Window *w)
{
    Fgrab_server ();
    XResizeWindow (dpy, w->id, w->attr.width, w->attr.height);
    if (w->frame != 0 && w->rebuild_frame != 0)
	w->rebuild_frame (w);
    Fungrab_server ();
}

/* Call destroy-notify-hook on any newly-dead windows */
void
emit_pending_destroys (void)
{
    if (pending_destroys > 0)
    {
	Lisp_Window *w;
    again:
	for (w = window_list; w != 0 && !rep_INTERRUPTP; w = w->next)
	{
	    if (w->id == 0 && !w->destroyed)
	    {
		pending_destroys--;
		w->destroyed = 1;
		Fcall_window_hook (Qdestroy_notify_hook,
				   rep_VAL(w), Qnil, Qnil);

		if (focus_window == w)
		    focus_on_window (0);

		/* gc may have reordered the list, so we have to start
		   at the beginning again.. */
		goto again;
	    }
	}
    }
}


/* Lisp functions */

DEFUN("window-get", Fwindow_get, Swindow_get,
      (repv win, repv prop), rep_Subr2) /*
::doc::Swindow-get::
window-get WINDOW PROPERTY

Return the value of the property named PROPERTY (a symbol) of WINDOW.

Note that these are Lisp properties not X properties.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, XWINDOWP);
    plist = VWIN(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop)
	    return rep_CAR(rep_CDR(plist));
	plist = rep_CDR(rep_CDR(plist));
    }
    return Qnil;
}

DEFUN("window-put", Fwindow_put, Swindow_put,
      (repv win, repv prop, repv val), rep_Subr3) /*
::doc:Swindow-put::
window-put WINDOW PROPERTY VALUE

Set the value of the property named PROPERTY (a symbol) of WINDOW to VALUE.

Note that these are Lisp properties not X properties.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, XWINDOWP);
    plist = VWIN(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop)
	{
	    if (prop == Qkeymap && VWIN(win)->id)
	    {
		/* A bit of a hack */
		grab_keymap_events (VWIN(win)->id,
				    rep_CAR(rep_CDR(plist)), FALSE);
		grab_keymap_events (VWIN(win)->id, val, TRUE);
	    }
	    if (!rep_CONS_WRITABLE_P(rep_CDR(plist)))
	    {
		/* Can't write into a dumped cell; need to cons
		   onto the head. */
		break;
	    }
	    rep_CAR(rep_CDR(plist)) = val;
	    return val;
	}
	plist = rep_CDR(rep_CDR(plist));
    }
    plist = Fcons(prop, Fcons(val, VWIN(win)->plist));
    if (plist != rep_NULL)
	VWIN(win)->plist = plist;
    return val;
}

DEFUN("window-name", Fwindow_name, Swindow_name, (repv win), rep_Subr1) /*
::doc:Swindow-name::
window-name WINDOW

Return the name of window object WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->name;
}

DEFUN("window-full-name", Fwindow_full_name, Swindow_full_name,
      (repv win), rep_Subr1) /*
::doc:Swindow-full-name::
window-full-name WINDOW

Return the full name of window object WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->full_name;
}

DEFUN("window-icon-name", Fwindow_icon_name, Swindow_icon_name,
      (repv win), rep_Subr1) /*
::doc:Swindow-icon-name::
window-icon-name WINDOW

Return the name of window object WINDOW's icon.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->icon_name;
}

DEFUN("window-mapped-p", Fwindow_mapped_p, Swindow_mapped_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-mapped-p::
window-mapped-p WINDOW

Return t if the client window associated with object WINDOW is mapped.
(This doesn't necessarily mean that it is visible.)
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->mapped ? Qt : Qnil;
}

DEFUN("window-frame", Fwindow_frame, Swindow_frame, (repv win), rep_Subr1) /*
::doc:Swindow-frame::
window-frame WINDOW

Return the frame object associated with WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->frame_style;
}

DEFUN("set-window-frame", Fset_window_frame, Sset_window_frame,
      (repv win, repv frame), rep_Subr2) /*
::doc:Sset-window-frame::
set-window-frame WINDOW FRAME

Set the frame associated with the window object WINDOW to FRAME (a
list). If the window is mapped the old frame will be destroyed and a
new frame constructed as specified by FRAME.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    rep_DECLARE2(frame, rep_LISTP);
    Fgrab_server ();

    if (VWIN(win)->reparented)
	destroy_window_frame (VWIN(win), TRUE);

    VWIN(win)->frame_style = frame;

    if (VWIN(win)->reparented)
	create_window_frame (VWIN(win));

    Fungrab_server ();
    Fcall_window_hook (Qafter_framing_hook, win, Qnil, Qnil);
    return VWIN(win)->frame_style;
}

DEFUN("rebuild-frame", Frebuild_frame, Srebuild_frame, (repv win), rep_Subr1) /*
::doc:Srebuild-frame::
rebuild-frame WINDOW

Reinitialises and recalibrates the window frame of WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->frame != 0 && VWIN(win)->rebuild_frame != 0)
    {
	VWIN(win)->rebuild_frame (VWIN(win));
	refresh_frame_parts (VWIN(win));
	Fcall_window_hook (Qafter_framing_hook, win, Qnil, Qnil);
    }
    return win;
}

DEFUN("window-position", Fwindow_position, Swindow_position,
      (repv win), rep_Subr1) /*
::doc:Swindow-position::
window-position WINDOW

Return (X . Y) defining the current position of WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return Fcons (rep_MAKE_INT(VWIN(win)->attr.x),
		  rep_MAKE_INT(VWIN(win)->attr.y));
}

DEFUN("window-dimensions", Fwindow_dimensions, Swindow_dimensions,
      (repv win), rep_Subr1) /*
::doc:Swindow-dimensions::
window-dimensions WINDOW

Return (WIDTH . HEIGHT) defining the current dimensions of the client
window associated with WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return Fcons (rep_MAKE_INT(VWIN(win)->attr.width),
		  rep_MAKE_INT(VWIN(win)->attr.height));
}

DEFUN("window-frame-dimensions", Fwindow_frame_dimensions,
      Swindow_frame_dimensions, (repv win), rep_Subr1) /*
::doc:Swindow-frame-dimensions::
window-frame-dimensions WINDOW

Return (WIDTH . HEIGHT) defining the current dimensions of the frame
surrounding WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->reparented)
    {
	return Fcons (rep_MAKE_INT(VWIN(win)->frame_width),
		      rep_MAKE_INT(VWIN(win)->frame_height));
    }
    else
	return Fwindow_dimensions (win);
}

DEFUN("window-frame-offset", Fwindow_frame_offset,
      Swindow_frame_offset, (repv win), rep_Subr1) /*
::doc:Swindow-frame-offset::
window-frame-offset WINDOW

Return (X . Y) defining the offset from the origin of the client window
associated with WINDOW to its frame window.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return Fcons (rep_MAKE_INT(VWIN(win)->frame_x),
		  rep_MAKE_INT(VWIN(win)->frame_y));
}

DEFUN("windowp", Fwindowp, Swindowp, (repv win), rep_Subr1) /*
::doc:Swindowp::
windowp ARG

Return t if ARG is a window object.
::end:: */
{
    return WINDOWP(win) ? Qt : Qnil;
}

DEFUN("set-input-focus", Fset_input_focus, Sset_input_focus,
      (repv win), rep_Subr1) /*
::doc:Sset-input-focus::
set-input-focus WINDOW

Set the input focus to WINDOW. If WINDOW is nil, then no window will
have the focus.
::end:: */
{
    /* Always unfocus, this seems to help where we've just grabbed the
       keyboard (thus focus has been hijacked) before calling this
       function..!? */
    focus_on_window (0);
    if (win != Qnil)
    {
	rep_DECLARE1(win, WINDOWP);
	focus_on_window (VWIN(win));
    }
    return win;
}

DEFUN("input-focus", Finput_focus, Sinput_focus, (void), rep_Subr0) /*
::doc:Sinput-focus::
input-focus

Return the window object that has the input focus, or nil if none does.
::end:: */
{
    return (focus_window == 0) ? Qnil : rep_VAL(focus_window);
}

DEFUN("window-wants-input-p", Fwindow_wants_input_p, Swindow_wants_input_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-wants-input-p::
window-wants-input-p WINDOW

Return t if the client window associated with object WINDOW has hinted
that it would like to be given the input focus when applicable.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return (VWIN(win)->wmhints && VWIN(win)->wmhints->input) ? Qt : Qnil;
}

DEFUN("managed-windows", Fmanaged_windows, Smanaged_windows,
      (void), rep_Subr0) /*
::doc:Smanaged-windows::
managed-windows

Return a list of all known client window objects.
::end:: */
{
    repv list = Qnil;
    Lisp_Window *w = window_list;
    while (w != 0)
    {
	if (w->id != 0)
	    list = Fcons (rep_VAL(w), list);
	w = w->next;
    }
    return list;
}

DEFUN("get-window-by-id", Fget_window_by_id, Sget_window_by_id,
      (repv id), rep_Subr1) /*
::doc:Sget-window-by-id::
get-window-by-id ID

Return the window object associated with xid ID, or nil.
::end:: */
{
    Lisp_Window *w;
    rep_DECLARE1(id, rep_INTP);
    w = find_window_by_id (rep_INT(id));
    return w ? rep_VAL(w) : Qnil;
}

DEFUN("stacking-order", Fstacking_order, Sstacking_order, (void), rep_Subr0) /*
::doc:Sstacking-order::
stacking-order

Return a list of windows defining the current stacking order of all
client windows.
::end:: */
{
    Window root, parent, *children;
    u_int nchildren;
    if (XQueryTree (dpy, root_window, &root, &parent, &children, &nchildren))
    {
	int i;
	repv ret = Qnil;
	rep_GC_root gc_ret;
	for (i = 0; i < nchildren; i++)
	{
	    Lisp_Window *w = find_window_by_id (children[i]);
	    if (w != 0)
		ret = Fcons (rep_VAL(w), ret);
	}
	if (children != 0)
	    XFree (children);
	rep_PUSHGC(gc_ret, ret);
	emit_pending_destroys ();
	rep_POPGC;
	return ret;
    }
    else
	return Qnil;
}

DEFUN("window-visibility", Fwindow_visibility, Swindow_visibility,
      (repv win), rep_Subr1) /*
::doc:Swindow-visibility::
window-visibility WINDOW

Return a symbol defining the visibility of WINDOW. Possible returned
symbols are `fully-obscured', `partially-obscured' or `unobscured'.
::end:: */
{
    repv sym = Qnil;
    switch (VWIN(win)->frame_vis)
    {
    case VisibilityFullyObscured:
	sym = Qfully_obscured;
	break;

    case VisibilityPartiallyObscured:
	sym = Qpartially_obscured;
	break;

    case VisibilityUnobscured:
	sym = Qunobscured;
	break;
    }
    return sym;
}

DEFUN("window-transient-p", Fwindow_transient_p, Swindow_transient_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-transient-p::
window-transient-p WINDOW

Return non-nil if WINDOW is a transient window. The returned value will
then be the numeric id of its parent window.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return (VWIN(win)->transient_for_hint
	    ? rep_MAKE_INT(VWIN(win)->transient_for_hint) : Qnil);
}

DEFUN("window-shaped-p", Fwindow_shaped_p, Swindow_shaped_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-shaped-p::
window-shaped-p WINDOW

Return non-nil if WINDOW is shaped.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->shaped ? Qt : Qnil;
}

DEFUN("hide-window", Fhide_window, Shide_window, (repv win), rep_Subr1) /*
::doc:Shide-window::
hide-window WINDOW

Prevent WINDOW from being displayed. See `show-window'.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->visible)
    {
	if (VWIN(win)->mapped)
	{
	    if (VWIN(win)->frame)
		XUnmapWindow (dpy, VWIN(win)->frame);
	    if (!VWIN(win)->client_unmapped)
	    {
		before_local_map (VWIN(win));
		XUnmapWindow (dpy, VWIN(win)->id);
		VWIN(win)->client_unmapped = 1;
		after_local_map (VWIN(win));
	    }
	}
	VWIN(win)->visible = 0;
	reset_frame_parts (VWIN(win));
    }
    return win;
}

DEFUN("show-window", Fshow_window, Sshow_window, (repv win), rep_Subr1) /*
::doc:Sshow-window::
show-window WINDOW

Ensure that WINDOW (if it has been mapped) is visible. See `hide-window'.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (!VWIN(win)->visible)
    {
	if (VWIN(win)->mapped)
	{
	    if (VWIN(win)->client_unmapped && !VWIN(win)->client_hidden)
	    {
		before_local_map (VWIN(win));
		XMapWindow (dpy, VWIN(win)->id);
		VWIN(win)->client_unmapped = 0;
		after_local_map (VWIN(win));
	    }
	    if (VWIN(win)->frame)
		XMapWindow (dpy, VWIN(win)->frame);
	}
	VWIN(win)->visible = 1;
    }
    return win;
}

DEFUN("window-visible-p", Fwindow_visible_p, Swindow_visible_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-visible-p::
window-visible-p WINDOW

Return t if WINDOW is currently visible (i.e. not hidden, see `hide-window').
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->visible ? Qt : Qnil;
}

DEFUN("window-framed-p", Fwindow_framed_p, Swindow_framed_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-framed-p::
window-framed-p WINDOW

Return t if WINDOW has been reparented to a frame window.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->reparented ? Qt : Qnil;
}

DEFUN("window-id", Fwindow_id, Swindow_id, (repv win), rep_Subr1) /*
::doc:Swindow-id::
window-id WINDOW

Return the numeric id of the client window associated with object
WINDOW. Returns nil if the client window has been deleted.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->id ? rep_MAKE_INT (VWIN(win)->id) : Qnil;
}

DEFUN("window-group-id", Fwindow_group_id, Swindow_group_id,
      (repv win), rep_Subr1) /*
::doc:Swindow-group-id::
window-group-id WINDOW

Return the numeric id defining the leader of the group that WINDOW is a
member of, or nil if it is not a member of a group.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->wmhints == 0)
	return Qnil;
    return ((VWIN(win)->wmhints->flags & WindowGroupHint)
	    ? rep_MAKE_INT (VWIN(win)->wmhints->window_group)
	    : Qnil);
}

DEFUN("window-border-width", Fwindow_border_width, Swindow_border_width,
      (repv win), rep_Subr1) /*
::doc:Swindow-border-width::
window-border-width WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return rep_MAKE_INT(VWIN(win)->attr.border_width);
}

DEFUN("window-size-hints", Fwindow_size_hints, Swindow_size_hints,
      (repv win), rep_Subr1) /*
::doc:Swindow-size-hints::
window-size-hints WINDOW

Return an alist defining the size-hints specified by the client window
associated with WINDOW. Possible keys in the alist are `min-height',
`max-height', `min-width', `max-width', `height-inc', `width-inc',
`min-aspect', `max-aspect', `base-height', `base-width',
`user-position', `program-position', `user-size', `program-size',
`window-gravity', `border-size'.
::end:: */
{
    repv ret = Qnil;
    XSizeHints *hints;
    long flags;
    rep_DECLARE1(win, WINDOWP);

    hints = &VWIN(win)->hints;
    flags = hints->flags;

    if (flags & PMinSize)
    {
	ret = Fcons (Fcons (Qmin_width, rep_MAKE_INT(hints->min_width)),
		     Fcons (Fcons (Qmin_height,
				   rep_MAKE_INT(hints->min_height)), ret));
    }
    if (flags & PMaxSize)
    {
	ret = Fcons (Fcons (Qmax_width, rep_MAKE_INT(hints->max_width)),
		     Fcons (Fcons (Qmax_height,
				   rep_MAKE_INT(hints->max_height)), ret));
    }
    if (flags & PResizeInc)
    {
	ret = Fcons (Fcons (Qwidth_inc, rep_MAKE_INT(hints->width_inc)),
		     Fcons (Fcons (Qheight_inc,
				   rep_MAKE_INT(hints->height_inc)), ret));
    }
    if (flags & PBaseSize)
    {
	ret = Fcons (Fcons (Qbase_width, rep_MAKE_INT(hints->base_width)),
		     Fcons (Fcons (Qbase_height,
				   rep_MAKE_INT(hints->base_height)), ret));
    }
    if (flags & PAspect)
    {
	ret = Fcons (Fcons (Qmin_aspect,
			    Fcons (rep_MAKE_INT(hints->min_aspect.x),
				   rep_MAKE_INT(hints->min_aspect.y))),
		     Fcons (Fcons (Qmax_aspect,
				   Fcons (rep_MAKE_INT(hints->max_aspect.x),
					  rep_MAKE_INT(hints->max_aspect.y))),
			    ret));
    }
    if (flags & USPosition)
	ret = Fcons (Fcons (Quser_position, Qt), ret);
    else if (flags & PPosition)
	ret = Fcons (Fcons (Qprogram_position, Qt), ret);
    if (flags & USSize)
	ret = Fcons (Fcons (Quser_size, Qt), ret);
    else if (flags & PSize)
	ret = Fcons (Fcons (Qprogram_size, Qt), ret);
    if ((flags & PWinGravity)
	&& hints->win_gravity >= ForgetGravity
	&& hints->win_gravity <= StaticGravity)
    {
	ret = Fcons (Fcons (Qwindow_gravity,
			    gravity_map[hints->win_gravity]), ret);
    }
    return ret;
}

DEFUN("call-window-hook", Fcall_window_hook, Scall_window_hook,
      (repv hook, repv win, repv args, repv type), rep_Subr4) /*
::doc:Scall-window-hook::
call-window-hook HOOK WINDOW &optional ARGS HOOK-TYPE

Call HOOK for WINDOW with extra arguments ARGS. See `call-hook' for a
description of HOOK-TYPE.
::end:: */
{
    repv tem;
    rep_GC_root gc_hook, gc_args, gc_type;
    rep_DECLARE1(hook, rep_SYMBOLP);
    rep_DECLARE2(win, XWINDOWP);
    args = Fcons (win, args);
    rep_PUSHGC(gc_hook, hook);
    rep_PUSHGC(gc_args, args);
    rep_PUSHGC(gc_type, type);
    tem = Fwindow_get (win, hook);
    if (tem && tem != Qnil)
    {
	tem = Fcall_hook (tem, args, type);
	if (!tem || (type == Qand && tem == Qnil)
	    || (type == Qor && tem != Qnil))
	{
	    goto out;
	}
    }
    tem = Fcall_hook (hook, args, type);
out:
    rep_POPGC; rep_POPGC; rep_POPGC;
    return tem;
}


/* type hooks */

static int
window_cmp (repv w1, repv w2)
{
    return w1 != w2;
}

static void
window_prin (repv stream, repv win)
{
    char buf[128];
    sprintf (buf, "#<window %lx>", VWIN(win)->id);
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
window_mark (repv win)
{
    rep_MARKVAL(VWIN(win)->plist);
    rep_MARKVAL(VWIN(win)->frame_style);
    if (VWIN(win)->frame)
	mark_frame_parts (VWIN(win));
    rep_MARKVAL(VWIN(win)->name);
    rep_MARKVAL(VWIN(win)->full_name);
    rep_MARKVAL(VWIN(win)->icon_name);
}

static void
window_mark_type (void)
{
    Lisp_Window *w = window_list;
    while (w != 0)
    {
	if (w->id != 0 || !w->destroyed)
	    rep_MARKVAL(rep_VAL(w));
	w = w->next;
    }
}

static void
window_sweep (void)
{
    Lisp_Window **ptr = &window_list;
    while (*ptr != 0)
    {
	Lisp_Window *w = *ptr;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
	    destroy_window_frame (w, FALSE);
	    if (w->wmhints != 0)
		XFree (w->wmhints);
	    *ptr = w->next;
	    rep_FREE_CELL(w);
	}
	else
	{
	    ptr = &(w->next);
	    rep_GC_CLR_CELL(rep_VAL(w));
	}
    }
}


/* initialisation */

void
manage_windows (void)
{
    Window root, parent, *children;
    unsigned int nchildren, i;

    Fgrab_server ();
    XQueryTree (dpy, root_window, &root, &parent, &children, &nchildren);
    initialising = TRUE;
    for (i = 0; i < nchildren; i++)
    {
	if (mapped_not_override_p (children[i]))
	{
	    XEvent fake;
	    Lisp_Window *w;
	    fake.xmaprequest.window = children[i];
	    map_request (&fake);
	    w = find_window_by_id (children[i]);
	}
    }
    initialising = FALSE;
    if (nchildren > 0)
	XFree (children);
    Fungrab_server ();

    Fcall_hook (Qafter_initialization_hook, Qnil, Qnil);
}

void
windows_init (void)
{
    window_type = rep_register_new_type ("window", window_cmp, window_prin,
					 window_prin, window_sweep,
					 window_mark, window_mark_type,
					 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Swindow_get);
    rep_ADD_SUBR(Swindow_put);
    rep_ADD_SUBR(Swindow_name);
    rep_ADD_SUBR(Swindow_full_name);
    rep_ADD_SUBR(Swindow_icon_name);
    rep_ADD_SUBR(Swindow_mapped_p);
    rep_ADD_SUBR(Swindow_frame);
    rep_ADD_SUBR(Sset_window_frame);
    rep_ADD_SUBR(Srebuild_frame);
    rep_ADD_SUBR(Swindow_position);
    rep_ADD_SUBR(Swindow_dimensions);
    rep_ADD_SUBR(Swindow_frame_dimensions);
    rep_ADD_SUBR(Swindow_frame_offset);
    rep_ADD_SUBR(Swindowp);
    rep_ADD_SUBR(Sset_input_focus);
    rep_ADD_SUBR(Sinput_focus);
    rep_ADD_SUBR(Swindow_wants_input_p);
    rep_ADD_SUBR(Smanaged_windows);
    rep_ADD_SUBR(Sget_window_by_id);
    rep_ADD_SUBR(Sstacking_order);
    rep_ADD_SUBR(Swindow_visibility);
    rep_ADD_SUBR(Swindow_transient_p);
    rep_ADD_SUBR(Swindow_shaped_p);
    rep_ADD_SUBR(Shide_window);
    rep_ADD_SUBR(Sshow_window);
    rep_ADD_SUBR(Swindow_visible_p);
    rep_ADD_SUBR(Swindow_framed_p);
    rep_ADD_SUBR(Swindow_id);
    rep_ADD_SUBR(Swindow_group_id);
    rep_ADD_SUBR(Swindow_size_hints);
    rep_ADD_SUBR(Scall_window_hook);
    rep_ADD_SUBR(Swindow_border_width);

    rep_INTERN_SPECIAL(before_add_window_hook);
    rep_INTERN_SPECIAL(add_window_hook);
    rep_INTERN_SPECIAL(place_window_hook);
    rep_INTERN(placed);
    rep_INTERN_SPECIAL(after_framing_hook);
    rep_INTERN_SPECIAL(after_initialization_hook);

    rep_INTERN(fully_obscured);
    rep_INTERN(partially_obscured);
    rep_INTERN(unobscured);

    rep_INTERN(min_width);
    rep_INTERN(min_height);
    rep_INTERN(max_width);
    rep_INTERN(max_height);
    rep_INTERN(width_inc);
    rep_INTERN(height_inc);
    rep_INTERN(base_width);
    rep_INTERN(base_height);
    rep_INTERN(min_aspect);
    rep_INTERN(max_aspect);
    rep_INTERN(user_size);
    rep_INTERN(user_position);
    rep_INTERN(program_size);
    rep_INTERN(program_position);
    rep_INTERN(window_gravity);
    rep_INTERN(forget);
    rep_INTERN(static);
    rep_INTERN(north_west);
    rep_INTERN(north);
    rep_INTERN(north_east);
    rep_INTERN(west);
    rep_INTERN(center);
    rep_INTERN(east);
    rep_INTERN(south_west);
    rep_INTERN(south);
    rep_INTERN(south_east);

    gravity_map[ForgetGravity] = Qforget;
    gravity_map[NorthWestGravity] = Qnorth_west;
    gravity_map[NorthGravity] = Qnorth;
    gravity_map[NorthEastGravity] = Qnorth_east;
    gravity_map[WestGravity] = Qwest;
    gravity_map[CenterGravity] = Qcenter;
    gravity_map[EastGravity] = Qeast;
    gravity_map[SouthWestGravity] = Qsouth_west;
    gravity_map[SouthGravity] = Qsouth;
    gravity_map[SouthEastGravity] = Qsouth_east;
    gravity_map[StaticGravity] = Qstatic;
}

void
windows_kill (void)
{
    Lisp_Window *w = window_list;
    while (w != 0)
    {
	remove_window (w, Qnil, Qnil);
	w = w->next;
    }
}
