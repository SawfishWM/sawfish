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

static bool initialising;

DEFSYM(add_window_hook, "add-window-hook");
DEFSYM(place_window_hook, "place-window-hook");

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
DEFSYM(window_state_change_hook, "window-state-change-hook");
DEFSYM(iconified, "iconified");


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
    if (w != 0)
    {
	DB(("focus_on_window (%s)\n", w->name));
	if (w->does_wm_take_focus)
	{
	    DB(("  sending WM_TAKE_FOCUS message\n"));
	    send_client_message (w->id, xa_wm_take_focus, last_event_time);
	}
	DB(("  XSetInputFocus (%lx, RevertToParent, %ld)\n", w->id,
	    last_event_time));
	XSetInputFocus (dpy, w->id, RevertToParent, last_event_time);
    }
    else
    {
	DB(("focus_on_window (nil)\n"));
	DB(("  XSetInputFocus (None, RevertToParent, %ld)\n",
	    last_event_time));
	XSetInputFocus (dpy, None, RevertToParent, last_event_time);
    }
}

/* Set flags in W relating to which window manager protocols are recognised
   by window W. */
static void
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


/* manipulating the Lisp window structures */

/* Return the window object containing ID, or a null pointer */
Lisp_Window *
find_window_by_id (Window id)
{
    Lisp_Window *w;
    DB(("find_window_by_id (%lx)\n", id));
    w = window_list;
    while (w != 0 && w->id != id && w->frame != id)
	w = w->next;
    if (w != 0 && w->id == 0)
	w = 0;
    DB(("  --> %p (%s)\n", w, (w != 0) ? w->name : ""));
    return w;
}

/* Note that frames.c:list_frame_generator also does this itself */
void
set_window_shape (Lisp_Window *w)
{
    if (w->frame)
    {
	if (w->shaped)
	{
	    XRectangle rect;
	    rect.x = -w->frame_x;
	    rect.y = -w->frame_y;
	    rect.width = w->attr.width;
	    rect.height = w->attr.height;
	    XShapeCombineRectangles (dpy, w->frame, ShapeBounding, 0, 0,
				     &rect, 1, ShapeSubtract, Unsorted);
	    XShapeCombineShape (dpy, w->frame, ShapeBounding,
				-w->frame_x, -w->frame_y, w->id,
				ShapeBounding, ShapeUnion);
	}
	else
	{
	    XRectangle rect;
	    rect.x = -w->frame_x;
	    rect.y = -w->frame_y;
	    rect.width = w->frame_width;
	    rect.height = w->frame_height;
	    XShapeCombineRectangles (dpy, w->frame, ShapeBounding, 0, 0,
				     &rect, 1, ShapeUnion, Unsorted);
	}
    }
}

void
install_window_frame (Lisp_Window *w)
{
    DB(("install_window_frame (%s)\n", w->name));
    if (!w->reparented)
    {
	XSelectInput (dpy, w->frame,
		      ButtonPressMask | ButtonReleaseMask
		      | KeyPressMask | ButtonMotionMask | PointerMotionHintMask
		      | EnterWindowMask | LeaveWindowMask
		      | ExposureMask | FocusChangeMask
		      | SubstructureRedirectMask);

	XReparentWindow (dpy, w->id, w->frame, -w->frame_x, -w->frame_y);
	w->reparented = TRUE;
	w->reparenting++;
	DB(("  reparented to %lx [%dx%d%+d%+d]\n",
	    w->frame, w->frame_width, w->frame_height,
	    w->frame_x, w->frame_y));
    }
}

void
remove_window_frame (Lisp_Window *w)
{
    DB(("remove_window_frame (%s)\n", w->name));
    if (w->reparented)
    {
	/* reparent the subwindow back to the root window */
	XReparentWindow (dpy, w->id, root_window, w->attr.x, w->attr.y);
	w->reparented = FALSE;
	w->reparenting++;
    }
}

/* Add the top-level window ID to the manager's data structures */
Lisp_Window *
add_window (Window id)
{
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

	memset (w, 0, sizeof (Lisp_Window));

	/* First initialise the Lisp stuff.. */
	w->next = window_list;
	window_list = w;
	w->car = window_type;
	w->id = id;
	w->plist = Qnil;
	w->frame_style = Qnil;;

	/* ..now do the X11 stuff */

	XSelectInput (dpy, id, PropertyChangeMask | StructureNotifyMask
		      | ColormapChangeMask | VisibilityChangeMask);
	XGetWindowAttributes (dpy, id, &w->attr);
	DB(("  orig: width=%d height=%d x=%d y=%d\n",
	    w->attr.width, w->attr.height, w->attr.x, w->attr.y));
	XFetchName (dpy, id, &w->name);
#if 0
	XGetClassHint (dpy, id, &w->class);
#endif
	w->wmhints = XGetWMHints (dpy, id);
	if (!XGetWMNormalHints (dpy, w->id, &w->hints, &supplied))
	    w->hints.flags = 0;
	get_window_protocols (w);

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

	if (w->name == 0)
	    w->name = "";
	w->full_name = w->name;
	if (XGetIconName (dpy, id, &w->icon_name) == 0)
	    w->icon_name = w->name;

	DB(("  name=`%s' x=%d y=%d width=%d height=%d\n",
	    w->name, w->attr.x, w->attr.y, w->attr.width, w->attr.height));

	xwcm = CWX | CWX | CWWidth | CWHeight | CWBorderWidth;
	xwc.x = w->attr.x + w->attr.border_width;
	xwc.y = w->attr.y + w->attr.border_width;
	xwc.width = w->attr.width;
	xwc.height = w->attr.height;
	xwc.border_width = 0;
	XConfigureWindow (dpy, id, xwcm, &xwc);

        w->visible = TRUE;

	/* window managers are supposed to set WM_STATE to diffentiate
	   top-level windows of applications from others */
	Fset_client_state (rep_VAL(w));

	/* ..then call the add-window-hook.. */
	rep_PUSHGC(gc_win, win);
	Fcall_window_hook (Qadd_window_hook, rep_VAL(w), Qnil, Qnil);
	rep_POPGC;

	/* In case the window disappeared during the hook call */
	if (w->id != 0)
	{
	    Fgrab_server ();
	    XAddToSaveSet (dpy, id);

	    /* this is where we create and reparent the window frame */
	    create_window_frame (w);
	    install_window_frame (w);

	    /* this grabs bound events in the subwindow */
	    grab_window_events (w, TRUE);

	    Fungrab_server ();
	}

	if (w->id != 0 && !initialising)
	{
	    /* ..then the place-window-hook.. */
	    rep_PUSHGC(gc_win, win);
	    Fcall_window_hook (Qplace_window_hook, rep_VAL(w), Qnil, Qor);
	    rep_POPGC;
	}
    }
    return w;
}

/* Remove W from the managed windows. If DESTROYED is nil, then the
   window will be reparented back to the root window */
void
remove_window (Lisp_Window *w, repv destroyed)
{
    DB(("remove_window (%s, %s)\n", w->name, destroyed == Qnil ? "nil" : "t"));
    if (w->id != 0)
    {
	if (destroyed == Qnil)
	{
	    remove_window_frame (w);
	    XRemoveFromSaveSet (dpy, w->id);
	}
	destroy_window_frame (w);
	w->id = 0;
	/* gc will do the rest... */
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


/* Lisp functions */

DEFUN("window-get", Fwindow_get, Swindow_get,
      (repv win, repv prop), rep_Subr2) /*
::doc::Swindow-get::
window-get WINDOW PROPERTY
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
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return rep_string_dup (VWIN(win)->name);
}

DEFUN("window-full-name", Fwindow_full_name, Swindow_full_name,
      (repv win), rep_Subr1) /*
::doc:Swindow-full-name::
window-full-name WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return rep_string_dup (VWIN(win)->full_name);
}

DEFUN("window-icon-name", Fwindow_icon_name, Swindow_icon_name,
      (repv win), rep_Subr1) /*
::doc:Swindow-icon-name::
window-icon-name WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return rep_string_dup (VWIN(win)->icon_name);
}

DEFUN("window-mapped-p", Fwindow_mapped_p, Swindow_mapped_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-mapped-p::
window-mapped-p WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->mapped ? Qt : Qnil;
}

DEFUN("window-frame", Fwindow_frame, Swindow_frame, (repv win), rep_Subr1) /*
::doc:Swindow-frame::
window-frame WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->frame_style;
}

DEFUN("set-window-frame", Fset_window_frame, Sset_window_frame,
      (repv win, repv frame), rep_Subr2) /*
::doc:Sset-window-frame::
window-frame WINDOW FRAME
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    Fgrab_server ();
    if (VWIN(win)->frame != 0)
    {
	remove_window_frame (VWIN(win));
	destroy_window_frame (VWIN(win));
    }
    VWIN(win)->frame_style = frame;

    if (VWIN(win)->mapped)
    {
	/* The `w->reparenting' flag is a _bit_ and thus can only handle
	   one outstanding ReparentWindow request. So flush and handle
	   the events generated by the above call to remove_window_frame ()
	   before calling install_window_frame () below */
	XSync (dpy, False);
	handle_input_mask (SubstructureNotifyMask);

	/* Just in case the window disappeared.. */
	if (VWIN(win)->id != 0)
	{
	    create_window_frame (VWIN(win));
	    install_window_frame (VWIN(win));
	    if (VWIN(win)->visible)
		XMapWindow (dpy, VWIN(win)->frame);
	}
    }
    Fungrab_server ();
    
    return VWIN(win)->frame_style;
}

DEFUN("window-position", Fwindow_position, Swindow_position,
      (repv win), rep_Subr1) /*
::doc:Swindow-position::
window-position WINDOW
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
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return Fcons (rep_MAKE_INT(VWIN(win)->frame_width),
		  rep_MAKE_INT(VWIN(win)->frame_height));
}

DEFUN("window-frame-offset", Fwindow_frame_offset,
      Swindow_frame_offset, (repv win), rep_Subr1) /*
::doc:Swindow-frame-offset::
window-frame-offset WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return Fcons (rep_MAKE_INT(VWIN(win)->frame_x),
		  rep_MAKE_INT(VWIN(win)->frame_y));
}

DEFUN("windowp", Fwindowp, Swindowp, (repv win), rep_Subr1) /*
::doc:Swindowp::
windowp ARG
::end:: */
{
    return WINDOWP(win) ? Qt : Qnil;
}

DEFUN("set-input-focus", Fset_input_focus, Sset_input_focus,
      (repv win), rep_Subr1) /*
::doc:Sset-input-focus::
set-input-focus WINDOW-OR-NIL
::end:: */
{
    if (win == Qnil)
	focus_on_window (0);
    else
    {
	rep_DECLARE1(win, WINDOWP);
	focus_on_window (VWIN(win));
    }
    return win;
}

DEFUN("input-focus", Finput_focus, Sinput_focus, (void), rep_Subr0) /*
::doc:Sinput-focus::
input-focus
::end:: */
{
    return (focus_window == 0) ? Qnil : rep_VAL(focus_window);
}

DEFUN("managed-windows", Fmanaged_windows, Smanaged_windows,
      (void), rep_Subr0) /*
::doc:Smanaged-windows::
managed-windows
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

DEFUN("window-visibility", Fwindow_visibility, Swindow_visibility,
      (repv win), rep_Subr1) /*
::doc:Swindow-visibility::
window-visibility WINDOW
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
::end:: */
{
    Window tem;
    rep_DECLARE1(win, WINDOWP);
    return (XGetTransientForHint (dpy, VWIN(win)->id, &tem)
	    ? rep_MAKE_INT(tem) : Qnil);
}

DEFUN("window-shaped-p", Fwindow_shaped_p, Swindow_shaped_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-shaped-p::
window-shaped-p WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->shaped ? Qt : Qnil;
}

DEFUN("hide-window", Fhide_window, Shide_window, (repv win), rep_Subr1) /*
::doc:Shide-window::
hide-window WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->visible)
    {
	if (VWIN(win)->visible && VWIN(win)->mapped)
	    XUnmapWindow (dpy, VWIN(win)->frame);
	VWIN(win)->visible = FALSE;
    }
    return win;
}

DEFUN("show-window", Fshow_window, Sshow_window, (repv win), rep_Subr1) /*
::doc:Sshow-window::
show-window WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (!VWIN(win)->visible)
    {
	if (!VWIN(win)->visible && VWIN(win)->mapped)
	    XMapWindow (dpy, VWIN(win)->frame);
	VWIN(win)->visible = TRUE;
    }
    return win;
}

DEFUN("window-visible-p", Fwindow_visible_p, Swindow_visible_p,
      (repv win), rep_Subr1) /*
::doc:Swindow-visible-p::
window-visible-p WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->visible ? Qt : Qnil;
}

DEFUN("window-id", Fwindow_id, Swindow_id, (repv win), rep_Subr1) /*
::doc:Swindow-id::
window-id WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return rep_MAKE_INT (VWIN(win)->id);
}

DEFUN("window-group-id", Fwindow_group_id, Swindow_group_id,
      (repv win), rep_Subr1) /*
::doc:Swindow-group-id::
window-group-id WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return ((VWIN(win)->wmhints->flags & WindowGroupHint)
	    ? rep_MAKE_INT (VWIN(win)->wmhints->window_group)
	    : Qnil);
}

DEFUN("window-size-hints", Fwindow_size_hints, Swindow_size_hints,
      (repv win), rep_Subr1) /*
::doc:Swindow-size-hints::
window-size-hints WINDOW
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

    return ret;
}

DEFUN("call-window-hook", Fcall_window_hook, Scall_window_hook,
      (repv hook, repv win, repv args, repv type), rep_Subr4) /*
::doc:Scall-window-hook::
call-window-hook HOOK WINDOW &optional ARGS HOOK-TYPE
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

DEFUN("set-client-state", Fset_client_state, Sset_client_state,
      (repv win), rep_Subr1)
{
    u_long data = NormalState;
    repv tem;
    rep_DECLARE1(win, WINDOWP);
    tem = Fwindow_get (win, Qiconified);
    if (tem != Qnil)
	data = IconicState;
    XChangeProperty (dpy, VWIN(win)->id, xa_wm_state, xa_wm_state, 32,
		     PropModeReplace, (u_char *)&data, 1);
    return win;
}


/* type hooks */

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
}

static void
window_mark_type (void)
{
    Lisp_Window *w = window_list;
    while (w != 0)
    {
	if (w->id != 0)
	    rep_MARKVAL(rep_VAL(w));
	w = w->next;
    }
}

static void
window_sweep (void)
{
    Lisp_Window *w = window_list;
    window_list = 0;
    while (w != 0)
    {
	Lisp_Window *next = w->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	    rep_FREE_CELL(w);
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = window_list;
	    window_list = w;
	}
	w = next;
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
}

void
windows_init (void)
{
    window_type = rep_register_new_type ("window", 0, window_prin, window_prin,
					 window_sweep, window_mark,
					 window_mark_type, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Swindow_get);
    rep_ADD_SUBR(Swindow_put);
    rep_ADD_SUBR(Swindow_name);
    rep_ADD_SUBR(Swindow_full_name);
    rep_ADD_SUBR(Swindow_icon_name);
    rep_ADD_SUBR(Swindow_mapped_p);
    rep_ADD_SUBR(Swindow_frame);
    rep_ADD_SUBR(Sset_window_frame);
    rep_ADD_SUBR(Swindow_position);
    rep_ADD_SUBR(Swindow_dimensions);
    rep_ADD_SUBR(Swindow_frame_dimensions);
    rep_ADD_SUBR(Swindowp);
    rep_ADD_SUBR(Sset_input_focus);
    rep_ADD_SUBR(Sinput_focus);
    rep_ADD_SUBR(Smanaged_windows);
    rep_ADD_SUBR(Swindow_visibility);
    rep_ADD_SUBR(Swindow_transient_p);
    rep_ADD_SUBR(Swindow_shaped_p);
    rep_ADD_SUBR(Shide_window);
    rep_ADD_SUBR(Sshow_window);
    rep_ADD_SUBR(Swindow_visible_p);
    rep_ADD_SUBR(Swindow_id);
    rep_ADD_SUBR(Swindow_group_id);
    rep_ADD_SUBR(Swindow_size_hints);
    rep_ADD_SUBR(Scall_window_hook);
    rep_ADD_SUBR(Sset_client_state);
    rep_INTERN(add_window_hook);
    rep_INTERN(place_window_hook);

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
    rep_INTERN(window_state_change_hook);
    rep_INTERN(iconified);

    add_hook (Qwindow_state_change_hook, rep_VAL(&Sset_client_state));
}

void
windows_kill (void)
{
    Lisp_Window *w = window_list;
    while (w != 0)
    {
	remove_window (w, Qnil);
	w = w->next;
    }
}
