/* events.c -- Event handling
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

/* Lookup table of event handlers */
void (*event_handlers[LASTEvent])(XEvent *ev);

/* Map events to their names for debugging */
static char *event_names[LASTEvent];

/* Map events to the mask selecting them */
static long event_masks[LASTEvent];

/* Most recent known mouse position relative to the root window */
static int current_mouse_x, current_mouse_y;

/* ..And the position before the most recently known */
static int previous_mouse_x, previous_mouse_y;

/* Most recently seen server timestamp */
Time last_event_time;

/* Current XEvent or a null pointer */
XEvent *current_x_event;
static bool current_event_updated_mouse;

/* We need a ButtonRelease on this fp. */
struct frame_part *clicked_frame_part;

DEFSYM(visibility_notify_hook, "visibility-notify-hook");
DEFSYM(destroy_notify_hook, "destroy-notify-hook");
DEFSYM(map_notify_hook, "map-notify-hook");
DEFSYM(unmap_notify_hook, "unmap-notify-hook");
DEFSYM(reparent_notify_hook, "reparent-notify-hook");
DEFSYM(property_notify_hook, "property-notify-hook");
DEFSYM(enter_notify_hook, "enter-notify-hook");
DEFSYM(leave_notify_hook, "leave-notify-hook");
DEFSYM(focus_in_hook, "focus-in-hook");
DEFSYM(focus_out_hook, "focus-out-hook");
DEFSYM(iconify_window, "iconify-window");
DEFSYM(uniconify_window, "uniconify-window");
DEFSYM(client_message_hook, "client-message-hook");

/* for enter/leave-notify-hook */
DEFSYM(root, "root");

/* for property-notify-hook */
DEFSYM(new_value, "new-value");
DEFSYM(deleted, "deleted");

DEFSYM(raise_window, "raise-window");
DEFSYM(lower_window, "lower-window");

/* Where possible record the timestamp from event EV */
static void
record_event_time (XEvent *ev)
{
    switch (ev->type)
    {
    case KeyPress:
    case KeyRelease:
	last_event_time = ev->xkey.time;
	break;

    case ButtonPress:
    case ButtonRelease:
	last_event_time = ev->xbutton.time;
	break;

    case MotionNotify:
	last_event_time = ev->xmotion.time;
	break;

    case EnterNotify:
    case LeaveNotify:
	last_event_time = ev->xcrossing.time;
	break;

    case PropertyNotify:
	last_event_time = ev->xproperty.time;
	break;
    }
}

static void
record_mouse_position (int x, int y)
{
    previous_mouse_x = current_mouse_x;
    previous_mouse_y = current_mouse_y;
    current_mouse_x = x;
    current_mouse_y = y;
    current_event_updated_mouse = TRUE;
}


/* Individual event handlers */

static void
visibility_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xvisibility.window);
    if (w != 0)
    {
	repv vis;
	w->frame_vis = ev->xvisibility.state;
	vis = Fwindow_visibility (rep_VAL(w));
	Fcall_window_hook (Qvisibility_notify_hook,
			   rep_VAL(w), Fcons(vis, Qnil), Qnil);
    }
}

static void
colormap_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xcolormap.window);
    if (w != 0 && ev->xcolormap.window == w->id)
    {
	XWindowAttributes attr;
	XGetWindowAttributes (dpy, w->id, &attr);
	w->attr.colormap = attr.colormap;
	/* Is it worth copying any other fields..?
	   Note that we _musn't_ copy the x or y values */

	if (w == focus_window)
	    XInstallColormap (dpy, w->attr.colormap);
    }
}

static void
key_press (XEvent *ev)
{
    record_mouse_position (ev->xkey.x_root, ev->xkey.y_root);

    /* Don't look for a context map, frame parts are never focused */
    eval_input_event (Qnil);

    XAllowEvents (dpy, AsyncKeyboard, last_event_time);
}

static void
handle_fp_click (struct frame_part *fp, XEvent *ev)
{
    int old_clicked = fp->clicked;
    if (ev->type == ButtonPress)
    {
	fp->clicked = 1;
	if (clicked_frame_part != 0)
	    unclick_current_fp ();
	clicked_frame_part = fp;
    }
    else if (ev->type == ButtonRelease)
    {
	fp->clicked = 0;
	clicked_frame_part = 0;
    }
    if (fp->clicked != old_clicked)
	refresh_frame_part (fp);
}

void
unclick_current_fp (void)
{
    if (clicked_frame_part != 0)
    {
	if (clicked_frame_part->clicked)
	{
	    clicked_frame_part->clicked = 0;
	    refresh_frame_part (clicked_frame_part);
	}
	clicked_frame_part = 0;
    }
}

static void
button_press (XEvent *ev)
{
    struct frame_part *fp;
    Lisp_Window *w = 0;
    repv context_map = Qnil;

    record_mouse_position (ev->xbutton.x_root, ev->xbutton.y_root);

    fp = find_frame_part_by_window (ev->xbutton.window);
    if (fp != 0)
    {
	w = fp->win;

	if (ev->type == ButtonPress)
	    handle_fp_click (fp, ev);

	if (fp->clicked)
	    context_map = get_keymap_for_frame_part (fp);
    }

    eval_input_event (context_map);

    if (fp != 0 && w->id != 0 && ev->type == ButtonRelease)
    {
	/* In case the event binding threw a non-local-exit, fake
	   an unwind-protect thing */
	repv old_throw = rep_throw_value;
	rep_GC_root gc_old_throw;
	rep_throw_value = rep_NULL;
	rep_PUSHGC(gc_old_throw, old_throw);
	handle_fp_click (fp, ev);
	rep_POPGC;
	rep_throw_value = old_throw;
    }

    XAllowEvents (dpy, AsyncPointer, last_event_time);
}

static void
motion_notify (XEvent *ev)
{
    struct frame_part *fp;
    repv context_map = Qnil;

    Window tmpw;
    int tmp;
    int x, y;
    
    /* Swallow any pending motion events as well. */
    while(XCheckMaskEvent(dpy, ButtonMotionMask, ev))
	;
	    
    /* It seems that further MotionNotify events are suspended
       until the pointer's position has been queried. I should
       check the Xlib manuals about this. */
    if(XQueryPointer(dpy, ev->xmotion.window,
		     &tmpw, &tmpw, &x, &y, &tmp, &tmp, &tmp))
    {
	record_mouse_position (x, y);
    }

    fp = find_frame_part_by_window (ev->xmotion.window);
    if (fp != 0)
	context_map = get_keymap_for_frame_part (fp);

    eval_input_event (context_map);
}

static void
property_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xproperty.window);
    if (w != 0 && ev->xproperty.window == w->id)
    {
	switch (ev->xproperty.atom)
	{
	    u_char *prop;
	    Atom actual;
	    int format;
	    long nitems, bytes_after;
	    XSizeHints hints;

	case XA_WM_NAME:
	case XA_WM_ICON_NAME:
	    if (ev->xproperty.state == PropertyNewValue
		&& XGetWindowProperty (dpy, w->id, ev->xproperty.atom,
				       0, 200, False, XA_STRING, &actual,
				       &format, &nitems,
				       &bytes_after, &prop) == Success
		&& actual != None)
	    {
		if (format == 8 && w->id != 0)
		{
		    if (ev->xproperty.atom == XA_WM_NAME)
			w->full_name = w->name = rep_string_dup (prop);
		    else
			w->icon_name = rep_string_dup (prop);
		}
		XFree (prop);
	    }
	    break;

	case XA_WM_HINTS:
	    if (w->wmhints != 0)
		XFree (w->wmhints);
	    w->wmhints = XGetWMHints (dpy, w->id);
	    break;

	case XA_WM_NORMAL_HINTS:
	    XGetNormalHints (dpy, w->id, &hints);
	    break;
	}
	if (w->reparented && w->property_change != 0 && w->id != 0)
	    w->property_change (w);

	Fcall_window_hook (Qproperty_notify_hook, rep_VAL(w),
			   rep_list_2 (x_atom_symbol (ev->xproperty.atom),
				       ev->xproperty.state == PropertyNewValue
				       ? Qnew_value : Qdeleted), Qnil);
    }
}

static void
client_message (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xclient.window);
    repv type, data, args;
    type = x_atom_symbol (ev->xclient.message_type);
    switch (ev->xclient.format)
    {
	int i;

    case 8:
	data = rep_string_dupn (ev->xclient.data.b, 20);
	break;

    case 16:
	data = Fmake_vector (rep_MAKE_INT(10), Qnil);
	for (i = 0; i < 10; i++)
	    rep_VECTI(data,i) = rep_MAKE_INT (ev->xclient.data.s[i]);
	break;

    case 32:
	data = Fmake_vector (rep_MAKE_INT(5), Qnil);
	for (i = 0; i < 5; i++)
	    rep_VECTI(data,i) = rep_MAKE_INT (ev->xclient.data.l[i]);
	break;

    default:
	data = Qnil;
    }
    args = rep_list_2 (type, data);

    if (w != 0)
	Fcall_window_hook (Qclient_message_hook, rep_VAL(w), args, Qor);
    else
    {
	Fcall_hook (Qclient_message_hook,
		    Fcons ((ev->xclient.window == root_window)
			   ? Qroot : rep_MAKE_INT(ev->xclient.window), args),
		    Qor);
    }
}

static void
expose (XEvent *ev)
{
    struct frame_part *fp = find_frame_part_by_window (ev->xexpose.window);
    if (fp != 0)
	frame_part_exposer (&ev->xexpose, fp);
    else if (message_win != 0 && ev->xexpose.window == message_win)
	refresh_message_window ();
}

static void
destroy_notify (XEvent *ev)
{
    Lisp_Window *w = x_find_window_by_id (ev->xdestroywindow.window);
    if (w == 0 || ev->xdestroywindow.window != w->saved_id)
	return;
    if (w == focus_window)
	focus_window = 0;
    remove_window (w, Qt, Qnil);
    emit_pending_destroys ();
}

void
map_request (XEvent *ev)
{
    Window id = ev->xmaprequest.window;
    Lisp_Window *w;
    w = find_window_by_id (id);
    if (w == 0)
    {
	w = add_window (id);
	if (w == 0)
	    return;

	XMapWindow (dpy, w->id);
	w->mapped = TRUE;

	if (w->wmhints && w->wmhints->flags & StateHint
	    && w->wmhints->initial_state == IconicState)
	{
	    rep_call_lisp1 (Qiconify_window, rep_VAL(w));
	}

	if (w->visible)
	    XMapWindow (dpy, w->frame);
    }
    else
    {
	if (!w->reparented)
	{
	    Fgrab_server ();
	    create_window_frame (w);
	    install_window_frame (w);
	    Fungrab_server ();
	}
	XMapWindow (dpy, w->id);
	w->mapped = TRUE;
	rep_call_lisp1 (Quniconify_window, rep_VAL(w));
	if (w->id != 0 && w->visible)
	    XMapWindow (dpy, w->frame);
    }
}

static void
reparent_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xreparent.window);
    if (w != 0 && ev->xreparent.window == w->id
	&& ev->xreparent.event == w->id)
    {
	w->reparenting = FALSE;
	Fcall_window_hook (Qreparent_notify_hook, rep_VAL(w), Qnil, Qnil);
    }
}

static void
map_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xmap.window);
    if (w != 0 && ev->xmap.window == w->id && ev->xmap.event == w->id)
    {
	w->mapped = TRUE;
	if (!w->reparenting && w->frame != 0)
	{
	    install_window_frame (w);
	    if (w->visible)
		XMapWindow (dpy, w->frame);
	    Fcall_window_hook (Qmap_notify_hook, rep_VAL(w), Qnil, Qnil);
	}
    }
}

static void
unmap_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xunmap.window);
    if (w != 0 && ev->xunmap.window == w->id && ev->xunmap.event == w->id)
    {
	if (!w->reparenting && w->frame != 0)
	{
	    w->mapped = FALSE;
	    if (w->visible)
	    {
		XUnmapWindow (dpy, w->frame);
		reset_frame_parts (w);
	    }
	    /* Removing the frame reparents the client window back to
	       the root. This means that we receive the next MapRequest
	       for the window. */
	    remove_window_frame (w);
	    Fcall_window_hook (Qunmap_notify_hook, rep_VAL(w), Qnil, Qnil);

	    if (focus_window == w)
		focus_on_window (0);
	}
    }
}

static void
enter_notify (XEvent *ev)
{
    struct frame_part *fp;
    if (ev->xcrossing.window == root_window)
	Fcall_hook (Qenter_notify_hook, Fcons (Qroot, Qnil), Qnil);
    else if ((fp = find_frame_part_by_window (ev->xcrossing.window)) != 0)
    {
	bool refresh = FALSE;
	if (!fp->highlighted && !frame_state_mutex)
	{
	    fp->highlighted = 1;
	    refresh = TRUE;
	}
	if (clicked_frame_part == fp && !frame_state_mutex)
	{
	    fp->clicked = 1;
	    refresh = TRUE;
	}
	if (refresh)
	    refresh_frame_part (fp);
    }
    else
    {
	Lisp_Window *w = find_window_by_id (ev->xcrossing.window);
	if (w != 0 && w->mapped && w->visible)
	    Fcall_window_hook (Qenter_notify_hook, rep_VAL(w), Qnil, Qnil);
    }
}

static void
leave_notify (XEvent *ev)
{
    struct frame_part *fp;
    if (ev->xcrossing.window == root_window)
	Fcall_hook (Qleave_notify_hook, Fcons (Qroot, Qnil), Qnil);
    else if ((fp = find_frame_part_by_window (ev->xcrossing.window)) != 0)
    {
	bool refresh = FALSE;
	if (fp->highlighted && !frame_state_mutex)
	{
	    fp->highlighted = 0;
	    refresh = TRUE;
	}
	if (clicked_frame_part == fp && !frame_state_mutex)
	{
	    fp->clicked = 0;
	    refresh = TRUE;
	}
	if (refresh)
	    refresh_frame_part (fp);
    }
    else
    {
	Lisp_Window *w = find_window_by_id (ev->xcrossing.window);
	if (w != 0 && w->mapped && w->visible
	    && ev->xcrossing.detail != NotifyInferior)
	{
	    Fcall_window_hook (Qleave_notify_hook, rep_VAL(w), Qnil, Qnil);
	}
    }
}

static void
focus_in (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xfocus.window);
    if (w != 0 && w->visible)
    {
	XInstallColormap (dpy, w->attr.colormap);
	focus_window = w;
	if (w->focus_change != 0)
	{
	    DB (("  calling focus change %p on %s\n",
		 w->focus_change, rep_STR(w->name)));
	    w->focus_change (w);
	}
	if (w->id != 0)
	    Fcall_window_hook (Qfocus_in_hook, rep_VAL(w), Qnil, Qnil);
    }
}

static void
focus_out (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xfocus.window);
    if (w != 0 && ev->xfocus.detail != NotifyInferior)
    {
	XUninstallColormap (dpy, w->attr.colormap);
	if (focus_window == w)
	    focus_window = 0;
	if (w->focus_change != 0)
	{
	    DB (("  calling focus change %p on %s\n",
		 w->focus_change, rep_STR(w->name)));
	    w->focus_change (w);
	}
	if (w->id != 0)
	    Fcall_window_hook (Qfocus_out_hook, rep_VAL(w), Qnil, Qnil);
    }
}

static void
configure_request (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xconfigurerequest.window);
    if (w == 0)
    {
	XWindowChanges xwc;
	u_int xwcm = (ev->xconfigurerequest.value_mask & 
		      (CWX | CWY | CWWidth | CWHeight));
	xwc.x = ev->xconfigurerequest.x;
	xwc.y = ev->xconfigurerequest.y;
	xwc.width = ev->xconfigurerequest.width;
	xwc.height = ev->xconfigurerequest.height;
	XConfigureWindow (dpy, ev->xconfigurerequest.window, xwcm, &xwc);
	if (ev->xconfigurerequest.value_mask & CWStackMode)
	{
	    if (ev->xconfigurerequest.detail == Above)
		XRaiseWindow (dpy, ev->xconfigurerequest.window);
	    else if (ev->xconfigurerequest.detail == Below)
		XLowerWindow (dpy, ev->xconfigurerequest.window);
	}
    }
    else if (w != 0)
    {
	u_long mask = ev->xconfigurerequest.value_mask;
	bool need_move = FALSE, need_resize = FALSE;
	if (mask & CWStackMode)
	{
	    if (ev->xconfigurerequest.detail == Above)
	    {
		rep_GC_root gc_win;
		repv win = rep_VAL(w);
		rep_PUSHGC(gc_win, win);
		/* The GNOME pager seems to believe that asking for
		   a window to be raised to the top of the stack will
		   uniconify it. The Xlib manual suggests that the
		   correct method is to just remap the window.. */
#ifndef GNOME_PAGER_UNICONIFY_IS_BROKEN
		rep_call_lisp1 (Quniconify_window, win);
#endif
		rep_call_lisp1 (Qraise_window, win);
		rep_POPGC;
	    }
	    else if (ev->xconfigurerequest.detail == Below)
		rep_call_lisp1 (Qlower_window, rep_VAL(w));
	}
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
	if ((mask & CWX) || (mask & CWY))
	{
	    int old_x = w->attr.x, old_y = w->attr.y;
	    if (mask & CWX)
		w->attr.x = (ev->xconfigurerequest.x
			     + (w->reparented ? w->frame_x : 0));
	    if (ev->xconfigurerequest.value_mask & CWY)
		w->attr.y = (ev->xconfigurerequest.y
			     + (w->reparented ? w->frame_y : 0));
	    if (w->attr.x != old_x || w->attr.y != old_y)
		need_move = TRUE;
	}
	if ((mask & CWWidth) || (mask & CWHeight))
	{
	    int old_width = w->attr.width, old_height = w->attr.height;
	    if (ev->xconfigurerequest.value_mask & CWWidth)
		w->attr.width = ev->xconfigurerequest.width;
	    if (ev->xconfigurerequest.value_mask & CWHeight)
		w->attr.height = ev->xconfigurerequest.height;
	    if (w->attr.width != old_width || w->attr.height != old_height)
		need_resize = TRUE;
	}
	if (need_move)
	{
	    XMoveWindow (dpy, w->reparented ? w->frame : w->id,
			 w->attr.x, w->attr.y);
	}
	if (need_resize)
	    fix_window_size (w);
	if (need_move && !need_resize)
	    send_synthetic_configure (w);
    }
}

static void
configure_notify (XEvent *ev)
{
}

static void
create_notify (XEvent *ev)
{
}

static void
circulate_notify (XEvent *ev)
{
}

static void
shape_notify (XEvent *ev)
{
    XShapeEvent *sev = (XShapeEvent *)ev;
    Lisp_Window *w = find_window_by_id (sev->window);
    if (w != 0 && sev->window == w->id && sev->kind == ShapeBounding)
    {
	w->shaped = sev->shaped ? 1 : 0;
	set_window_shape (w);
    }
}



/* From the afterstep sources, ``According to the July 27, 1988 ICCCM
   draft, we should send a "synthetic" ConfigureNotify event to the
   client if the window was moved but not resized.'' */
void
send_synthetic_configure (Lisp_Window *w)
{
    XEvent ev;
    ev.type = ConfigureNotify;
    ev.xconfigure.display = dpy;
    ev.xconfigure.event = w->id;
    ev.xconfigure.window = w->id;
    ev.xconfigure.x = w->attr.x;
    ev.xconfigure.y = w->attr.y;
    if (w->reparented)
    {
	ev.xconfigure.x -= w->frame_x;
	ev.xconfigure.y -= w->frame_y;
    }
    ev.xconfigure.width = w->attr.width;
    ev.xconfigure.height = w->attr.height;
    ev.xconfigure.border_width = w->attr.border_width;
    ev.xconfigure.above = w->reparented ? w->frame : root_window;
    ev.xconfigure.override_redirect = False;
    XSendEvent (dpy, w->id, False, StructureNotifyMask, &ev);
}

long
get_event_mask (int type)
{
    if (type >= 0 && type < LASTEvent)
	return event_masks[type];
    else
	return 0;
}


/* Event loop */

/* Handle all available X events matching event mask MASK. Or any events
   if MASK is zero. */
void
handle_input_mask(long mask)
{
    /* Read all events in the input queue. */
    while(rep_throw_value == rep_NULL)
    {
	XEvent xev;
	if (mask == 0)
	{
	    if(XEventsQueued(dpy, QueuedAfterReading) <= 0)
		break;
	    XNextEvent(dpy, &xev);
	}
	else
	{
	    if (!XCheckMaskEvent (dpy, mask, &xev))
		break;
	}

	emit_pending_destroys ();

	if (xev.type == NoExpose || xev.type == GraphicsExpose)
	    continue;

	DB(("** Event: %s (win %lx)\n",
	    xev.type < LASTEvent ? event_names[xev.type] : "unknown",
	    (long)xev.xany.window));
	record_event_time (&xev);
	current_x_event = &xev;
	current_event_updated_mouse = FALSE;
	if (xev.type < LASTEvent && event_handlers[xev.type] != 0)
	    event_handlers[xev.type] (&xev);
	else if (xev.type == shape_event_base + ShapeNotify)
	    shape_notify (&xev);
	else
	    fprintf (stderr, "warning: unhandled event: %d\n", xev.type);
	current_x_event = 0;
	XFlush (dpy);
    }
    /* in case a function is invoked from outside the event loop
       that passes last_event_time to an X function */
    last_event_time = CurrentTime;

    emit_pending_destroys ();
}

/* Handle all available X events on file descriptor FD. */
void
handle_sync_input (int fd)
{
    handle_input_mask (0);
}


/* Lisp functions */

DEFUN("query-pointer", Fquery_pointer, Squery_pointer, (repv get), rep_Subr1) /*
::doc:Squery-pointer::
query-pointer [GET-FROM-SERVER]

Returns (MOUSE-X . MOUSE-Y) representing the current mouse pointer position,
relative to the origin of the root window.

If GET-FROM-SERVER is non-nil then the position is read directly from
the server, otherwise it's taken from the current event (if possible).
::end:: */
{
    if (get != Qnil || current_x_event == 0 || !current_event_updated_mouse)
    {
	Window tmpw;
	int tmp;
	int x, y;
	if(XQueryPointer(dpy, root_window, &tmpw, &tmpw,
			 &x, &y, &tmp, &tmp, &tmp))
	{
	    record_mouse_position (x, y);
	}
	emit_pending_destroys ();
    }
    return Fcons (rep_MAKE_INT(current_mouse_x),
		  rep_MAKE_INT(current_mouse_y));
}

DEFUN("query-last-pointer", Fquery_last_pointer, Squery_last_pointer,
      (void), rep_Subr0) /*
::doc:Squery-last-pointer::
query-last-pointer

Returns (MOUSE-X . MOUSE-Y) representing the second most recent mouse
pointer position, relative to the root window.
::end:: */
{
    return Fcons (rep_MAKE_INT(previous_mouse_x),
		  rep_MAKE_INT(previous_mouse_y));
}

DEFUN("query-pointer-window", Fquery_pointer_window, Squery_pointer_window, (void), rep_Subr0) /*
::doc:Squery-pointer-window::
query-pointer-window

Returns the top-level window under the mouse pointer, or nil if the cursor
is in the root window.
::end:: */
{
    Window child, root;
    int win_x, win_y;
    u_int state;

    XQueryPointer (dpy, root_window, &root, &child,
		   &current_mouse_x, &current_mouse_y, &win_x, &win_y, &state);
    emit_pending_destroys ();
    if (child != 0)
    {
	Lisp_Window *w = find_window_by_id (child);
	if (w == 0)
	{
	    struct frame_part *fp = find_frame_part_by_window (child);
	    if (fp != 0)
		w = fp->win;
	}
	return w ? rep_VAL(w) : Qnil;
    }
    return Qnil;
}

DEFUN("accept-x-input", Faccept_x_input, Saccept_x_input, (repv mask), rep_Subr1) /*
::doc:Saccept-x-input::
accept-x-input [EVENT-MASK]

Handle any X events received. If EVENT-MASK is non-nil then only events
matching this numeric value are handled (see <X11/X.h>).
::end:: */
{
    handle_input_mask (rep_INTP(mask) ? rep_INT(mask) : 0);
    return Qt;
}


/* initialisation */

void
events_init (void)
{
    event_handlers[VisibilityNotify] = visibility_notify;
    event_handlers[ColormapNotify] = colormap_notify;
    event_handlers[KeyPress] = key_press;
    event_handlers[KeyRelease] = key_press;
    event_handlers[ButtonPress] = button_press;
    event_handlers[ButtonRelease] = button_press;
    event_handlers[MotionNotify] = motion_notify;
    event_handlers[PropertyNotify] = property_notify;
    event_handlers[ClientMessage] = client_message;
    event_handlers[Expose] = expose;
    event_handlers[DestroyNotify] = destroy_notify;
    event_handlers[MapRequest] = map_request;
    event_handlers[MapNotify] = map_notify;
    event_handlers[UnmapNotify] = unmap_notify;
    event_handlers[EnterNotify] = enter_notify;
    event_handlers[LeaveNotify] = leave_notify;
    event_handlers[FocusIn] = focus_in;
    event_handlers[FocusOut] = focus_out;
    event_handlers[ConfigureRequest] = configure_request;
    event_handlers[ConfigureNotify] = configure_notify;
    event_handlers[ReparentNotify] = reparent_notify;
    event_handlers[CreateNotify] = create_notify;
    event_handlers[CirculateNotify] = circulate_notify;

    event_names[KeyPress] = "KeyPress";
    event_names[KeyRelease] = "KeyRelease";
    event_names[ButtonPress] = "ButtonPress";
    event_names[ButtonRelease] = "ButtonRelease";
    event_names[MotionNotify] = "MotionNotify";
    event_names[EnterNotify] = "EnterNotify";
    event_names[LeaveNotify] = "LeaveNotify";
    event_names[FocusIn] = "FocusIn";
    event_names[FocusOut] = "FocusOut";
    event_names[KeymapNotify] = "KeymapNotify";
    event_names[Expose] = "Expose";
    event_names[GraphicsExpose] = "GraphicsExpose";
    event_names[NoExpose] = "NoExpose";
    event_names[VisibilityNotify] = "VisibilityNotify";
    event_names[CreateNotify] = "CreateNotify";
    event_names[DestroyNotify] = "DestroyNotify";
    event_names[UnmapNotify] = "UnmapNotify";
    event_names[MapNotify] = "MapNotify";
    event_names[MapRequest] = "MapRequest";
    event_names[ReparentNotify] = "ReparentNotify";
    event_names[ConfigureNotify] = "ConfigureNotify";
    event_names[ConfigureRequest] = "ConfigureRequest";
    event_names[GravityNotify] = "GravityNotify";
    event_names[ResizeRequest] = "ResizeRequest";
    event_names[CirculateNotify] = "CirculateNotify";
    event_names[CirculateRequest] = "CirculateRequest";
    event_names[PropertyNotify] = "PropertyNotify";
    event_names[SelectionClear] = "SelectionClear";
    event_names[SelectionRequest] = "SelectionRequest";
    event_names[SelectionNotify] = "SelectionNotify";
    event_names[ColormapNotify] = "ColormapNotify";
    event_names[ClientMessage] = "ClientMessage";
    event_names[MappingNotify] = "MappingNotify";

    event_masks[KeyPress] = KeyPressMask;
    event_masks[KeyRelease] = KeyReleaseMask;
    event_masks[ButtonPress] = ButtonPressMask;
    event_masks[ButtonRelease] = ButtonReleaseMask;
    event_masks[EnterNotify] = EnterWindowMask;
    event_masks[LeaveNotify] = LeaveWindowMask;
    event_masks[MotionNotify] = (PointerMotionMask | PointerMotionHintMask
				| Button1MotionMask | Button2MotionMask
				| Button3MotionMask | Button4MotionMask
				| Button5MotionMask | ButtonMotionMask);
    event_masks[FocusIn] = FocusChangeMask;
    event_masks[FocusOut] = FocusChangeMask;
    event_masks[KeymapNotify] = KeymapStateMask;
    event_masks[Expose] = ExposureMask;
    event_masks[GraphicsExpose] = 0;
    event_masks[NoExpose] = 0;
    event_masks[VisibilityNotify] = VisibilityChangeMask;
    event_masks[CreateNotify] = SubstructureNotifyMask;
    event_masks[DestroyNotify] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[UnmapNotify] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[MapNotify] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[MapRequest] = 0;
    event_masks[ReparentNotify] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[ConfigureNotify] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[ConfigureRequest] = 0;
    event_masks[GravityNotify] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[ResizeRequest] = 0;
    event_masks[CirculateNotify] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[CirculateRequest] = StructureNotifyMask | SubstructureNotifyMask;
    event_masks[PropertyNotify] = PropertyChangeMask;
    event_masks[SelectionClear] = 0;
    event_masks[SelectionRequest] = 0;
    event_masks[SelectionNotify] = 0;
    event_masks[ColormapNotify] = ColormapChangeMask;
    event_masks[ClientMessage] = 0;
    event_masks[MappingNotify] = StructureNotifyMask | SubstructureNotifyMask;

    rep_ADD_SUBR(Squery_pointer);
    rep_ADD_SUBR(Squery_last_pointer);
    rep_ADD_SUBR(Squery_pointer_window);
    rep_ADD_SUBR(Saccept_x_input);

    rep_INTERN(visibility_notify_hook);
    rep_INTERN(destroy_notify_hook);
    rep_INTERN(map_notify_hook);
    rep_INTERN(unmap_notify_hook);
    rep_INTERN(reparent_notify_hook);
    rep_INTERN(property_notify_hook);
    rep_INTERN(enter_notify_hook);
    rep_INTERN(leave_notify_hook);
    rep_INTERN(focus_in_hook);
    rep_INTERN(focus_out_hook);
    rep_INTERN(client_message_hook);
    rep_INTERN(iconify_window);
    rep_INTERN(uniconify_window);
    rep_INTERN(root);
    rep_INTERN(new_value);
    rep_INTERN(deleted);
    rep_INTERN(raise_window);
    rep_INTERN(lower_window);
}

void
events_kill (void)
{
}
