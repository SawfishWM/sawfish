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
#include <limits.h>
#include <time.h>
#include <X11/extensions/shape.h>
#include <X11/Xresource.h>
#include <X11/Xatom.h>

/* Lookup table of event handlers */
void (*event_handlers[LASTEvent])(XEvent *ev);

/* Map events to their names for debugging */
static char *event_names[LASTEvent];

/* Map events to the mask selecting them */
static long event_masks[LASTEvent];

/* Most recent known mouse position relative to the root window */
static int current_mouse_x, current_mouse_y;

/* ..and the position (and window) at the last button-press */
static int button_press_mouse_x = -1, button_press_mouse_y = -1;
static Window button_press_window;
static bool pointer_in_motion;

/* Most recently seen server timestamp. */
Time last_event_time;

/* Current XEvent or a null pointer */
XEvent *current_x_event;
static bool current_event_updated_mouse;
static repv current_event_window;

/* We need a ButtonRelease on this fp. */
struct frame_part *clicked_frame_part;

static XID event_handler_context;

static Atom xa_sawmill_timestamp;

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
DEFSYM(window_moved_hook, "window-moved-hook");
DEFSYM(window_resized_hook, "window-resized-hook");
DEFSYM(shape_notify_hook, "shape-notify-hook");
DEFSYM(enter_frame_part_hook, "enter-frame-part-hook");
DEFSYM(leave_frame_part_hook, "leave-frame-part-hook");
DEFSYM(configure_request_hook, "configure-request-hook");
DEFSYM(window_state_change_hook, "window-state-change-hook");

DEFSYM(pointer_motion_threshold, "pointer-motion-threshold");

/* for enter/leave-notify-hook */
DEFSYM(root, "root");

/* for property-notify-hook */
DEFSYM(new_value, "new-value");
DEFSYM(deleted, "deleted");

DEFSYM(urgency, "urgency");

DEFSYM(stack, "stack");
DEFSYM(above, "above");
DEFSYM(below, "below");
DEFSYM(top_if, "top-if");
DEFSYM(bottom_if, "bottom-if");
DEFSYM(opposite, "opposite");
DEFSYM(dimensions, "dimensions");

/* for enter/leave-notify-hook */
DEFSYM(normal, "normal");
DEFSYM(grab, "grab");
DEFSYM(ungrab, "ungrab");

DEFSYM(ignore_fp_keymap, "ignore-fp-keymap");

repv Fsynthetic_configure_mutex (repv);

/* `Time' will always be 32-bits, due to underlying wire protocol (?) */
#define TIME_MAX 4294967295UL

/* Return the value of T2 - T1, compensating for clock wrap-arounds.
   If the difference is greater than half the possible range, assumes
   that T2 is _less_ than T1, returning a negative value */
static long
subtract_timestamps (Time t2, Time t1)
{
    Time diff = ((t2 >= t1)
		 ? (t2 - t1)
		 : (TIME_MAX - (t1 - t2) + 1));

    if (diff > TIME_MAX / 2)
	/* too big, assume it's negative */
	return - (long) (TIME_MAX - diff + 1);
    else
	return (long) diff;
}

/* Record the recently seen timestamp T */
void
save_timestamp (Time t)
{
    if (subtract_timestamps (t, last_event_time) > 0)
	last_event_time = t;

    DB(("  last_event_time=%lu\n", last_event_time));
}

/* Where possible record the timestamp from event EV */
static void
record_event_time (XEvent *ev)
{
    switch (ev->type)
    {
    case KeyPress:
    case KeyRelease:
	save_timestamp (ev->xkey.time);
	break;

    case ButtonPress:
    case ButtonRelease:
	save_timestamp (ev->xbutton.time);
	break;

    case MotionNotify:
	save_timestamp (ev->xmotion.time);
	break;

    case EnterNotify:
    case LeaveNotify:
	save_timestamp (ev->xcrossing.time);
	break;

    case PropertyNotify:
	save_timestamp (ev->xproperty.time);
	break;
    }
}

static void
record_mouse_position (int x, int y, int event_type, Window w)
{
    bool update_press = FALSE;

    switch (event_type)
    {
    case ButtonPress:
	update_press = TRUE;
	break;

    case MotionNotify:
	if (button_press_mouse_x == -1)
	    update_press = TRUE;
	break;

    case ButtonRelease:
	if (button_press_mouse_x == -1)
	    update_press = TRUE;
	break;
    }

    if (update_press)
    {
	button_press_mouse_x = x;
	button_press_mouse_y = y;
	button_press_window = w;
	pointer_in_motion = FALSE;
    }

    current_mouse_x = x;
    current_mouse_y = y;
    current_event_updated_mouse = TRUE;

    if (event_type == MotionNotify && !pointer_in_motion)
    {
	int delta_x = x - button_press_mouse_x;
	int delta_y = y - button_press_mouse_y;
	repv tem = Fsymbol_value (Qpointer_motion_threshold, Qt);
	int threshold = rep_INTP (tem) ? rep_INT (tem) : 0;
	
	if (ABS (delta_x) > threshold || ABS (delta_y) > threshold)
	    pointer_in_motion = TRUE;
    }
}

static void
install_colormaps (Lisp_Window *w)
{
    XWindowAttributes attr;
    bool seen_toplevel = FALSE;
    if (w->reparented)
	XInstallColormap (dpy, image_cmap);
    if (w->n_cmap_windows > 0)
    {
	int i;
	for (i = w->n_cmap_windows - 1; i >= 0; i--)
	{
	    XGetWindowAttributes (dpy, w->cmap_windows[i], &attr);
	    XInstallColormap (dpy, attr.colormap);
	    if (w->cmap_windows[i] == w->id)
		seen_toplevel = TRUE;
	}
    }
    if (!seen_toplevel)
    {
	XGetWindowAttributes (dpy, w->id, &attr);
	XInstallColormap (dpy, attr.colormap);
    }
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
    if (w != 0 && ev->xcolormap.window == w->id && ev->xcolormap.new)
    {
	if (w == focus_window)
	    install_colormaps (w);
    }
}

static void
key_press (XEvent *ev)
{
    record_mouse_position (ev->xkey.x_root, ev->xkey.y_root, ev->type, 0);

    /* Don't look for a context map, frame parts are never focused */
    eval_input_event (Qnil);

    XAllowEvents (dpy, SyncKeyboard, last_event_time);
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
    if (fp->clicked != old_clicked && fp->id != 0 && fp->win != 0)
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
	    if (clicked_frame_part->id != 0 && clicked_frame_part->win != 0)
		refresh_frame_part (clicked_frame_part);
	}
	clicked_frame_part = 0;
    }
}

/* Called when the pointer is ungrabbed (i.e. no ButtonRelease
   event will follow) */
void
synthesize_button_release (void)
{
    unclick_current_fp ();
    button_press_mouse_x = button_press_mouse_y = -1;
    button_press_window = 0;
}

static repv
current_context_map (void)
{
    repv map = Qnil;

    if (clicked_frame_part
	&& clicked_frame_part->clicked
	&& clicked_frame_part->win != 0
	&& clicked_frame_part->win->visible)
    {
	repv tem = Fwindow_get (rep_VAL (clicked_frame_part->win),
				Qignore_fp_keymap);
	if (tem == Qnil)
	    map = get_keymap_for_frame_part (clicked_frame_part);
    }

    return map;
}

static void
button_press (XEvent *ev)
{
    struct frame_part *fp;
    Lisp_Window *w = 0;

    record_mouse_position (ev->xbutton.x_root, ev->xbutton.y_root,
			   ev->type, ev->xany.window);

    fp = find_frame_part_by_window (ev->xbutton.window);
    if (fp != 0)
    {
	w = fp->win;

	if (ev->type == ButtonPress)
	    handle_fp_click (fp, ev);
    }

    /* Only use the context map if the frame part is currently clicked,
       and it's window is visible (i.e. not iconified) */
    eval_input_event (current_context_map ());

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

    if (ev->type == ButtonRelease)
    {
	button_press_mouse_x = button_press_mouse_y = -1;
	button_press_window = 0;
	/* The pointer is _always_ ungrabbed after a button-release */
	XUngrabPointer (dpy, last_event_time);
    }

    XAllowEvents (dpy, SyncPointer, last_event_time);
}

static void
motion_notify (XEvent *ev)
{
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
	record_mouse_position (x, y, ev->type, ev->xmotion.window);
    }

    if (pointer_in_motion)
	eval_input_event (current_context_map ());

    XAllowEvents (dpy, SyncPointer, last_event_time);
}

static void
property_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xproperty.window);
    repv w_ = rep_VAL (w);		/* type alias for gc-pro'ing */
    if (w != 0 && ev->xproperty.window == w->id)
    {
	bool need_refresh = FALSE, changed = TRUE;
	repv changed_states = Qnil;
	rep_GC_root gc_w, gc_changed_states;

	switch (ev->xproperty.atom)
	{
	    u_char *prop;
	    Atom actual;
	    int format;
	    long nitems, bytes_after;
	    long supplied;
	    bool old_urgency, new_urgency;

	case XA_WM_NAME:
	case XA_WM_ICON_NAME:
	    if (ev->xproperty.state == PropertyNewValue
		&& XGetWindowProperty (dpy, w->id, ev->xproperty.atom,
				       0, 200, False, AnyPropertyType, &actual,
				       &format, &nitems,
				       &bytes_after, &prop) == Success
		&& actual != None)
	    {
		if (format == 8 && w->id != 0)
		{
		    repv str = Qnil;
		    if (actual == xa_compound_text)
		    {
			char **text_list;
			XTextProperty tprop;
			int count;
			tprop.value = prop;
			tprop.encoding = actual;
			tprop.format = format;
			tprop.nitems = strlen (prop);
			if (XmbTextPropertyToTextList (dpy, &tprop,
						       &text_list, &count)
			    >= Success)
			{
			    if (count > 0)
			    	str = rep_string_dup (text_list[0]);
			    XFreeStringList(text_list);
			}
		    }
		    else if (actual == XA_STRING)
		    {
			str = rep_string_dup (prop);
		    }
		    if (str == Qnil)
			str = rep_null_string ();

		    if (ev->xproperty.atom == XA_WM_NAME)
		    {
			if (Fequal (w->name, str) == Qnil
			    || Fequal (w->full_name, str) == Qnil)
			{
			    w->full_name = w->name = str;
			    need_refresh = TRUE;
			}
			else
			    changed = FALSE;
		    }
		    else
		    {
			if (Fequal (w->icon_name, str) == Qnil)
			{
			    w->icon_name = str;
			    need_refresh = TRUE;
			}
			else
			    changed = FALSE;
		    }
		}
		XFree (prop);
	    }
	    break;

	case XA_WM_HINTS:
	    old_urgency = w->wmhints != 0 && w->wmhints->flags & XUrgencyHint;
	    if (w->wmhints != 0)
		XFree (w->wmhints);
	    w->wmhints = XGetWMHints (dpy, w->id);
	    new_urgency = w->wmhints != 0 && w->wmhints->flags & XUrgencyHint;
	    w->icon_image = rep_NULL;
	    need_refresh = TRUE;
	    if (old_urgency != new_urgency)
		changed_states = Fcons (Qurgency, changed_states);
	    break;

	case XA_WM_NORMAL_HINTS:
	    XGetWMNormalHints (dpy, w->id, &w->hints, &supplied);
	    break;

	case XA_WM_TRANSIENT_FOR:
	    if (!XGetTransientForHint (dpy, w->id, &w->transient_for_hint))
		w->transient_for_hint = 0;
	    break;

	default:
	    if (ev->xproperty.atom == xa_wm_colormap_windows)
	    {
		if (w->n_cmap_windows > 0)
		    XFree (w->cmap_windows);
		if (!XGetWMColormapWindows (dpy, w->id, &w->cmap_windows,
					    &w->n_cmap_windows))
		{
		    w->n_cmap_windows = 0;
		}
		if (w == focus_window)
		    install_colormaps (w);
	    }
	}

	rep_PUSHGC (gc_w, w_);
	rep_PUSHGC (gc_changed_states, changed_states);

	if (need_refresh && w->reparented
	    && w->property_change != 0 && w->id != 0)
	{
	    w->property_change (w);
	}

	if (changed)
	{
	    Fcall_window_hook (Qproperty_notify_hook, rep_VAL(w),
			       rep_list_2 (x_atom_symbol (ev->xproperty.atom),
					   ev->xproperty.state
					   == PropertyNewValue
					   ? Qnew_value : Qdeleted), Qnil);
	}
	if (changed_states != Qnil)
	{
	    Fcall_window_hook (Qwindow_state_change_hook, rep_VAL (w),
			       rep_LIST_1 (changed_states), Qnil);
	}

	rep_POPGC; rep_POPGC;
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
}

static void
destroy_notify (XEvent *ev)
{
    Lisp_Window *w = x_find_window_by_id (ev->xdestroywindow.window);
    if (w == 0 || ev->xdestroywindow.window != w->saved_id)
	return;
    remove_window (w, Qt, Qnil);
    emit_pending_destroys ();
}

void
map_request (XEvent *ev)
{
    DEFSTRING (iconify_mod, "sawfish.wm.state.iconify");
    Window id = ev->xmaprequest.window;
    Lisp_Window *w = find_window_by_id (id);
    if (w == 0)
    {
	w = add_window (id);
	if (w == 0)
	    return;

	if (w->wmhints && w->wmhints->flags & StateHint
	    && w->wmhints->initial_state == IconicState)
	{
	    rep_call_lisp1 (module_symbol_value (rep_VAL (&iconify_mod),
						 Qiconify_window), rep_VAL(w));
	}
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
	w->mapped = TRUE;
	rep_call_lisp1 (module_symbol_value (rep_VAL (&iconify_mod),
					     Quniconify_window), rep_VAL(w));
    }

    if (!w->client_unmapped)
	XMapWindow (dpy, w->id);
    else
	/* wouldn't happen otherwise */
	Fcall_window_hook (Qmap_notify_hook, rep_VAL(w), Qnil, Qnil);

    if (w->visible)
	XMapWindow (dpy, w->frame);
}

static void
reparent_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xreparent.window);
    if (w != 0 && ev->xreparent.window == w->id
	&& ev->xreparent.event == w->id)
    {
	if (ev->xreparent.parent != root_window
	    && ev->xreparent.parent != w->frame)
	{
	    /* Not us doing the reparenting.. */
	    remove_window (w, Qnil, Qnil);
	    XReparentWindow (dpy, ev->xreparent.window, ev->xreparent.parent,
			     ev->xreparent.x, ev->xreparent.y);
	}
	Fcall_window_hook (Qreparent_notify_hook, rep_VAL(w), Qnil, Qnil);
    }
}

static void
map_notify (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xmap.window);
    if (w != 0 && ev->xmap.window == w->id && ev->xmap.event == w->id)
    {
	XWindowAttributes wa;
	XGetWindowAttributes (dpy, w->id, &wa);
	if (wa.override_redirect)
	{
	    /* arrgh, the window changed its override redirect status.. */
	    remove_window (w, Qnil, Qnil);
	}
	else
	{
	    /* copy in some of the new values */
	    w->attr.width = wa.width;
	    w->attr.height = wa.height;

	    w->mapped = TRUE;
	    if (w->frame == 0)
		create_window_frame (w);
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
    if (w != 0 && ev->xunmap.window == w->id
	&& (ev->xunmap.event == w->id || ev->xunmap.send_event))
    {
	w->mapped = FALSE;
	if (w->reparented)
	{
	    if (w->visible)
	    {
		XUnmapWindow (dpy, w->frame);
		reset_frame_parts (w);
	    }
	    /* Removing the frame reparents the client window back to
	       the root. This means that we receive the next MapRequest
	       for the window. */
	    remove_window_frame (w);
	    destroy_window_frame (w, FALSE);
	}
	Fcall_window_hook (Qunmap_notify_hook, rep_VAL(w), Qnil, Qnil);

	if (focus_window == w)
	    focus_on_window (0);

	XDeleteProperty (dpy, w->id, xa_wm_state);
    }
}

static inline repv
mode_to_sym (int mode)
{
    return (mode == NotifyNormal ? Qnormal
	    : mode == NotifyGrab ? Qgrab
	    : Qungrab);
}

static void
enter_notify (XEvent *ev)
{
    struct frame_part *fp;
    repv mode = mode_to_sym (ev->xcrossing.mode);
    if (ev->xcrossing.window == root_window)
	Fcall_hook (Qenter_notify_hook, rep_list_2 (Qroot, mode), Qnil);
    else if ((fp = find_frame_part_by_window (ev->xcrossing.window)) != 0)
    {
	repv tem;
	bool refresh = FALSE;
	Lisp_Window *w = fp->win;
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

	tem = Fassq (Qclass, fp->alist);
	if (tem && rep_CONSP(tem) && w->id != 0)
	{
	    Fcall_window_hook (Qenter_frame_part_hook, rep_VAL(w),
			       rep_list_2 (rep_VAL(fp), mode), Qnil);
	}
    }
    else
    {
	Lisp_Window *w = find_window_by_id (ev->xcrossing.window);
	if (w != 0 && w->mapped && w->visible
	    && ev->xcrossing.detail != NotifyInferior)
	{
	    Fcall_window_hook (Qenter_notify_hook, rep_VAL(w),
			       rep_LIST_1 (mode), Qnil);
	}
    }
}

static void
leave_notify (XEvent *ev)
{
    struct frame_part *fp;
    repv mode = mode_to_sym (ev->xcrossing.mode);
    if (ev->xcrossing.window == root_window)
    {
	Fcall_hook (Qleave_notify_hook, rep_LIST_2 (Qroot, mode), Qnil);
    }
    else if ((fp = find_frame_part_by_window (ev->xcrossing.window)) != 0)
    {
	repv tem;
	bool refresh = FALSE;
	Lisp_Window *w = fp->win;
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

	tem = Fassq (Qclass, fp->alist);
	if (tem && rep_CONSP(tem) && w->id != 0)
	{
	    Fcall_window_hook (Qleave_frame_part_hook, rep_VAL(w),
			       rep_LIST_2 (rep_VAL(fp), mode), Qnil);
	}
    }
    else
    {
	Lisp_Window *w = find_window_by_id (ev->xcrossing.window);
	if (w != 0 && w->mapped && w->visible
	    && ev->xcrossing.detail != NotifyInferior)
	{
	    Fcall_window_hook (Qleave_notify_hook, rep_VAL(w),
			       rep_LIST_1 (mode), Qnil);
	}
    }
}

static Lisp_Window *last_focused;

static void
focus_in (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xfocus.window);
    if (ev->xfocus.detail == NotifyPointer)
	return;
    if (w != 0 && w->visible)
    {
	focus_window = w;
	if (last_focused != w)
	{
	    last_focused = w;
	    install_colormaps (w);
	    if (w->focus_change != 0)
	    {
		DB (("  calling focus change %p on %s\n",
		     w->focus_change, rep_STR(w->name)));
		w->focus_change (w);
	    }
	    if (w->id != 0)
	    {
		Fcall_window_hook (Qfocus_in_hook, rep_VAL(w),
				   rep_LIST_1 (mode_to_sym (ev->xfocus.mode)),
				   Qnil);
	    }
	}
    }
    else if (ev->xfocus.window == root_window)
	focus_on_window (0);
}

static void
focus_out (XEvent *ev)
{
    Lisp_Window *w = find_window_by_id (ev->xfocus.window);
    if (ev->xfocus.detail == NotifyPointer)
	return;
    if (w != 0 && ev->xfocus.detail != NotifyInferior)
    {
	if (focus_window == w)
	    focus_window = 0;
	if (last_focused == w)
	{
	    last_focused = 0;
	    if (w->focus_change != 0)
	    {
		DB (("  calling focus change %p on %s\n",
		     w->focus_change, rep_STR(w->name)));
		w->focus_change (w);
	    }
	    if (w->id != 0)
	    {
		Fcall_window_hook (Qfocus_out_hook, rep_VAL(w),
				   rep_LIST_1 (mode_to_sym (ev->xfocus.mode)),
				   Qnil);
	    }
	}
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
		      (CWX | CWY | CWWidth | CWHeight
		       | CWStackMode | CWSibling));
	xwc.x = ev->xconfigurerequest.x;
	xwc.y = ev->xconfigurerequest.y;
	xwc.width = ev->xconfigurerequest.width;
	xwc.height = ev->xconfigurerequest.height;
	xwc.sibling = ev->xconfigurerequest.above;
	xwc.stack_mode = ev->xconfigurerequest.detail;
	XConfigureWindow (dpy, ev->xconfigurerequest.window, xwcm, &xwc);
    }
    else if (w != 0)
    {
	u_long mask = ev->xconfigurerequest.value_mask;
	repv alist = Qnil;
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
	if (mask & CWStackMode)
	{
	    Window above_win;
	    repv relation = Qnil, sibling = Qnil;

	    above_win = ev->xconfigurerequest.above;
	    if (above_win != 0)
	    {
		Lisp_Window *tem = find_window_by_id (above_win);
		if (tem != 0)
		    sibling = rep_VAL (tem);
	    }

	    switch (ev->xconfigurerequest.detail)
	    {
	    case Above:
		relation = Qabove;
		break;
	    case Below:
		relation = Qbelow;
		break;
	    case TopIf:
		relation = Qtop_if;
		break;
	    case BottomIf:
		relation = Qbottom_if;
		break;
	    case Opposite:
		relation = Qopposite;
		break;
	    }

	    if (relation != Qnil)
	    {
		repv stack_data;
		stack_data = Fcons (relation, Fcons (sibling, Qnil));
		alist = Fcons (Fcons (Qstack, stack_data), alist);
	    }
	}
	if ((mask & CWX) && (mask & CWY))
	{
	    int x = ev->xconfigurerequest.x;
	    int y = ev->xconfigurerequest.y;
	    alist = Fcons (Fcons (Qposition,
				  Fcons (rep_MAKE_INT (x),
					 rep_MAKE_INT (y))), alist);
	}
	if ((mask & CWWidth) || (mask & CWHeight))
	{
	    int width = w->attr.width, height = w->attr.height;
	    if (ev->xconfigurerequest.value_mask & CWWidth)
		width = ev->xconfigurerequest.width;
	    if (ev->xconfigurerequest.value_mask & CWHeight)
		height = ev->xconfigurerequest.height;
	    if (w->attr.width != width || w->attr.height != height)
	    {
		alist = Fcons (Fcons (Qdimensions,
				      Fcons (rep_MAKE_INT (width),
					     rep_MAKE_INT (height))), alist);
	    }
	}
	if (alist != Qnil)
	{
	    /* Be sure to send one (and only one) synthetic ConfigureNotify
	       to the window. The ICCCM states that we should send the event
	       even if the state of the window doesn't change */

	    Fsynthetic_configure_mutex (Qt);
	    send_synthetic_configure (w);

	    Fcall_window_hook (Qconfigure_request_hook, rep_VAL(w),
			       rep_LIST_1 (alist), Qnil);

	    Fsynthetic_configure_mutex (Qnil);
	}
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
mapping_notify (XEvent *ev)
{
    if (ev->xmapping.request == MappingModifier
	|| ev->xmapping.request == MappingKeyboard)
    {
	XRefreshKeyboardMapping (&ev->xmapping);
	update_keyboard_mapping ();
    }
}

static void
shape_notify (XEvent *ev)
{
    XShapeEvent *sev = (XShapeEvent *)ev;
    Lisp_Window *w = find_window_by_id (sev->window);
    if (w != 0 && sev->window == w->id && sev->kind == ShapeBounding)
    {
	w->shaped = sev->shaped ? 1 : 0;
	queue_reshape_frame (w);
	Fcall_window_hook (Qshape_notify_hook, rep_VAL(w), Qnil, Qnil);
    }
}



static int synthetic_configure_mutex;

/* From the afterstep sources, ``According to the July 27, 1988 ICCCM
   draft, we should send a "synthetic" ConfigureNotify event to the
   client if the window was moved but not resized.'' */
void
send_synthetic_configure (Lisp_Window *w)
{
    if (!synthetic_configure_mutex)
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
	w->pending_configure = 0;
    }
    else
	w->pending_configure = 1;
}

DEFUN("synthetic-configure-mutex", Fsynthetic_configure_mutex,
      Ssynthetic_configure_mutex, (repv arg), rep_Subr1) /*
::doc:sawfish.wm.events#synthetic-configure-mutex::
While this variable is non-nil no synthetic ConfigureNotify events will
be sent to windows.
::end:: */
{
    repv ret = synthetic_configure_mutex ? Qt : Qnil;
    synthetic_configure_mutex = (arg != Qnil);
    if (!synthetic_configure_mutex)
    {
	Lisp_Window *w;
	for (w = window_list; w != 0; w = w->next)
	{
	    if (w->pending_configure && w->id != 0)
		send_synthetic_configure (w);
	}
    }
    return ret;
}

long
get_event_mask (int type)
{
    if (type >= 0 && type < LASTEvent)
	return event_masks[type];
    else
	return 0;
}

/* Fetch a recent timestamp from the server. */
Time
get_server_timestamp (void)
{
    XEvent ev;
    Window w = no_focus_window;			/* XXX abuse */

    /* XXX There must be an easier method.. */
    while (XCheckWindowEvent (dpy, w, PropertyChangeMask, &ev)) ;
    XSelectInput (dpy, w, PropertyChangeMask | KeyPressMask);
    XChangeProperty (dpy, w, xa_sawmill_timestamp,
		     XA_STRING, 8, PropModeReplace, "foo", 3);
    XSelectInput (dpy, w, KeyPressMask);
    XWindowEvent (dpy, w, PropertyChangeMask, &ev);

    return ev.xproperty.time;
}


/* Window-local event handlers */

/* Register that FUN should be called for any events received from
   the window with id W. */
void
register_event_handler (Window w, void (*fun)(XEvent *ev))
{
    XSaveContext (dpy, w, event_handler_context, (XPointer) fun);
}

/* Remove any event handler associated with window id W. */
void
deregister_event_handler (Window w)
{
    XDeleteContext (dpy, w, event_handler_context);
}

static inline void *
window_event_handler (Window w)
{
    void *fun;
    return XFindContext (dpy, w, event_handler_context,
			 (XPointer *)&fun) ? 0 : fun;
}


/* Event loop */

static repv
inner_handle_input (repv arg)
{
    XEvent *ev = (XEvent *) rep_PTR (arg);

    void (*handler)(XEvent *) = window_event_handler (ev->xany.window);
    if (handler != 0)
	(*handler) (ev);
    else if (ev->type < LASTEvent && event_handlers[ev->type] != 0)
	event_handlers[ev->type] (ev);
    else if (ev->type == shape_event_base + ShapeNotify)
	shape_notify (ev);
    else
	fprintf (stderr, "warning: unhandled event: %d\n", ev->type);
    return Qnil;
}

/* Handle all available X events matching event mask MASK. Or any events
   if MASK is zero. */
void
handle_input_mask(long mask)
{
    static time_t last_time;

    time_t current_time = time (0);
    if (current_time < last_time)
    {
	/* Hmm. Looks like the clock's been turned backwards. Refetch the
	   server timestamp so we don't ignore any following timestamps */
	last_event_time = get_server_timestamp ();
    }
    last_time = current_time;

    /* Read all events in the input queue. */
    while(rep_throw_value == rep_NULL)
    {
	XEvent xev, *old_current_event = current_x_event;
	repv old_current_window = current_event_window;
	rep_GC_root gc_old_current_window;

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

	rep_PUSHGC(gc_old_current_window, old_current_window);
	emit_pending_destroys ();
	rep_POPGC;

	if (xev.type == NoExpose || xev.type == GraphicsExpose)
	    continue;

	DB(("** Event: %s (win %lx)\n",
	    xev.type < LASTEvent ? event_names[xev.type] : "unknown",
	    (long)xev.xany.window));

	record_event_time (&xev);
	current_x_event = &xev;
	current_event_updated_mouse = FALSE;
	current_event_window = rep_NULL;

	rep_PUSHGC(gc_old_current_window, old_current_window);
	rep_call_with_barrier (inner_handle_input, rep_VAL (&xev),
			       rep_TRUE, 0, 0, 0);
	rep_POPGC;

	current_x_event = old_current_event;
	current_event_window = old_current_window;
	XFlush (dpy);
    }

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
::doc:sawfish.wm.events#query-pointer::
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
	    record_mouse_position (x, y, -1, 0);
	}
	emit_pending_destroys ();
    }
    return Fcons (rep_MAKE_INT(current_mouse_x),
		  rep_MAKE_INT(current_mouse_y));
}

DEFUN("query-button-press-pointer", Fquery_button_press_pointer,
      Squery_button_press_pointer, (void), rep_Subr0) /*
::doc:sawfish.wm.events#query-button-press-pointer::
query-button-press-pointer

Returns (MOUSE-X . MOUSE-Y) representing the mouse position relative to
the root window at the last button-press event.
::end:: */
{
    if (button_press_mouse_x != -1)
    {
	return Fcons (rep_MAKE_INT(button_press_mouse_x),
		      rep_MAKE_INT(button_press_mouse_y));
    }
    else
	return Qnil;
}

DEFUN("query-button-press-window", Fquery_button_press_window,
      Squery_button_press_window, (void), rep_Subr0) /*
::doc:sawfish.wm.events#query-button-press-window::
query-button-press-window

Returns the window that the mouse was in when the button was pressed.
::end:: */
{
    Lisp_Window *w;
    struct frame_part *fp;
    if (button_press_window == 0)
	return Qnil;
    if (button_press_window == root_window)
	return Qroot;
    w = find_window_by_id (button_press_window);
    if (w != 0)
	return rep_VAL(w);
    fp = find_frame_part_by_window (button_press_window);
    if (fp != 0)
	return rep_VAL(fp->win);
    return rep_MAKE_INT(button_press_window);
}

DEFUN("query-pointer-window", Fquery_pointer_window, Squery_pointer_window, (void), rep_Subr0) /*
::doc:sawfish.wm.events#query-pointer-window::
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
::doc:sawfish.wm.events#accept-x-input::
accept-x-input [EVENT-MASK]

Handle any X events received. If EVENT-MASK is non-nil then only events
matching this numeric value are handled (see <X11/X.h>).
::end:: */
{
    handle_input_mask (rep_INTP(mask) ? rep_INT(mask) : 0);
    return Qt;
}

DEFUN("current-event-window", Fcurrent_event_window, Scurrent_event_window,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.events#current-event-window::
current-event-window

Return the window that received the current event, or the symbol
`root', or nil if no such window.
::end:: */
{
    if (WINDOWP(win))
	current_event_window = win;
    if (current_event_window == rep_NULL)
    {
	if (current_x_event != 0)
	{
	    struct frame_part *fp;
	    Lisp_Window *w = find_window_by_id (current_x_event->xany.window);
	    if (w == 0)
	    {
		fp = find_frame_part_by_window (current_x_event->xany.window);
		if (fp != 0)
		    w = fp->win;
	    }
	    if (w != 0)
		current_event_window = rep_VAL(w);
	    else if (current_x_event->xany.window == root_window)
		current_event_window = Qroot;
	    else
		current_event_window = Qnil;
	}
	else
	    current_event_window = Qnil;
    }

    return current_event_window;
}

DEFUN("x-server-timestamp", Fx_server_timestamp, Sx_server_timestamp,
      (repv from_server), rep_Subr1) /*
::doc:sawfish.wm.events#x-server-timestamp::
x-server-timestamp [FROM-SERVER]

Return a recent X server timestamp, as an integer.

If FROM-SERVER is non-nil the timestamp is read directly from the
server, otherwise the most recent timestamp seen by the window manager
(i.e. from an event) is returned.
::end:: */
{
    Time time = ((from_server == Qnil)
		 ? last_event_time : get_server_timestamp ());
    return rep_make_long_uint (time);
}

DEFUN("x-events-queued", Fx_events_queued, Sx_events_queued, (void), rep_Subr0)
{
    return rep_MAKE_INT (XEventsQueued (dpy, QueuedAfterReading));
}

DEFUN("clicked-frame-part", Fclicked_frame_part,
      Sclicked_frame_part, (void), rep_Subr0)
{
    return (clicked_frame_part != 0) ? rep_VAL (clicked_frame_part) : Qnil;
}


/* initialisation */

void
events_init (void)
{
    repv tem;

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
    event_handlers[MappingNotify] = mapping_notify;

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

    tem = rep_push_structure ("sawfish.wm.events");
    rep_ADD_SUBR(Squery_pointer);
    rep_ADD_SUBR(Squery_button_press_pointer);
    rep_ADD_SUBR(Squery_button_press_window);
    rep_ADD_SUBR(Squery_pointer_window);
    rep_ADD_SUBR(Saccept_x_input);
    rep_ADD_SUBR(Scurrent_event_window);
    rep_ADD_SUBR(Sx_server_timestamp);
    rep_ADD_SUBR(Sx_events_queued);
    rep_ADD_SUBR(Sclicked_frame_part);
    rep_ADD_SUBR(Ssynthetic_configure_mutex);
    rep_pop_structure (tem);

    rep_INTERN_SPECIAL(visibility_notify_hook);
    rep_INTERN_SPECIAL(destroy_notify_hook);
    rep_INTERN_SPECIAL(map_notify_hook);
    rep_INTERN_SPECIAL(unmap_notify_hook);
    rep_INTERN_SPECIAL(reparent_notify_hook);
    rep_INTERN_SPECIAL(property_notify_hook);
    rep_INTERN_SPECIAL(enter_notify_hook);
    rep_INTERN_SPECIAL(leave_notify_hook);
    rep_INTERN_SPECIAL(focus_in_hook);
    rep_INTERN_SPECIAL(focus_out_hook);
    rep_INTERN_SPECIAL(client_message_hook);
    rep_INTERN_SPECIAL(window_moved_hook);
    rep_INTERN_SPECIAL(window_resized_hook);
    rep_INTERN_SPECIAL(shape_notify_hook);
    rep_INTERN_SPECIAL(enter_frame_part_hook);
    rep_INTERN_SPECIAL(leave_frame_part_hook);
    rep_INTERN_SPECIAL(configure_request_hook);
    rep_INTERN_SPECIAL(window_state_change_hook);

    rep_INTERN_SPECIAL(pointer_motion_threshold);
    Fset (Qpointer_motion_threshold, rep_MAKE_INT (0));

    rep_INTERN(iconify_window);
    rep_INTERN(uniconify_window);
    rep_INTERN(root);
    rep_INTERN(new_value);
    rep_INTERN(deleted);
    rep_INTERN(urgency);

    rep_INTERN(stack);
    rep_INTERN(above);
    rep_INTERN(below);
    rep_INTERN(bottom_if);
    rep_INTERN(top_if);
    rep_INTERN(opposite);
    rep_INTERN(dimensions);

    rep_INTERN(normal);
    rep_INTERN(grab);
    rep_INTERN(ungrab);

    rep_INTERN(ignore_fp_keymap);

    rep_mark_static (&current_event_window);

    event_handler_context = XUniqueContext ();

    if(!batch_mode_p ())
    {
	xa_sawmill_timestamp = XInternAtom (dpy, "_SAWMILL_TIMESTAMP", False);
	last_event_time = get_server_timestamp ();
    }
}

void
events_kill (void)
{
}
