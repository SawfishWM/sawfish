/* windows.c -- window manipulation
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

#include "sawfish.h"
#include <assert.h>
#include <string.h>
#include <X11/extensions/shape.h>
#include <glib.h>

Lisp_Window *window_list;
int window_type;

Lisp_Window *focus_window;

int pending_destroys;

static bool initialising;

DEFSYM(add_window_hook, "add-window-hook");
DEFSYM(before_add_window_hook, "before-add-window-hook");
DEFSYM(after_add_window_hook, "after-add-window-hook");
DEFSYM(place_window_hook, "place-window-hook");
DEFSYM(placed, "placed");
DEFSYM(after_framing_hook, "after-framing-hook");
DEFSYM(after_initialization_hook, "after-initialization-hook");
DEFSYM(remove_window_hook, "remove-window-hook");

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

/* In sawfish-1.3.3, the only callback used is keymap_prop_change. */

static struct prop_handler *prop_handlers;

struct prop_handler {
    struct prop_handler *next;
    repv prop;
    void (*callback) (Lisp_Window *w, repv prop, repv old, repv new);
};

/* utilities */

/* Returns true if we should manage window ID */
bool
mapped_not_override_p (Window id)
{
    XWindowAttributes wa;

    XGetWindowAttributes(dpy, id, &wa);
    return ((wa.map_state != IsUnmapped) && (wa.override_redirect != True));
}

/* Returns true if the window's Input hint is set (or defaults to being set) */
static bool
window_input_hint_p (Lisp_Window *w)
{
    if (w->wmhints != 0 && (w->wmhints->flags & InputHint))
	return w->wmhints->input;
    else
	return TRUE;
}

static Window queued_focus_id;
static bool queued_take_focus;
static bool queued_set_focus;
static int queued_focus_revert;
static Time queued_focus_time;

/* We can lose the focus sometimes, notably after a was-focused
   window is closed while a keyboard grab exists.. (netscape) */
static void
check_for_lost_focus (void)
{
    Window focus;
    int revert_to;
    XGetInputFocus (dpy, &focus, &revert_to);
    if (focus == None || focus == PointerRoot)
    {
	DB (("lost focus (%ld)\n", focus));
	focus_on_window (focus_window);
    }
}

void
commit_queued_focus_change (void)
{
    if (0 && queued_focus_id == 0)
	check_for_lost_focus ();

    if (queued_focus_id != 0)
    {
	if (queued_take_focus)
	{
	    DB(("  sending WM_TAKE_FOCUS %x %ld\n",
		(unsigned) queued_focus_id, queued_focus_time));
	    send_client_message (queued_focus_id,
				 xa_wm_take_focus,
				 queued_focus_time);
	}
	if (queued_set_focus)
	{
	    DB(("  focusing %x %ld\n",
		(unsigned) queued_focus_id, queued_focus_time));
	    XSetInputFocus (dpy, queued_focus_id,
			    queued_focus_revert, queued_focus_time);
	}
	queued_focus_id = 0;
    }
}

/* Give the input focus to window W, or to no window if W is null */
void
focus_on_window (Lisp_Window *w)
{
    /* something's going to change, so */
    queued_focus_id = 0;
    queued_focus_time = last_event_time;
    queued_set_focus = FALSE;
    queued_take_focus = FALSE;

    if (w != 0 && !WINDOW_IS_GONE_P (w) && w->visible)
    {
	DB(("focus_on_window (%s)\n", rep_STR(w->name)));
	if (!w->client_unmapped)
	{
	    queued_focus_id = w->id;

	    if (w->does_wm_take_focus)
	    {
		queued_take_focus = TRUE;

		if (window_input_hint_p (w))
		    queued_set_focus = TRUE;
	    }
	    else
		queued_set_focus = TRUE;
	}
	else
	{
	    queued_focus_id = w->frame;
	    queued_set_focus = TRUE;
	}

	queued_focus_revert = RevertToParent;
    }

    if (queued_focus_id == 0 || (!queued_set_focus && !queued_take_focus))
    {
	DB(("focus_on_window (nil)\n"));
	queued_focus_id = no_focus_window;
	queued_set_focus = TRUE;
	queued_focus_revert = RevertToNone;
	queued_focus_time = last_event_time;
    }
}

/* Should be called when W is no longer focusable. */
void
focus_off_window (Lisp_Window *w)
{
    if (w == focus_window)
    {
	focus_window = 0;

	/* Do this immediately. Any real focus-change will be queued,
	   so will happen after this. Doing this here just prevents us
	   getting stuck with focus on nothing in some cases.. */

	XSetInputFocus (dpy, no_focus_window, RevertToNone, last_event_time);
    }
}

/* Set flags in W relating to which window manager protocols are recognised
   by window W. */
void
get_window_protocols (Lisp_Window *w)
{
    Atom *prot;
    int n;
    w->does_wm_take_focus = 0;
    w->does_wm_delete_window = 0;
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
    if (w != 0 && WINDOW_IS_GONE_P (w))
	w = 0;
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
    return w;
}

void
install_window_frame (Lisp_Window *w)
{
    DB(("install_window_frame (%s)\n", rep_STR(w->name)));
    if (!w->reparented && w->frame != 0 && !WINDOW_IS_GONE_P (w))
    {
	XSetWindowAttributes wa;

	XSelectInput (dpy, w->frame, FRAME_EVENTS);

	before_local_map (w);
	XReparentWindow (dpy, w->id, w->frame, -w->frame_x, -w->frame_y);
	w->reparented = TRUE;
	after_local_map (w);
	restack_window (w);

	if (queued_focus_id == w->id)
	    queued_focus_id = w->frame;

	XAddToSaveSet (dpy, w->id);
	restack_frame_parts (w);
	reset_frame_parts (w);

	wa.win_gravity = StaticGravity;
	XChangeWindowAttributes (dpy, w->id, CWWinGravity, &wa);

	DB(("  reparented to %lx [%dx%d%+d%+d]\n",
	    w->frame, w->frame_width, w->frame_height,
	    w->frame_x, w->frame_y));
    }
}

void
remove_window_frame (Lisp_Window *w)
{
    DB(("remove_window_frame (%s)\n", rep_STR(w->name)));
    if (w->reparented && !WINDOW_IS_GONE_P (w))
    {
	XSetWindowAttributes wa;

	/* reparent the subwindow back to the root window */

	wa.win_gravity = w->attr.win_gravity;
	XChangeWindowAttributes (dpy, w->id, CWWinGravity, &wa);

	before_local_map (w);
	XReparentWindow (dpy, w->id, root_window, w->attr.x, w->attr.y);
	w->reparented = FALSE;
	after_local_map (w);
	restack_window (w);

	if (queued_focus_id == w->frame)
	    queued_focus_id = w->id;

	if (!w->mapped)
	    XRemoveFromSaveSet (dpy, w->id);
    }
}


static repv
text_prop_to_utf8 (XTextProperty *prop)
{
    repv rval = Qnil;
    if (prop->value && prop->nitems > 0)
    {
        char **list;
        int count;
        prop->nitems = strlen((char *) prop->value);
#ifdef X_HAVE_UTF8_STRING
        if (Xutf8TextPropertyToTextList (dpy, prop, &list, &count) >= Success)
        {
            if (count > 0)
                rval = rep_string_dup (list[0]);
            XFreeStringList (list);
        }
#else
        if (XmbTextPropertyToTextList (dpy, prop, &list, &count) >= Success)
        {
            if (count > 0) {
                gchar *ustr = g_locale_to_utf8(list[0], -1, NULL, NULL, NULL);
                if (ustr)
                {
                    rval = rep_string_dup (ustr);
                    g_free (ustr);
                }
            }
            XFreeStringList (list);
        }
#endif
    }
    return rval;
}


/* Queries X properties to get the window {icon,}name */
static void
get_window_name(Lisp_Window *w)
{
    XTextProperty prop;

    /* We only try to use the utf8 properties if our xlib supports them.
       Otherwise conversion would have to go via the current locale, which
       might lose some characters. */
#ifdef X_HAVE_UTF8_STRING
    if (XGetTextProperty (dpy, w->id, &prop, xa_wm_net_name))
        w->net_name = text_prop_to_utf8 (&prop);
    if (XGetTextProperty (dpy, w->id, &prop, xa_wm_net_icon_name))
        w->net_icon_name = text_prop_to_utf8 (&prop);
#endif

    if (w->net_name == Qnil && XGetWMName (dpy, w->id, &prop))
    {
        repv name = text_prop_to_utf8 (&prop);
        if (name != Qnil)
            w->name = name;
    }
    w->full_name = w->name;
  
    if (w->net_icon_name == Qnil && XGetWMIconName (dpy, w->id, &prop))
        w->icon_name = text_prop_to_utf8 (&prop);
    if (w->icon_name == Qnil)
        w->icon_name = w->name;
}


/* Add the top-level window ID to the manager's data structures.
 * This is called only from events.c -> map_request */
Lisp_Window *
add_window (Window id)
{
    Lisp_Window *w = rep_ALLOC_CELL(sizeof (Lisp_Window));
    if (w != 0)
    {
	rep_GC_root gc_win;
	repv win = rep_VAL(w);
	XWindowChanges xwc;
	unsigned int xwcm;
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
	w->icon_image = rep_NULL;
	w->name = rep_null_string ();
	w->net_name = Qnil;
	w->net_icon_name = Qnil;
	w->border_pixel = BlackPixel (dpy, screen_num);

        /* Don't garbage collect the window before we are done. */
        /* Note: must not return without rep_POPGC. */
	rep_PUSHGC(gc_win, win);

	/* have to put it somewhere until it finds the right place */
	insert_in_stacking_list_above_all (w);
	restack_window (w);

	/* ..now do the X11 stuff */

	XSelectInput (dpy, id, CLIENT_EVENTS);
	XGetWindowAttributes (dpy, id, &w->attr);
	DB(("  orig: width=%d height=%d x=%d y=%d\n",
	    w->attr.width, w->attr.height, w->attr.x, w->attr.y));
	w->old_border_width = w->attr.border_width;

	get_window_name(w);

	w->wmhints = XGetWMHints (dpy, id);
	if (!XGetWMNormalHints (dpy, w->id, &w->hints, &supplied))
	    w->hints.flags = 0;
	get_window_protocols (w);
	if (!XGetWMColormapWindows (dpy, w->id,
				    &w->cmap_windows, &w->n_cmap_windows))
	{
	    w->n_cmap_windows = 0;
	}

	{
	    /* Is the window shaped? */
	    int xws, yws, xbs, ybs;
	    unsigned int wws, hws, wbs, hbs;
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

	if (initialising)
	    Fwindow_put (rep_VAL (w), Qplaced, Qt);

	/* If the window requires to start as icon, then iconify it.
	 * It is better to be done before 'before_add_window_hook', where
	 * matching takes place, because in future, matcher can have an
	 * option to un-iconify, overrinding the application's request
	 * to iconify. 
	 */
	if (w->wmhints && w->wmhints->flags & StateHint
	    && w->wmhints->initial_state == IconicState)
	  {
	    DEFSTRING (iconify_mod, "sawfish.wm.state.iconify");
	    rep_call_lisp1 (module_symbol_value
			    (rep_VAL (&iconify_mod), Qiconify_window),
			    rep_VAL(w));
	  }
	
	/* Prevent hook call on non existing window */
	if (WINDOW_IS_GONE_P (w))
	{
		rep_POPGC;
		return 0;
	}
	
	/* ..then call the add-window-hook's.. */
	Fcall_window_hook (Qbefore_add_window_hook, rep_VAL(w), Qnil, Qnil);
	Fcall_window_hook (Qadd_window_hook, rep_VAL(w), Qnil, Qnil);

	/* In case the window disappeared during the hook call */
	if (!WINDOW_IS_GONE_P (w))
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

	if (!WINDOW_IS_GONE_P (w))
	{
            repv tem = Fwindow_get (rep_VAL(w), Qplaced, Qnil);
	    if (initialising || (tem && tem == Qnil))
	    {
		/* ..then the place-window-hook.. */
		Fcall_window_hook (Qplace_window_hook, rep_VAL(w), Qnil, Qor);
	    }
	}
	Fwindow_put (rep_VAL(w), Qplaced, Qt);

	if (!WINDOW_IS_GONE_P (w))
	    Fcall_window_hook (Qafter_add_window_hook, rep_VAL(w), Qnil, Qnil);

	if (!WINDOW_IS_GONE_P (w))
	{
	    /* Tell the window where it ended up.. */
	    send_synthetic_configure (w);
	}
        rep_POPGC;
    }
    return w;
}

/* Remove W from the managed windows. If DESTROYED is nil and
   the window is currently reparented by us, it will be reparented back to
   the root window */
void
remove_window (Lisp_Window *w, bool destroyed, bool from_error)
{
    DB(("remove_window (%s, %s)\n",
	rep_STR(w->name), destroyed ? "destroyed" : "not-destroyed"));

    if (w->id != 0)
    {
	if (!destroyed && !from_error)
	{
	    grab_window_events (w, FALSE);
	    remove_window_frame (w);

	    /* Restore original border width of the client */
	    XSetWindowBorderWidth (dpy, w->id, w->old_border_width);
	}

	if (!from_error)
	    destroy_window_frame (w, FALSE);

	if (!WINDOW_IS_GONE_P (w))
	    remove_from_stacking_list (w);

	if (!from_error)
	    focus_off_window (w);

	w->id = 0;
	pending_destroys++;

	/* gc will do the rest... */
    }
    else if (w->frame != 0 && !from_error)
	destroy_window_frame (w, FALSE);
}

void
fix_window_size (Lisp_Window *w)
{
    Fgrab_server ();
    if (w->frame != 0 && w->rebuild_frame != 0)
	w->rebuild_frame (w);
    else
	XResizeWindow (dpy, w->id, w->attr.width, w->attr.height);
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
	    if (WINDOW_IS_GONE_P (w) && !w->destroyed)
	    {
		w->destroyed = 1;
		Fcall_window_hook (Qdestroy_notify_hook,
				   rep_VAL(w), Qnil, Qnil);

		focus_off_window (w);

		/* gc may have reordered the list, so we have to start
		   at the beginning again.. */
		goto again;
	    }
	}
    }
    pending_destroys = 0;
}

/* Lisp functions */

DEFUN("window-get", Fwindow_get, Swindow_get,
      (repv win, repv prop, repv checker), rep_Subr3) /*
::doc:sawfish.wm.windows.subrs#window-get::
window-get WINDOW PROPERTY &optional CHECKER

Return the value of the property named PROPERTY (a symbol) of WINDOW.

Note that these are Lisp properties not X properties.

If the optional argument CHECKER is nil, then the return value is
nil, either when the property value is nil, or the property is absent.

If CHECKER is non-nil, than it returns CHECKER if the property
is unset.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, XWINDOWP);
    plist = VWIN(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop
	    || (!rep_SYMBOLP(prop)
		&& rep_value_cmp (rep_CAR(plist), prop) == 0))
	{
	    return rep_CAR(rep_CDR(plist));
	}
	plist = rep_CDR(rep_CDR(plist));
    }
    return checker;
}

DEFUN("map-window-properties", Fmap_window_properties,
      Smap_window_properties, (repv fun, repv win), rep_Subr2) /*
::doc:sawfish.wm.windows.subrs#map-window-properties::
map-window-properties FUNCTION WINDOW

Call (FUNCTION PROPERTY VALUE) for all Lisp properties set on window
object WINDOW.
::end:: */
{
    repv ret = Qnil, plist;
    rep_GC_root gc_plist, gc_fun;
    rep_DECLARE2 (win, XWINDOWP);
    plist = VWIN(win)->plist;
    rep_PUSHGC (gc_plist, plist);
    rep_PUSHGC (gc_fun, fun);
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	ret = rep_call_lisp2 (fun, rep_CAR (plist), rep_CADR (plist));
	if (ret == rep_NULL)
	    break;
	plist = rep_CDDR (plist);
    }
    rep_POPGC; rep_POPGC;
    return ret;
}

void
register_property_monitor (repv prop,
			   void (*callback) (Lisp_Window *, repv, repv, repv))
{
    struct prop_handler *ph = rep_alloc (sizeof (struct prop_handler));
    ph->next = prop_handlers;
    prop_handlers = ph;
    ph->prop = prop;
    ph->callback = callback;
}    

DEFUN("window-put", Fwindow_put, Swindow_put,
      (repv win, repv prop, repv val), rep_Subr3) /*
::doc:sawfish.wm.windows.subrs#window-put::
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
	if (rep_CAR(plist) == prop
	    || (!rep_SYMBOLP(prop)
		&& rep_value_cmp (rep_CAR(plist), prop) == 0))
	{
	    struct prop_handler *ph;
	    for (ph = prop_handlers; ph != 0; ph = ph->next)
	    {
		repv old = rep_CADR (plist);
		if (ph->prop == prop && old != val)
		    ph->callback (VWIN (win), prop, old, val);
	    }
	    rep_CADR(plist) = val;
	    return val;
	}
	plist = rep_CDDR(plist);
    }
    plist = Fcons(prop, Fcons(val, VWIN(win)->plist));
    if (plist != rep_NULL)
	VWIN(win)->plist = plist;
    return val;
}

DEFUN("window-remprop", Fwindow_remprop, Swindow_remprop,
      (repv win, repv prop), rep_Subr2) /*
::doc:sawfish.wm.windows.subrs#window-prop-del::
window-put WINDOW PROPERTY

Delete PROPERTY of WINDOW. Return t for success, nil if WINDOW
did not have PROPERTY.
::end:: */
{
    repv *pplist;
    rep_DECLARE1(win, XWINDOWP);
    pplist = &VWIN(win)->plist;
    while (rep_CONSP(*pplist) && rep_CONSP(rep_CDR(*pplist)))
    {
	if (rep_CAR(*pplist) == prop
	    || (!rep_SYMBOLP(prop)
		&& rep_value_cmp (rep_CAR(*pplist), prop) == 0))
	{
	    struct prop_handler *ph;
            repv old = rep_CADR(*pplist);
            if (old != Qnil)
                for (ph = prop_handlers; ph != 0; ph = ph->next)
                    if (ph->prop == prop)
                        ph->callback(VWIN (win), prop, old, Qnil);
            *pplist = rep_CDDR(*pplist);
	    return Qt;
	}
	pplist = &rep_CDDR(*pplist);
    }
    return Qnil;
}

DEFUN("window-plist", Fwindow_plist, Swindow_plist,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-plist::
window-plist WINDOW

Returns the property list of the window window which is of the form
(prop value prop value ...).

Do not attempt to change properties by modifying the property list in place.
Use window-put instead.
::end:: */
{
    rep_DECLARE1(win, XWINDOWP);
    return VWIN(win)->plist;
}

DEFUN("window-name", Fwindow_name, Swindow_name, (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-name::
window-name WINDOW

Return the name of window object WINDOW.
::end:: */
{
    Lisp_Window * w;
    rep_DECLARE1(win, WINDOWP);
    w = VWIN(win);
    return w->net_name != Qnil ? w->net_name : w->name;
}

DEFUN("window-full-name", Fwindow_full_name, Swindow_full_name,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-full-name::
window-full-name WINDOW

Return the full name of window object WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->full_name;
}

DEFUN("window-icon-name", Fwindow_icon_name, Swindow_icon_name,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-icon-name::
window-icon-name WINDOW

Return the name of window object WINDOW's icon.
::end:: */
{
    Lisp_Window * w;
    rep_DECLARE1(win, WINDOWP);
    w = VWIN(win);
    return w->net_icon_name != Qnil ? w->net_icon_name : w->icon_name;
}

DEFUN("window-mapped-p", Fwindow_mapped_p, Swindow_mapped_p,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-mapped-p::
window-mapped-p WINDOW

Return t if the client window associated with object WINDOW is mapped.
(This doesn't necessarily mean that it is visible.)
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->mapped ? Qt : Qnil;
}

DEFUN("window-frame", Fwindow_frame, Swindow_frame, (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-frame::
window-frame WINDOW

Return the frame object associated with WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->frame_style;
}

DEFUN("set-window-frame", Fset_window_frame, Sset_window_frame,
      (repv win, repv frame), rep_Subr2) /*
::doc:sawfish.wm.windows.subrs#set-window-frame::
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
::doc:sawfish.wm.windows.subrs#rebuild-frame::
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
::doc:sawfish.wm.windows.subrs#window-position::
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
::doc:sawfish.wm.windows.subrs#window-dimensions::
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
::doc:sawfish.wm.windows.subrs#window-frame-dimensions::
window-frame-dimensions WINDOW

Return (WIDTH . HEIGHT) defining the current dimensions of the frame
surrounding WINDOW.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->reparented)
    {
	return Fcons (rep_MAKE_INT(VWIN(win)->frame_width + 2*VWIN(win)->border_width),
		      rep_MAKE_INT(VWIN(win)->frame_height + 2*VWIN(win)->border_width));
    }
    else
	return Fwindow_dimensions (win);
}

DEFUN("window-frame-offset", Fwindow_frame_offset,
      Swindow_frame_offset, (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-frame-offset::
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
::doc:sawfish.wm.windows.subrs#windowp::
windowp ARG

Return t if ARG is a window object.
::end:: */
{
    return WINDOWP(win) ? Qt : Qnil;
}

DEFUN("set-input-focus", Fset_input_focus, Sset_input_focus,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#set-input-focus::
set-input-focus WINDOW

Set the input focus to WINDOW. If WINDOW is nil, then no window will
have the focus.
::end:: */
{
    if (win != Qnil && win != Qroot)
    {
	rep_DECLARE1(win, WINDOWP);
	focus_on_window (VWIN(win));
    }
    else
	focus_on_window (0);
    return win;
}

DEFUN("input-focus", Finput_focus, Sinput_focus, (void), rep_Subr0) /*
::doc:sawfish.wm.windows.subrs#input-focus::
input-focus

Return the window object that has the input focus, or nil if none does.
::end:: */
{
    return (focus_window == 0) ? Qnil : rep_VAL(focus_window);
}

DEFUN("window-wants-input-p", Fwindow_wants_input_p, Swindow_wants_input_p,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-wants-input-p::
window-wants-input-p WINDOW

Return t if the WINDOW wants focus in X sense, i.e. if it has hinted
that it would like to be given the input focus when applicable.

If unsure, use `window-really-wants-input-p' which also takes into
account `never-focus' window property.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->does_wm_take_focus)
	return Qt;
    else
	return window_input_hint_p (VWIN (win)) ? Qt : Qnil;
}

DEFUN("managed-windows", Fmanaged_windows, Smanaged_windows,
      (void), rep_Subr0) /*
::doc:sawfish.wm.windows.subrs#managed-windows::
managed-windows

Return a list of all known client window objects.
::end:: */
{
    repv list = Qnil;
    Lisp_Window *w = window_list;
    while (w != 0)
    {
	if (!WINDOW_IS_GONE_P (w))
	    list = Fcons (rep_VAL(w), list);
	w = w->next;
    }
    return list;
}

DEFUN("get-window-by-id", Fget_window_by_id, Sget_window_by_id,
      (repv id), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#get-window-by-id::
get-window-by-id ID

Return the window object associated with xid ID, or nil.
::end:: */
{
    Lisp_Window *w;
    rep_DECLARE1(id, rep_INTEGERP);
    w = find_window_by_id (rep_get_long_uint (id));
    return w ? rep_VAL(w) : Qnil;
}

DEFUN("stacking-order", Fstacking_order, Sstacking_order, (void), rep_Subr0) /*
::doc:sawfish.wm.windows.subrs#stacking-order::
stacking-order

Return a list of windows defining the current stacking order of all
client windows.
::end:: */
{
    return make_stacking_list ();
}

DEFUN("window-visibility", Fwindow_visibility, Swindow_visibility,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-visibility::
window-visibility WINDOW

Return a symbol defining the visibility of WINDOW. Possible returned
symbols are `fully-obscured', `partially-obscured' or `unobscured'.

This function is deprecated. Instead use `window-obscured' and
`stacking-visibility'.
::end:: */
{
    repv sym = Qnil;
    rep_DECLARE1(win, WINDOWP);
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

DEFUN("window-urgent-p", Fwindow_urgent_p, Swindow_urgent_p,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-urgent-p::
window-urgent-p WINDOW

Return true if the `Urgency' hint of the window associated with WINDOW
is set.
::end:: */
{
    rep_DECLARE1 (win, WINDOWP);
    return ((VWIN (win)->wmhints
	     && VWIN (win)->wmhints->flags & XUrgencyHint) ? Qt : Qnil);
}

DEFUN("window-shaped-p", Fwindow_shaped_p, Swindow_shaped_p,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-shaped-p::
window-shaped-p WINDOW

Return non-nil if WINDOW is shaped.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->shaped ? Qt : Qnil;
}

DEFUN("hide-window", Fhide_window, Shide_window, (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#hide-window::
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
::doc:sawfish.wm.windows.subrs#show-window::
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
::doc:sawfish.wm.windows.subrs#window-visible-p::
window-visible-p WINDOW

Return t if WINDOW is not hidden by `hide-window'. Notice there're
various cases where a window is not visible.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->visible ? Qt : Qnil;
}

DEFUN("window-framed-p", Fwindow_framed_p, Swindow_framed_p,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-framed-p::
window-framed-p WINDOW

Return t if WINDOW has been reparented to a frame window.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->reparented ? Qt : Qnil;
}

DEFUN("window-frame-id", Fwindow_frame_id, Swindow_frame_id,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-frame-id::
window-frame-id WINDOW

Return the numeric id of the framing window associated with object
WINDOW. Returns nil if the client window has no frame.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return VWIN(win)->frame ? rep_MAKE_INT (VWIN(win)->frame) : Qnil;
}

DEFUN("window-id", Fwindow_id, Swindow_id, (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-id::
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
::doc:sawfish.wm.windows.subrs#window-group-id::
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
::doc:sawfish.wm.windows.subrs#window-border-width::
window-border-width WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    return rep_MAKE_INT(VWIN(win)->border_width);
}

DEFUN("window-size-hints", Fwindow_size_hints, Swindow_size_hints,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-size-hints::
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

    /* Some sanity checking */
    if ((flags & PMinSize) 
	&& (hints->min_width < 0 || hints->min_height < 0))
	flags &= ~PMinSize;
    if ((flags & PMaxSize)
	&& (hints->max_width <= 0 || hints->max_height <= 0))
	flags &= ~PMaxSize;
    if ((flags & PResizeInc)
	&& (hints->width_inc <= 0 || hints->width_inc <= 0))
	flags &= ~PResizeInc;
    if ((flags & PBaseSize)
	&& (hints->base_width <= 0 || hints->base_height <= 0))
	flags &= ~PBaseSize;

    if (flags & PMinSize)
    {
	ret = Fcons (Fcons (Qmin_width, rep_MAKE_INT(MAX(hints->min_width,1))),
		     Fcons (Fcons (Qmin_height, rep_MAKE_INT(MAX(hints->min_height, 1))), ret));
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
::doc:sawfish.wm.windows.subrs#call-window-hook::
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
    tem = Fwindow_get (win, hook, Qnil);
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

DEFUN("window-icon-image", Fwindow_icon_image,
      Swindow_icon_image, (repv win), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#window-icon-image::
window-icon-image WINDOW

Returns an image object representing the icon currently associated with
WINDOW. Returns the symbol `nil' if no such image.
::end:: */
{
   rep_DECLARE1 (win, WINDOWP);

   if (VWIN (win)->icon_image == rep_NULL)
   {
       Window pixmap_id = 0, mask_id = 0;

       if (VWIN (win)->wmhints != 0)
       {
	   if (VWIN (win)->wmhints->flags & IconPixmapHint
	       && VWIN (win)->wmhints->icon_pixmap != 0)
	   {
	       pixmap_id = VWIN (win)->wmhints->icon_pixmap;
	   }

	   if (VWIN (win)->wmhints->flags & IconMaskHint
	       && VWIN (win)->wmhints->icon_mask != 0)
	   {
	       mask_id = VWIN (win)->wmhints->icon_mask;
	   }
       }

       if (pixmap_id == 0 && !WINDOW_IS_GONE_P (VWIN (win)))
       {
	   Atom actual_type;
	   int actual_format;
	   unsigned long nitems, bytes_after;
	   union {
	       unsigned long *l;
	       unsigned char *c;
	   } data;

	   static Atom kwm_win_icon = 0;

	   if (kwm_win_icon == 0)
	       kwm_win_icon = XInternAtom (dpy, "KWM_WIN_ICON", False);

	   data.l = 0;
	   if (XGetWindowProperty (dpy, VWIN (win)->id, kwm_win_icon,
				   0, 2, False, kwm_win_icon,
				   &actual_type, &actual_format,
				   &nitems, &bytes_after,
				   &data.c) == Success
	       && actual_type == kwm_win_icon
	       && bytes_after == 0)
	   {
	       pixmap_id = data.l[0];
	       mask_id = data.l[1];
	   }
	   if (data.l != 0)
	       XFree (data.l);
       }

       VWIN (win)->icon_image = Qnil;

       if (pixmap_id != 0)
       {
	   VWIN (win)->icon_image = (Fmake_image_from_x_drawable
				     (rep_MAKE_INT (pixmap_id),
				      mask_id ? rep_MAKE_INT (mask_id) : Qnil));
       }
   }

   return VWIN (win)->icon_image;
}

DEFUN ("map-windows", Fmap_windows, Smap_windows, (repv fun), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#map-windows::
map-windows FUN

Map the single-parameter function FUN over all existing windows.
::end:: */
{
    repv w;
    rep_GC_root gc_fun, gc_w;
    repv ret = Qnil;

    rep_PUSHGC (gc_fun, fun);
    rep_PUSHGC (gc_w, w);
    for (w = rep_VAL (window_list); w != rep_NULL; w = rep_VAL (VWIN(w)->next))
    {
	if (!WINDOW_IS_GONE_P (VWIN (w)))
	{
	    ret = rep_call_lisp1 (fun, w);
	    if (ret == rep_NULL)
		break;
	}
    }
    rep_POPGC; rep_POPGC;
    return ret;
}

DEFUN ("filter-windows", Ffilter_windows,
       Sfilter_windows, (repv pred), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#filter-windows::
filter-windows PRED

Return the list of windows that match the predicate function PRED.
::end:: */
{
    repv w, output = Qnil, *ptr = &output;
    rep_GC_root gc_pred, gc_w, gc_output;

    rep_PUSHGC(gc_pred, pred);
    rep_PUSHGC(gc_w, w);
    rep_PUSHGC(gc_output, output);
    for (w = rep_VAL (window_list); w != rep_NULL; w = rep_VAL (VWIN(w)->next))
    {
	if (!WINDOW_IS_GONE_P (VWIN (w)))
	{
	    repv tem = rep_call_lisp1 (pred, w);
	    if (tem == rep_NULL)
	    {
		output = rep_NULL;
		break;
	    }
	    if (tem != Qnil)
	    {
		*ptr = Fcons (w, Qnil);
		ptr = rep_CDRLOC (*ptr);
	    }
	}
    }
    rep_POPGC; rep_POPGC; rep_POPGC;
    return output;
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
    mark_frame_parts (VWIN(win));
    rep_MARKVAL(VWIN(win)->name);
    rep_MARKVAL(VWIN(win)->full_name);
    rep_MARKVAL(VWIN(win)->icon_name);
    rep_MARKVAL(VWIN(win)->net_name);
    rep_MARKVAL(VWIN(win)->net_icon_name);
    rep_MARKVAL(VWIN(win)->icon_image);
}

static void
window_mark_type (void)
{
    Lisp_Window *w;
    struct prop_handler *ph;
    for (w = window_list; w != 0; w = w->next)
    {
	if (!WINDOW_IS_GONE_P (w) || !w->destroyed)
	    rep_MARKVAL(rep_VAL(w));
    }
    for (ph = prop_handlers; ph != 0; ph = ph->next)
	rep_MARKVAL (ph->prop);
    rep_MARKVAL (rep_VAL (focus_window));
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
	    assert (!window_in_stacking_list_p (w));
	    destroy_window_frame (w, FALSE);
	    if (w->wmhints != 0)
		XFree (w->wmhints);
	    if (w->n_cmap_windows > 0)
		XFree (w->cmap_windows);
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
    Window root, parent, *children, focus;
    unsigned int nchildren, i;
    int revert_to;

    Fgrab_server ();

    XGetInputFocus (dpy, &focus, &revert_to);
    if (focus == PointerRoot)
    {
    	Window root, child;
	Bool found;
	int rx, ry, wx, wy;
	unsigned mask;

	found = XQueryPointer (dpy, DefaultRootWindow(dpy), &root,
			       &child, &rx, &ry, &wx, &wy, &mask);
	if (!found)
	{
	    found = XQueryPointer (dpy, root, &root, &child,
				   &rx, &ry, &wx, &wy, &mask);
	}
	focus = child;
    }

    XQueryTree (dpy, root_window, &root, &parent, &children, &nchildren);
    initialising = TRUE;
    for (i = 0; i < nchildren; i++)
    {
	if (mapped_not_override_p (children[i]))
	{
	    XEvent fake;
	    Lisp_Window *w;
	    fake.xmaprequest.window = children[i];
	    /* Make sure the window is initially unmapped. We expect to
	       get map-notify events when we later remap it.. #67601 */
	    XUnmapWindow (dpy, children[i]);
	    map_request (&fake);
	    w = find_window_by_id (children[i]);
	}
    }
    initialising = FALSE;
    if (nchildren > 0)
	XFree (children);

    /* Try to keep the current focus state. */
    focus_on_window (0);
    if (focus != None)
    {
	Lisp_Window *w = find_window_by_id (focus);
	if (w != 0)
	    focus_on_window (w);
    }

    Fungrab_server ();
    Fcall_hook (Qafter_initialization_hook, Qnil, Qnil);
}

void
windows_init (void)
{
    repv tem;
    window_type = rep_register_new_type ("window", window_cmp, window_prin,
					 window_prin, window_sweep,
					 window_mark, window_mark_type,
					 0, 0, 0, 0, 0, 0);

    tem = rep_push_structure ("sawfish.wm.windows.subrs");
    rep_ADD_SUBR(Swindow_get);
    rep_ADD_SUBR(Smap_window_properties);
    rep_ADD_SUBR(Swindow_put);
    rep_ADD_SUBR(Swindow_remprop);
    rep_ADD_SUBR(Swindow_plist);
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
    rep_ADD_SUBR(Swindow_urgent_p);
    rep_ADD_SUBR(Swindow_shaped_p);
    rep_ADD_SUBR(Shide_window);
    rep_ADD_SUBR(Sshow_window);
    rep_ADD_SUBR(Swindow_visible_p);
    rep_ADD_SUBR(Swindow_framed_p);
    rep_ADD_SUBR(Swindow_frame_id);
    rep_ADD_SUBR(Swindow_id);
    rep_ADD_SUBR(Swindow_group_id);
    rep_ADD_SUBR(Swindow_size_hints);
    rep_ADD_SUBR(Scall_window_hook);
    rep_ADD_SUBR(Swindow_border_width);
    rep_ADD_SUBR(Swindow_icon_image);
    rep_ADD_SUBR(Smap_windows);
    rep_ADD_SUBR(Sfilter_windows);
    rep_pop_structure (tem);

    rep_INTERN_SPECIAL(before_add_window_hook);
    rep_INTERN_SPECIAL(add_window_hook);
    rep_INTERN_SPECIAL(after_add_window_hook);
    rep_INTERN_SPECIAL(place_window_hook);
    rep_INTERN(placed);
    rep_INTERN_SPECIAL(after_framing_hook);
    rep_INTERN_SPECIAL(after_initialization_hook);
    rep_INTERN_SPECIAL(remove_window_hook);

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
    repv next;
    rep_GC_root gc_next;
    rep_PUSHGC (gc_next, next);
    while (w != 0)
    {
	next = rep_VAL (w->next);
	Fcall_window_hook (Qremove_window_hook, rep_VAL (w), Qnil, Qnil);
	remove_window (w, FALSE, FALSE);
	w = VWIN (next);
    }
}
