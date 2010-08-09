/* Handling the (current) key events(s). */

#include "sawfish.h"
#include "keys.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

extern unsigned long current_event[2], last_event[2];
extern repv lookup_event_binding (unsigned long code, unsigned long mods,
                                  repv context_map);
extern repv search_keymap(repv km, unsigned long code, unsigned long mods,
                          bool (*callback)(repv key));
extern bool compare_events (unsigned long code1, unsigned long mods1,
                            unsigned long code2, unsigned long mods2);
extern bool translate_event_to_x_key (repv ev, unsigned int *keycode,
                                      unsigned int *state);
extern unsigned int translate_event_to_x_button (repv ev, unsigned int *button,
                                                 unsigned int *state);

extern Time last_click;
DEFSYM(async_pointer, "async-pointer");
DEFSYM(async_keyboard, "async-keyboard");
DEFSYM(sync_pointer, "sync-pointer");
DEFSYM(sync_keyboard, "sync-keyboard");
DEFSYM(replay_pointer, "replay-pointer");
DEFSYM(replay_keyboard, "replay-keyboard");
DEFSYM(sync_both, "sync-both");
DEFSYM(async_both, "async-both");

DEFSTRING(not_in_handler, "Not in event handler");
DEFUN("current-event-string", Fcurrent_event_string, Scurrent_event_string, (void), rep_Subr0) /*
::doc:sawfish.wm.events#current-event-string::
current-event-string

Returns the string which would have been inserted by the current event if
a Lisp function hadn't been called instead.
::end:: */
{
    KeySym ks;
    char buf[256];
    int len;

    if(current_x_event == 0)
	return Fsignal(Qerror, rep_LIST_1(rep_VAL(&not_in_handler)));

    len = XLookupString(&current_x_event->xkey,
                        buf, sizeof (buf) - 1, &ks, NULL);
    if(len > 0)
	return rep_string_dupn(buf, len);
    else
	return rep_null_string();
}

DEFUN("current-event", Fcurrent_event, Scurrent_event, (void), rep_Subr0) /*
::doc:sawfish.wm.events#current-event::
current-event

Return the event which caused the current command to be invoked.
::end:: */
{
    if(current_event[1])
	return MAKE_EVENT(rep_MAKE_INT(current_event[0]),
			  rep_MAKE_INT(current_event[1]));
    else
	return Qnil;
}

DEFUN("proxy-current-event", Fproxy_current_event, Sproxy_current_event,
      (repv win, repv mask, repv prop), rep_Subr3) /*
::doc:sawfish.wm.events#proxy-current-event::
proxy-current-event WINDOW [MASK] [PROPAGATE]

Send the current X event to WINDOW, either a window object, a numeric
window id, or the symbol `root'. If a ButtonPress event the pointer
grab will be released first.
::end:: */
{
    Window w = x_win_from_arg (win);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);

    if (current_x_event != 0)
    {
	long e_mask = (rep_INTP(mask) ? rep_INT(mask)
		       : get_event_mask (current_x_event->type));
	if (current_x_event->type == ButtonPress)
	    ungrab_pointer ();
	XSendEvent (dpy, w, prop == Qnil ? False : True,
		    e_mask, current_x_event);
	return Qt;
    }
    else
	return Qnil;
}

DEFUN("allow-events", Fallow_events, Sallow_events, (repv mode), rep_Subr1) /*
::doc:sawfish.wm.events#allow-events::
allow-events MODE
::end:: */
{
    int x_mode;
    if (mode == Qasync_pointer)
	x_mode = AsyncPointer;
    else if (mode == Qasync_keyboard)
	x_mode = AsyncKeyboard;
    else if (mode == Qsync_pointer)
	x_mode = SyncPointer;
    else if (mode == Qsync_keyboard)
	x_mode = SyncKeyboard;
    else if (mode == Qreplay_pointer)
	x_mode = ReplayPointer;
    else if (mode == Qreplay_keyboard)
	x_mode = ReplayKeyboard;
    else if (mode == Qsync_both)
	x_mode = SyncBoth;
    else if (mode == Qasync_both)
	x_mode = AsyncBoth;
    else
	return rep_signal_arg_error (mode, 1);

    XAllowEvents (dpy, x_mode, last_event_time);
    return Qt;
}

DEFUN("last-event", Flast_event, Slast_event, (void), rep_Subr0) /*
::doc:sawfish.wm.events#last-event::
last-event

Return the previous event which occurred.
::end:: */
{
    if(last_event[1])
	return MAKE_EVENT(rep_MAKE_INT(last_event[0]),
			  rep_MAKE_INT(last_event[1]));
    else
	return Qnil;
}

DEFUN("event-name", Fevent_name, Sevent_name, (repv ev), rep_Subr1) /*
::doc:sawfish.wm.events#event-name::
event-name EVENT

Returns a string naming the event EVENT.
::end:: */
{
    char buf[256];
    if(!EVENTP(ev))
	return rep_signal_arg_error(ev, 1);

    if(lookup_event_name(buf, rep_INT(EVENT_CODE(ev)),
			 rep_INT(EVENT_MODS(ev))))
    {
	return rep_string_dup(buf);
    }
    else
	return Qnil;
}

DEFUN("lookup-event", Flookup_event, Slookup_event, (repv name), rep_Subr1) /*
::doc:sawfish.wm.events#lookup-event::
lookup-event EVENT-NAME

Return the event whose name is EVENT-NAME.
::end:: */
{
    unsigned long code, mods;
    rep_DECLARE1(name, rep_STRINGP);

    if(lookup_event(&code, &mods, rep_STR(name)))
	return MAKE_EVENT(rep_MAKE_INT(code), rep_MAKE_INT(mods));
    else
	return Qnil;
}

DEFUN("lookup-event-binding", Flookup_event_binding, Slookup_event_binding, (repv ev), rep_Subr1) /*
::doc:sawfish.wm.events#lookup-event-binding::
lookup-event-binding EVENT

Return the command currently associated with the event EVENT.

Note that `currently associated' means that the currently active set of
keymaps is used to resolve the binding. This means the window and
frame-part that received the current event for pointer events, or the
currently focused window for keyboard events.
::end:: */
{
    repv res, context = Qnil;
    unsigned long code, mods;

    if(!EVENTP(ev))
	return(rep_signal_arg_error(ev, 1));

    code = rep_INT(EVENT_CODE(ev));
    mods = rep_INT(EVENT_MODS(ev));

    if (mods & EV_TYPE_MOUSE)
    {
	/* look for a context map */
	if (clicked_frame_part != 0 && clicked_frame_part->clicked)
	    context = get_keymap_for_frame_part (clicked_frame_part);
    }

    res = lookup_event_binding(code, mods, context);
    return res ? res : Qnil;
}

DEFUN("search-keymap", Fsearch_keymap, Ssearch_keymap,
      (repv ev, repv km), rep_Subr2) /*
::doc:sawfish.wm.events#search-keymap::
search-keymap EVENT KEYMAP

Return the (COMMAND . EVENT) binding of EVENT in KEYMAP, or nil.
::end:: */
{
    repv res;
    rep_DECLARE1(ev, EVENTP);
    res = search_keymap(km, rep_INT(EVENT_CODE(ev)),
			rep_INT(EVENT_MODS(ev)), 0);
    return res ? res : Qnil;
}

DEFUN("x-lookup-keysym", Fx_lookup_keysym,
      Sx_lookup_keysym, (repv name), rep_Subr1) /*
::doc:sawfish.wm.events#x-lookup-keysym::
x-lookup-keysym NAME

Return the X11 keysym (an integer), named by the Lisp symbol NAME.
::end:: */
{
    KeySym sym;
    rep_DECLARE1(name, rep_SYMBOLP);
    sym = XStringToKeysym (rep_STR(rep_SYM(name)->name));
    return sym == NoSymbol ? Qnil : rep_MAKE_INT (sym);
}

DEFUN("x-keysym-name", Fx_keysym_name, Sx_keysym_name, (repv ks), rep_Subr1) /*
::doc:sawfish.wm.events#x-keysym-name::
x-keysym-name KEYSYM

Return the Lisp symbol naming the X11 keysym represented by the integer
KEYSYM.
::end:: */
{
    char *id;
    rep_DECLARE1(ks, rep_INTP);
    id = XKeysymToString (rep_INT(ks));
    return !id ? Qnil : Fintern (rep_string_dup (id), rep_obarray);
}

DEFUN("eventp", Feventp, Seventp, (repv arg), rep_Subr1) /*
::doc:sawfish.wm.events#eventp::
eventp ARG

Returns t if the ARG is an input event.
::end:: */
{
    return EVENTP(arg) ? Qt : Qnil;
}

DEFUN("event-match", Fevent_match,
      Sevent_match, (repv ev1, repv ev2), rep_Subr2) /*
::doc:sawfish.wm.events#event-match::
event-match EVENT1 EVENT2
::end:: */
{
    rep_DECLARE1 (ev1, EVENTP);
    rep_DECLARE2 (ev2, EVENTP);

    return (compare_events (rep_INT (EVENT_CODE (ev1)),
			    rep_INT (EVENT_MODS (ev1)),
			    rep_INT (EVENT_CODE (ev2)),
			    rep_INT (EVENT_MODS (ev2)))
	    || compare_events (rep_INT (EVENT_CODE (ev2)),
			       rep_INT (EVENT_MODS (ev2)),
			       rep_INT (EVENT_CODE (ev1)),
			       rep_INT (EVENT_MODS (ev1)))) ? Qt : Qnil;
}

DEFUN("forget-button-press", Fforget_button_press,
      Sforget_button_press, (void), rep_Subr0)
{
    last_click = 0;
    return Qt;
}

/* Find the bottom-most window below TOP containing (X,Y) that selects
   for button events */
static Window
window_getting_button_event(Window top, int x, int y)
{
    Window w = top, w2 = w, child;
    XWindowAttributes wa;

    /* Find the bottom-most child of W containing (X,Y) */
    do {
	XTranslateCoordinates (dpy, w, w2, x, y, &x, &y, &child);
	w = w2;
	if (child)
	    w2 = child;
    } while (child);

    /* Then work up until TOP is reached, or a window selecting
       button events is found */
again:
    XGetWindowAttributes (dpy, w, &wa);
    if (w != top
	&& !(wa.all_event_masks & (ButtonPressMask | ButtonReleaseMask)))
    {
	Window d1, *d2, parent;
	unsigned int d3;
	    
	XQueryTree (dpy, w, &d1, &parent, &d2, &d3);
	if (d2 != 0)
	    XFree (d2);
	if (parent != 0)
	{
	    w = parent;
	    goto again;
	}
    }

    return w;
}

DEFUN("synthesize-event", Fsynthesize_event, Ssynthesize_event,
      (repv event, repv win, repv propagate), rep_Subr3) /*
::doc:sawfish.wm.events#synthesize-event::
synthesize-event EVENT WINDOW [PROPAGATE]
::end:: */
{
    XEvent ev;
    repv ptr = Fquery_pointer (Qnil);
    Window w = x_win_from_arg (win);
    Window child, dummy;
    int x_offset, y_offset;

    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);
    if (rep_STRINGP (event))
	event = Flookup_event (event);
    if (!event || !ptr)
	return rep_NULL;
    rep_DECLARE (1, event, EVENTP (event));
    ev.xany.display = dpy;
    ev.xany.window = w;

    if (WINDOWP(win))
    {
	x_offset = -VWIN(win)->attr.x;
	y_offset = -VWIN(win)->attr.y;
	if (VWIN(win)->reparented) 
	{
	    x_offset += VWIN(win)->frame_x;
	    y_offset += VWIN(win)->frame_y;
	}
    }
    else
	x_offset = y_offset = 0;

    switch (rep_INT(EVENT_MODS(event)) & EV_TYPE_MASK)
    {
	unsigned int mask;

    case EV_TYPE_KEY:
	if (!translate_event_to_x_key (event, &ev.xkey.keycode,
				       &ev.xkey.state))
	{
	    return rep_signal_arg_error (event, 1);
	}
	ev.xkey.root = root_window;
	ev.xkey.subwindow = 0;
	ev.xkey.time = last_event_time;
	ev.xkey.x_root = rep_INT (rep_CAR (ptr));
	ev.xkey.y_root = rep_INT (rep_CDR (ptr));
	ev.xkey.x = (ev.xkey.x_root + x_offset);
	ev.xkey.y = (ev.xkey.y_root + y_offset);
	ev.xkey.same_screen = True;
	ev.xany.type = KeyPress;
	XSendEvent (dpy, w, propagate != Qnil, KeyPressMask, &ev);
	ev.xany.type = KeyRelease;
	XSendEvent (dpy, w, propagate != Qnil, KeyReleaseMask, &ev);
	break;

    case EV_TYPE_MOUSE:
	mask = translate_event_to_x_button (event, &ev.xbutton.button,
					    &ev.xbutton.state);
	if (!mask || ev.xbutton.button == 0)
	    return rep_signal_arg_error (event, 1);

	ev.xbutton.root = root_window;
	ev.xbutton.subwindow = 0;
	ev.xbutton.time = last_event_time;
	ev.xbutton.x = rep_INT (rep_CAR (ptr)) + x_offset;
	ev.xbutton.y = rep_INT (rep_CDR (ptr)) + y_offset;
	XTranslateCoordinates (dpy, w, root_window,
			       ev.xbutton.x, ev.xbutton.y,
			       &ev.xbutton.x_root, &ev.xbutton.y_root,
			       &dummy);
	child = window_getting_button_event (w, ev.xbutton.x, ev.xbutton.y);
	XTranslateCoordinates (dpy, w, child,
			       ev.xbutton.x, ev.xbutton.y,
			       &ev.xbutton.x, &ev.xbutton.y,
			       &dummy);
	ev.xbutton.same_screen = True;
	ev.xbutton.window = child;
	ev.xany.type = ButtonPress;
	XSendEvent (dpy, child, propagate != Qnil, ButtonPressMask, &ev);
	ev.xany.type = ButtonRelease;
	ev.xbutton.state |= mask;
	XSendEvent (dpy, child, propagate != Qnil, ButtonReleaseMask, &ev);
	break;

    default:
	return rep_signal_arg_error (event, 1);
    }
    return Qt;
}

void
key_event_init(void)
{
    repv tem;

    tem = rep_push_structure ("sawfish.wm.events");

    rep_ADD_SUBR(Scurrent_event_string);
    rep_ADD_SUBR(Scurrent_event);
    rep_ADD_SUBR(Sproxy_current_event);
    rep_ADD_SUBR(Sallow_events);
    rep_ADD_SUBR(Slast_event);
    rep_ADD_SUBR(Sevent_name);

    rep_ADD_SUBR(Slookup_event);
    rep_ADD_SUBR(Slookup_event_binding);
    rep_ADD_SUBR(Ssearch_keymap);
    rep_ADD_SUBR(Seventp);

    rep_ADD_SUBR(Sevent_match);
    rep_ADD_SUBR(Sforget_button_press);
    rep_ADD_SUBR(Sx_lookup_keysym);
    rep_ADD_SUBR(Sx_keysym_name);
    rep_ADD_SUBR(Ssynthesize_event);
    rep_pop_structure (tem);

    /* now, what structure ?? */
    rep_INTERN(async_pointer);
    rep_INTERN(async_keyboard);
    rep_INTERN(sync_pointer);
    rep_INTERN(sync_keyboard);
    rep_INTERN(replay_pointer);
    rep_INTERN(replay_keyboard);
    rep_INTERN(sync_both);
    rep_INTERN(async_both);

}
