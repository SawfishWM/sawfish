/* keys.c -- Key binding and evaluating (this should be called events.c)
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
#include "keys.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

/* max number of milliseconds between successive-clicks */
#define DEFAULT_DOUBLE_CLICK_TIME 250

/* current_event holds the event we're processing (or 0s), last_event
   contains the previously processed event.  */
static unsigned long current_event[2], last_event[2];

/* print_prefix means echo all events upto the end of the key-sequence.
   printed_this_prefix says the last event has been echoed. */
static bool print_prefix, printed_this_prefix;

/* Buffer holding the events making this key-sequence. */
#define EVENT_BUFSIZ 20
static unsigned long event_buf[EVENT_BUFSIZ]; /* one event = (code,mods) */
static int event_index;

/* Data for testing double-clicks */
static Time last_click;
static unsigned long last_click_button;
static int click_count;

/* These control which types of keyboard events we actually evaluate */
DEFSYM(eval_modifier_events, "eval-modifier-events");
DEFSYM(eval_key_release_events, "eval-key-release-events");

DEFSYM(global_keymap, "global-keymap");
DEFSYM(root_window_keymap, "root-window-keymap");
DEFSYM(override_keymap, "override-keymap");
DEFSYM(unbound_key_hook, "unbound-key-hook");
DEFSYM(keymap, "keymap");

DEFSYM(this_command, "this-command");
DEFSYM(last_command, "last-command");
DEFSYM(prefix_arg, "prefix-arg");
DEFSYM(current_prefix_arg, "current-prefix-arg");

DEFSYM(async_pointer, "async-pointer");
DEFSYM(async_keyboard, "async-keyboard");
DEFSYM(sync_pointer, "sync-pointer");
DEFSYM(sync_keyboard, "sync-keyboard");
DEFSYM(replay_pointer, "replay-pointer");
DEFSYM(replay_keyboard, "replay-keyboard");
DEFSYM(sync_both, "sync-both");
DEFSYM(async_both, "async-both");

DEFSYM(display_message, "display-message");
DEFSYM(call_command, "call-command");

static repv next_keymap_path;

DEFSYM(multi_click_delay, "multi-click-delay");

/* The X modifiers being used for Meta, Alt, Hyper, Super */
static unsigned long meta_mod, alt_mod, hyper_mod, super_mod;

/* The user-customizable modifier; used for default key bindings. This
   shouldn't include any bits that don't have a fixed meaning. */
static unsigned long wm_mod = EV_MOD_META;

/* The X modifiers bound to the Num_Lock and Scroll_Lock keysyms */
static unsigned long num_lock_mod, scroll_lock_mod;

DEFSYM(meta_keysyms, "meta-keysyms");
DEFSYM(alt_keysyms, "alt-keysyms");
DEFSYM(hyper_keysyms, "hyper-keysyms");
DEFSYM(super_keysyms, "super-keysyms");

static void grab_keymap_event (repv km, long code, long mods, bool grab);
static void grab_all_keylist_events (repv map, bool grab);

#ifdef HAVE_NINE_MOUSEBUTTONS
static int all_buttons[9] = { Button1, Button2, Button3, Button4, Button5, Button6, Button7, Button8, Button9 };
#else
static int all_buttons[5] = { Button1, Button2, Button3, Button4, Button5 };
#endif

/* locks: currently LockMask, num_lock, and scroll_lock */
static int total_lock_combs, all_lock_mask;
static int all_lock_combs[2*2*2];

/* Translate from X events to Lisp events */

static unsigned long
direct_modifiers (unsigned long mods)
{
    /* Do this first, since it may contain other indirect mods */
    if (wm_mod != 0 && (mods & EV_MOD_WM))
	mods = (mods & ~EV_MOD_WM) | wm_mod;

    if (meta_mod != 0 && (mods & EV_MOD_META))
	mods = (mods & ~EV_MOD_META) | meta_mod;
    if (alt_mod != 0 && (mods & EV_MOD_ALT))
	mods = (mods & ~EV_MOD_ALT) | alt_mod;
    if (hyper_mod != 0 && (mods & EV_MOD_HYPER))
	mods = (mods & ~EV_MOD_HYPER) | hyper_mod;
    if (super_mod != 0 && (mods & EV_MOD_SUPER))
	mods = (mods & ~EV_MOD_SUPER) | super_mod;

    return mods;
}

static unsigned long
indirect_modifiers (unsigned long mods)
{
    if(mods & meta_mod)
	mods = (mods & ~meta_mod) | EV_MOD_META;
    if(mods & alt_mod)
	mods = (mods & ~alt_mod) | EV_MOD_ALT;
    if(mods & hyper_mod)
	mods = (mods & ~hyper_mod) | EV_MOD_HYPER;
    if(mods & super_mod)
	mods = (mods & ~super_mod) | EV_MOD_SUPER;

    return mods;
}

/* Translate the X key or button event XEV to *CODE and *MODS */
static bool
translate_event(unsigned long *code, unsigned long *mods, XEvent *xev)
{
    repv multi_click_delay;
    int delay;

    bool ret = FALSE;
    switch(xev->type)
    {
    case KeyRelease:
	if (global_symbol_value (Qeval_key_release_events) == Qnil)
	    break;
	/* FALL THROUGH */

    case KeyPress:
	*mods = xev->xkey.state & ~all_lock_mask;
	if (xev->type == KeyRelease)
	    *mods |= EV_MOD_RELEASE;
	if(*mods & ShiftMask)
	{
	    KeySym normal, shifted;
	    normal = XKeycodeToKeysym (dpy, xev->xkey.keycode, 0);
	    shifted = XKeycodeToKeysym (dpy, xev->xkey.keycode, 1);

	    /* Some keys don't have keysym at index 1, if not treat it as
	       normal keysym shifted.

	       But if the keysym at index 1 is the same as that in index
	       0, then preserve the shift modifier */

	    if (shifted == NoSymbol)
		*code = normal;
	    else
	    {
		*code = shifted;
		if (shifted != normal)
		    *mods &= ~ShiftMask;
	    }
	}
	else
	    *code = XKeycodeToKeysym(xev->xany.display, xev->xkey.keycode, 0);
	if(*code != NoSymbol
	   && (!IsModifierKey (*code)
	       || global_symbol_value (Qeval_modifier_events) == Qt))
	{
	    *mods |= EV_TYPE_KEY;
	    ret = TRUE;
	}
	break;

    case ButtonPress:

	if(xev->xbutton.button == last_click_button)
        {
            multi_click_delay = global_symbol_value (Qmulti_click_delay);
            if (rep_INTP(multi_click_delay))
                delay = rep_INT(multi_click_delay);
            else
                delay = DEFAULT_DOUBLE_CLICK_TIME;
  
            if (xev->xbutton.time < (last_click + delay))
            {
                click_count++;
            }
            else
                click_count = 1;
        }
	else
	    click_count = 1;

	switch (click_count)
	{
	case 2:
	    *code = EV_CODE_MOUSE_CLICK2;
	    break;

	case 3:
	    *code = EV_CODE_MOUSE_CLICK3;
	    break;

	default:
	    *code = EV_CODE_MOUSE_CLICK1;
	}
	last_click = xev->xbutton.time;
	last_click_button = xev->xbutton.button;
	goto button;

    case ButtonRelease:
	switch (click_count)
	{
	case 2:
	    *code = EV_CODE_MOUSE_UP2;
	    break;

	case 3:
	    *code = EV_CODE_MOUSE_UP3;
	    break;

	default:
	    *code = EV_CODE_MOUSE_UP1;
	}

    button:
	*mods = xev->xbutton.state & ~all_lock_mask;
	*mods |= EV_TYPE_MOUSE;
	switch(xev->xbutton.button)
	{
	case Button1:
	    *mods |= Button1Mask;
	    break;
	case Button2:
	    *mods |= Button2Mask;
	    break;
	case Button3:
	    *mods |= Button3Mask;
	    break;
	case Button4:
	    *mods |= Button4Mask;
	    break;
	case Button5:
	    *mods |= Button5Mask;
	    break;
#ifdef HAVE_NINE_MOUSEBUTTONS
	case Button6:
	    *mods |= Button6Mask;
	    break;
	case Button7:
	    *mods |= Button7Mask;
	    break;
	case Button8:
	    *mods |= Button8Mask;
	    break;
	case Button9:
	    *mods |= Button9Mask;
	    break;
#endif
	}
	ret = TRUE;
	break;

    case MotionNotify:
	*code = EV_CODE_MOUSE_MOVE;
	*mods = xev->xmotion.state & ~all_lock_mask;
	*mods |= EV_TYPE_MOUSE;
	ret = TRUE;
	break;
    }

    if (ret)
	*mods = indirect_modifiers (*mods);

    return ret;
}

/* Translate the Lisp key event EV to X keycode *KEYCODE and modifier
   mask *STATE, returning true if successful. */
static bool
translate_event_to_x_key (repv ev, unsigned int *keycode, unsigned int *state)
{
    if (rep_INT(EVENT_MODS(ev)) & EV_TYPE_KEY)
    {
	unsigned int s = rep_INT(EVENT_MODS(ev)) & EV_MOD_MASK;
	KeySym sym = rep_INT(EVENT_CODE(ev));
	KeySym normal, shifted;
	unsigned int k = XKeysymToKeycode (dpy, sym);

	if (k == 0)
	    return FALSE;

	s = direct_modifiers (s);

	/* Check if we need a shift modifier */
	normal = XKeycodeToKeysym (dpy, k, 0);
	shifted = XKeycodeToKeysym (dpy, k, 1);
	if (sym != normal && sym == shifted)
	    s |= ShiftMask;

	if (s & EV_MOD_RELEASE)
	    s &= ~EV_MOD_RELEASE;
	if (s == EV_MOD_ANY)
	    s = AnyModifier;

	if ((s & EV_VIRT_MOD_MASK) != 0)
	    return FALSE;

	*state = s;
	*keycode = k;
	return TRUE;
    }
    else
	return FALSE;
}

/* Translate the Lisp button event EV to X button identifier *BUTTON and
   modifier mask *STATE, returning true if successful. */
static unsigned int
translate_event_to_x_button (repv ev, unsigned int *button, unsigned int *state)
{
    if (rep_INT(EVENT_MODS(ev)) & EV_TYPE_MOUSE)
    {
	unsigned long mods = rep_INT(EVENT_MODS(ev));
	static struct { unsigned int button; unsigned int mask; } buttons[] = {
	    { Button1, Button1Mask },
	    { Button2, Button2Mask },
	    { Button3, Button3Mask },
	    { Button4, Button4Mask },
	    { Button5, Button5Mask },
#ifdef HAVE_NINE_MOUSEBUTTONS
	    { Button6, Button6Mask },
	    { Button7, Button7Mask },
	    { Button8, Button8Mask },
	    { Button9, Button9Mask },
#endif
	    { 0, 0 }
	};
	int i;

	for (i = 0; buttons[i].button != 0; i++)
	{
	    if (mods & buttons[i].mask)
	    {
		unsigned int s;
		mods &= ~buttons[i].mask;
		s = direct_modifiers (mods & EV_MOD_MASK);
		if (s == EV_MOD_ANY)
		    s = AnyModifier;
		if ((s & EV_VIRT_MOD_MASK) == 0)
		{
		    *button = buttons[i].button;
		    *state = s;
		    return buttons[i].mask;
		}
	    }
	}
	/* no actual button specified. if mod_any is set, then just
	   return this; hope the caller does the Right Thing */
	if (mods & EV_MOD_ANY)
	{
	    *state = AnyModifier;
	    *button = 0;		/* anything.. */
	    return 1;
	}
    }
    return 0;
}

/* Keymap searching */

static inline bool
compare_events (unsigned long code1, unsigned long mods1, unsigned long code2, unsigned long mods2)
{
    return (code1 == code2
	    && ((mods1 == mods2)
		|| ((mods2 & wm_mod) == wm_mod
		    && ((mods2 & ~wm_mod) | EV_MOD_WM) == mods1)
		|| (((mods1 & EV_MOD_MASK) & EV_MOD_ANY)
		    /* this allows things like Any-C-x, mapping to C-x
		       _plus_ any other modifiers */
		    && (((mods1 & ~EV_MOD_ANY) & mods2)
			== (mods1 & ~EV_MOD_ANY)))));
}

/* Search the keymap KM for a binding of CODE&MODS.
   If CALLBACK is non-nil it's a function to call for the binding found.
   If this function returns true, then this binding is acceptable and
   is returned from the function. */
static repv
search_keymap(repv km, unsigned long code, unsigned long mods, bool (*callback)(repv key))
{
    /* If it's a symbol, dereference it. */
    while(rep_SYMBOLP(km) && !rep_NILP(km) && !rep_INTERRUPTP)
    {
	repv tem = Fsymbol_value(km, Qt);
	if(tem == km)
	    break;
	km = tem;
	rep_TEST_INT;
    }

    /* Find the list of bindings to scan. */
    if rep_CONSP(km)
	km = rep_CDR(km);
    else
	return rep_NULL;

    /* Scan them for a match.. */
    while(rep_CONSP(km))
    {
	rep_TEST_INT; if(rep_INTERRUPTP) break;
	if(rep_CONSP(rep_CAR(km)))
	{
	    repv ev = KEY_EVENT(rep_CAR(km));
	    if(compare_events (rep_INT(EVENT_CODE(ev)),
			       rep_INT(EVENT_MODS(ev)),
			       code, mods))
	    {
		repv key = rep_CAR(km);
		if(callback == 0 || callback(key))
		    return key;
	    }
	    km = rep_CDR(km);
	}
	else
	    /* An inherited sub-keymap. Start scanning it */
	    km = rep_CDR(km);
    }
    return rep_NULL;
}

/* Search for a binding of CODE&MODS.  */
static repv
lookup_binding(unsigned long code, unsigned long mods, bool (*callback)(repv key),
	       repv context_keymap, Lisp_Window *current_window)
{
    repv k = rep_NULL, nkp = next_keymap_path;
    next_keymap_path = rep_NULL;

    if(nkp == rep_NULL || nkp == Qglobal_keymap)
    {
	repv tem = global_symbol_value (Qoverride_keymap);
	if (tem != Qnil && !rep_VOIDP(tem))
	    k = search_keymap (tem, code, mods, callback);
	else
	{
	    /* 1. search keymap for active window decoration */
	    k = search_keymap(context_keymap, code, mods, callback);

	    if (!k && (current_x_event != 0
		       && (current_x_event->xany.window == root_window
			   || current_x_event->xany.window
			   == no_focus_window)))
	    {
		/* 2. if event from root, search the root-window-keymap */
		k = search_keymap (Qroot_window_keymap, code, mods, callback);
	    }

	    if (!k && current_window)
	    {
		/* 3. search focused/pointer window keymap property */
	        tem = Fwindow_get (rep_VAL(current_window), Qkeymap, Qnil);
		if (tem && tem != Qnil)
		    k = search_keymap(tem, code, mods, callback);
	    }
	    if(!k)
		/* 4. search global-keymap */
		k = search_keymap(Qglobal_keymap, code, mods, callback);
	}
    }
    else
    {
	rep_GC_root gc_nkp;
	rep_PUSHGC(gc_nkp, nkp);
	while(!k && rep_CONSP(nkp))
	{
	    k = search_keymap(rep_CAR(nkp), code, mods, callback);
	    nkp = rep_CDR(nkp);
	}
	rep_POPGC;
    }
    return (k != rep_NULL && KEYP(k)) ? KEY_COMMAND(k) : rep_NULL;
}

static bool
eval_input_callback(repv key)
{
    repv cmd = KEY_COMMAND(key);
    if(rep_SYMBOLP(cmd))
    {
	cmd = Fsymbol_value (cmd, Qt);
	if (rep_FUNARGP(cmd))
	{
	    repv fun = rep_FUNARG(cmd)->fun;
	    if(rep_CONSP(fun) && rep_CAR(fun) == Qautoload)
	    {
		/* An autoload, try to load it. */
		rep_GC_root gc_key;
		rep_PUSHGC (gc_key, key);
		cmd = rep_call_with_closure (cmd, rep_load_autoload, cmd);
		rep_POPGC;
		if(cmd == rep_NULL)
		    return FALSE;
	    }
	}
    }
    if(Fkeymapp (cmd) != Qnil)
    {
	/* A prefix key, add its list to the next-keymap-path. */
	next_keymap_path = Fcons(cmd, next_keymap_path
				 ? next_keymap_path : Qnil);
	/* Look for more prefix keys */
	return FALSE;
    }
    if (cmd == Qnil)
	return FALSE;
    next_keymap_path = rep_NULL;
    return TRUE;
}

static repv
lookup_event_binding (unsigned long code, unsigned long mods, repv context_map)
{
    Lisp_Window *w = 0;
    if (current_x_event && (mods & EV_TYPE_MOUSE))
    {
	/* If a mouse event, look for bindings in the window that
	   the button was pressed in, not where the input focus is. */
	repv tem = Fquery_button_press_window ();
	if (tem && WINDOWP(tem))
	    w = VWIN(tem);
    }
    else
	w = focus_window;

    return lookup_binding(code, mods, eval_input_callback, context_map, w);
}

/* Process the event CODE+MODS. OS-INPUT-MSG is the raw input event
   from the window-system, this is only used to cook a string from.  */
repv
eval_input_event(repv context_map)
{
    unsigned long code, mods;
    repv result = Qnil, cmd, orig_next_keymap_path = next_keymap_path;

    if (!translate_event (&code, &mods, current_x_event))
	return Qnil;

    event_buf[event_index++] = code;
    event_buf[event_index++] = mods;
    if(event_index == EVENT_BUFSIZ)
	event_index = 0;
    printed_this_prefix = FALSE;

    current_event[0] = code;
    current_event[1] = mods;

    cmd = lookup_event_binding (code, mods, context_map);

    if (next_keymap_path == rep_NULL)
    {
	if (print_prefix)
	    rep_call_lisp1 (global_symbol_value (Qdisplay_message), Qnil);
	print_prefix = FALSE;
	event_index = 0;
    }

    if(cmd != rep_NULL)
    {
	/* Found a binding for this event; evaluate it. */
	result = rep_call_lisp1 (Fsymbol_value (Qcall_command, Qt), cmd);
    }
    else if(next_keymap_path != rep_NULL)
    {
	/* We already handled some prefixes. */
	Fset (Qthis_command, Qkeymap);
	result = Qnil;

	/* Grab the input devices for the next event */
	Fgrab_keyboard (focus_window ? rep_VAL(focus_window) : Qnil, Qt, Qt);
    }
    else if(orig_next_keymap_path != rep_NULL
	    && orig_next_keymap_path != Qglobal_keymap)
    {
	/* A multi-key binding, but no final step; clear the prefix
	   argument for the next command and beep. */
	Fset (Qprefix_arg, Qnil);
	Fbeep();
    }
    else
    {
	/* An unbound key with no prefix keys. */

	repv hook = global_symbol_value (Qunbound_key_hook);
	if (!rep_VOIDP (hook) && hook != Qnil)
	    result = Fcall_hook(Qunbound_key_hook, Qnil, Qor);
	else if (rep_recurse_depth == 0
		 && (mods & (EV_TYPE_KEY | EV_MOD_RELEASE)) == EV_TYPE_KEY)
	{
	    /* We're receiving events that we have no way of handling..
	       I think this gives us justification in aborting in case
	       we've got stuck with an unbreakable grab somehow.. */
	    Fungrab_keyboard ();
	    Fungrab_pointer ();
	    result = Fthrow (Qtop_level, Qnil);
	}
    }

    /* Out of a multi-key sequence, ungrab */
    if (orig_next_keymap_path && !next_keymap_path)
	Fungrab_keyboard ();

    last_event[0] = current_event[0];
    last_event[1] = current_event[1];

    /* Only lose the current event when in the topmost event loop. */
    if (rep_recurse_depth == 0)
	current_event[0] = current_event[1] = 0;

    if (print_prefix)
	print_event_prefix ();

    return result;
}

/* Translate text->event and vice versa */

struct key_def {
    const char *name;
    unsigned long mods, code;
};

static struct key_def default_mods[] = {
    { "S",	  ShiftMask },
    { "Shift",	  ShiftMask },
    { "C",        ControlMask },
    { "Control",  ControlMask },
    { "M",        EV_MOD_META },
    { "Meta",     EV_MOD_META },
    { "A",        EV_MOD_ALT },
    { "Alt",      EV_MOD_ALT },
    { "H",        EV_MOD_HYPER },
    { "Hyper",    EV_MOD_HYPER },
    { "Super",    EV_MOD_SUPER },
    { "W",	  EV_MOD_WM },
    { "Mod1",     Mod1Mask },
    { "Mod2",     Mod2Mask },
    { "Mod3",     Mod3Mask },
    { "Mod4",     Mod4Mask },
    { "Mod5",     Mod5Mask },
    { "Button1",  Button1Mask },
    { "Button2",  Button2Mask },
    { "Button3",  Button3Mask },
    { "Button4",  Button4Mask },
    { "Button5",  Button5Mask },
#ifdef HAVE_NINE_MOUSEBUTTONS
    { "Button6",  Button6Mask },
    { "Button7",  Button7Mask },
    { "Button8",  Button8Mask },
    { "Button9",  Button9Mask },
#endif
    { "Any",      EV_MOD_ANY },
    { "Release",  EV_MOD_RELEASE },
    { 0, 0 }
};

static struct key_def default_codes[] = {
    { "Click",    EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click1",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click2",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK2 },
    { "Click3",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK3 },
    { "Off",      EV_TYPE_MOUSE, EV_CODE_MOUSE_UP1 },
    { "Off1",     EV_TYPE_MOUSE, EV_CODE_MOUSE_UP1 },
    { "Off2",     EV_TYPE_MOUSE, EV_CODE_MOUSE_UP2 },
    { "Off3",     EV_TYPE_MOUSE, EV_CODE_MOUSE_UP3 },
    { "Move",     EV_TYPE_MOUSE, EV_CODE_MOUSE_MOVE },

    { "SPC",      EV_TYPE_KEY, XK_space },
    { "Space",    EV_TYPE_KEY, XK_space },
    { "TAB",      EV_TYPE_KEY, XK_Tab },
    { "RET",      EV_TYPE_KEY, XK_Return },
    { "ESC",      EV_TYPE_KEY, XK_Escape },
    { "BS",       EV_TYPE_KEY, XK_BackSpace },
    { "DEL",      EV_TYPE_KEY, XK_Delete },

    /* X defines lots of long names for these simple keys...  */
    { " ",        EV_TYPE_KEY, XK_space },
    { "!",        EV_TYPE_KEY, XK_exclam },
    { "\"",       EV_TYPE_KEY, XK_quotedbl },
    { "#",        EV_TYPE_KEY, XK_numbersign },
    { "$",        EV_TYPE_KEY, XK_dollar },
    { "%",        EV_TYPE_KEY, XK_percent },
    { "&",        EV_TYPE_KEY, XK_ampersand },
    { "'",        EV_TYPE_KEY, XK_quoteright },
    { "(",        EV_TYPE_KEY, XK_parenleft },
    { ")",        EV_TYPE_KEY, XK_parenright },
    { "*",        EV_TYPE_KEY, XK_asterisk },
    { "+",        EV_TYPE_KEY, XK_plus },
    { ",",        EV_TYPE_KEY, XK_comma },
    { "-",        EV_TYPE_KEY, XK_minus },
    { ".",        EV_TYPE_KEY, XK_period },
    { "/",        EV_TYPE_KEY, XK_slash },
    { ":",        EV_TYPE_KEY, XK_colon },
    { ";",        EV_TYPE_KEY, XK_semicolon },
    { "<",        EV_TYPE_KEY, XK_less },
    { "=",        EV_TYPE_KEY, XK_equal },
    { ">",        EV_TYPE_KEY, XK_greater },
    { "?",        EV_TYPE_KEY, XK_question },
    { "@",        EV_TYPE_KEY, XK_at },
    { "[",        EV_TYPE_KEY, XK_bracketleft },
    { "\\",       EV_TYPE_KEY, XK_backslash },
    { "]",        EV_TYPE_KEY, XK_bracketright },
    { "^",        EV_TYPE_KEY, XK_asciicircum },
    { "_",        EV_TYPE_KEY, XK_underscore },
    { "`",        EV_TYPE_KEY, XK_quoteleft },
    { "{",        EV_TYPE_KEY, XK_braceleft },
    { "|",        EV_TYPE_KEY, XK_bar },
    { "}",        EV_TYPE_KEY, XK_braceright },
    { "~",        EV_TYPE_KEY, XK_asciitilde },

    { 0, 0, 0 }
};

/* Convert an event described by a string DESC into integers
   CODE and MODS. */
static bool
lookup_event(unsigned long *code, unsigned long *mods, char *desc)
{
    char *tem;
    char buf[100];
    *code = *mods = 0;

    /* First handle all modifiers */
    while(*desc && (tem = strchr(desc + 1, '-')) != 0)
    {
	struct key_def *x = default_mods;

	memcpy(buf, desc, tem - desc);
	buf[tem - desc] = 0;

	while(x->name != 0)
	{
	    if(strcasecmp(buf, x->name) == 0)
	    {
		*mods |= x->mods;
		break;
	    }
	    x++;
	}
	if(x->name == 0)
	    goto error;

	desc = tem + 1;
    }

    /* Then go for the code itself */
    {
	struct key_def *x = default_codes;
	unsigned int ks;
	while(x->name != 0)
	{
	    if(strcasecmp(desc, x->name) == 0)
	    {
		*mods |= x->mods;
		*code = x->code;
		if ((x->mods == EV_TYPE_MOUSE)
		    && (*mods & (EV_MOD_BUTTON_MASK | EV_MOD_ANY)) == 0)
		{
		    /* Button events must include at least one of the button
		       modifiers (otherwise they can never be generated) */
		    return FALSE;
		}
		else
		    return TRUE;
	    }
	    x++;
	}
	ks = XStringToKeysym(desc);
	if(ks != NoSymbol)
	{
	    if (*mods & ShiftMask)
	    {
		KeySym lower, upper;
		XConvertCase (ks, &lower, &upper);
		if (ks == lower && upper != lower)
		{
		    /* canonify `S-x' to `X' */
		    *mods &= ~ShiftMask;
		    ks = upper;
		}
	    }
	    *mods |= EV_TYPE_KEY;
	    *code = ks;
	    return TRUE;
	}
	else
	    goto error;
    }

error:
    Fsignal(Qbad_event_desc, rep_LIST_1(rep_string_dup(desc)));
    return FALSE;
}

/* Constructs the name of the event defined by CODE and MODS in BUF.  */
static bool
lookup_event_name(char *buf, unsigned long code, unsigned long mods)
{
    int i;
    struct key_def *x;
    unsigned long type = mods & EV_TYPE_MASK;
    char
        *end = buf,
        *tem;

    *buf = '\0';

    mods &= EV_MOD_MASK;
    for(i = 32; i >= 0 && mods != 0; i--)	/* magic numbers!? */
    {
	unsigned long mask = 1 << i;
	if(mods & mask)
	{
	    mods &= ~mask;
	    x = default_mods;
	    while(x->name != 0)
	    {
		if(x->mods == mask)
		{
		    strcpy (end, x->name);
		    end += strlen (x->name);
		    break;
		}
		x++;
	    }
	    *end++ = '-';
	}
    }

    x = default_codes;
    while(x->name != 0)
    {
	if(type == x->mods && code == x->code)
	{
	    strcpy(end, x->name);
	    return TRUE;
	}
	x++;
    }
    tem = XKeysymToString((KeySym)code);
    if(tem != 0)
    {
	strcpy(end, tem);
	return TRUE;
    }
    return FALSE;
}

/* If necessary, print the name of the current event prefix. Returns true
   if in the middle of a multi-key sequence.  */
bool
print_event_prefix(void)
{
    int i;
    char buf[256];
    char *bufp = buf;

    if (next_keymap_path == rep_NULL)
	return FALSE;
    else if (printed_this_prefix)
	return TRUE;

    print_prefix = TRUE;
    for (i = 0; i < event_index; i += 2)
    {
	if (lookup_event_name (bufp, event_buf[i], event_buf[i+1]))
	{
	    bufp += strlen (bufp);
	    *bufp++ = ' ';
	}
    }
    if (next_keymap_path != rep_NULL)
    {
	if (bufp > buf)
	    bufp--;			/* erase the last space */
	*bufp++ = '.';
	*bufp++ = '.';
	*bufp++ = '.';
    }

    rep_call_lisp1 (global_symbol_value (Qdisplay_message),
		    rep_string_dupn (buf, bufp - buf));
    printed_this_prefix = TRUE;

    return TRUE;
}

/* Lisp functions */

DEFUN("make-keymap", Fmake_keymap, Smake_keymap, (void), rep_Subr0) /*
::doc:sawfish.wm.events#make-keymap::
make-keymap

Return a new keymap suitable for storing bindings in. This is a cons cell
looking like `(keymap . LIST-OF-BINDINGS)', LIST-OF-BINDINGS is initially
nil.
::end:: */
{
    return Fcons(Qkeymap, Qnil);
}

DEFUN("bind-keys", Fbind_keys, Sbind_keys, (repv args), rep_SubrN) /*
::doc:sawfish.wm.events#bind-keys::
bind-keys KEYMAP { EVENT-DESCRIPTION COMMAND }...

Adds key bindings to KEYMAP. Each EVENT-DESCRIPTION is a string naming an
event to bind to the corresponding COMMAND.

Returns KEYMAP when successful.
::end:: */
{
    repv km, arg1;
    if (!rep_CONSP(args))
	return rep_signal_missing_arg (1);
    km = rep_CAR(args);
    args = rep_CDR(args);
    while (rep_CONSP(args) && rep_CONSP(rep_CDR(args)))
    {
	unsigned long code, mods;
	repv key;
	arg1 = rep_CAR(args);
	args = rep_CDR(args);
	if (rep_STRINGP(arg1))
	{
	    if (!lookup_event (&code, &mods, rep_STR(arg1)))
		return Fsignal (Qbad_event_desc, rep_LIST_1(arg1));
	}
	else if(Feventp(arg1) != Qnil)
	{
	    code = rep_INT(rep_CAR(arg1));
	    mods = rep_INT(rep_CDR(arg1));
	}
	else
	    return Fsignal (Qbad_event_desc, rep_LIST_1(arg1));

	key = search_keymap (km, code, mods, 0);
	if (key != rep_NULL && rep_CONSP(key))
	    KEY_COMMAND(key) = rep_CAR(args);
	else
	{
	    key = MAKE_KEY(MAKE_EVENT(rep_MAKE_INT(code), rep_MAKE_INT(mods)),
			   rep_CAR(args));
	    rep_CDR(km) = Fcons(key, rep_CDR(km));
	    grab_keymap_event (km, code, mods, TRUE);
	}
	args = rep_CDR(args);
    }
    return km;
}

DEFUN("unbind-keys", Funbind_keys, Sunbind_keys, (repv args), rep_SubrN) /*
::doc:sawfish.wm.events#unbind-keys::
unbind-keys KEY-MAP EVENT-DESCRIPTION...
::end:: */
{
    repv km, arg1;
    if (!rep_CONSP(args))
	return rep_signal_missing_arg (1);

    km = rep_CAR(args);
    if (!rep_CONSP(km))
	return rep_signal_arg_error(km, 1);

    args = rep_CDR(args);
    while (rep_CONSP(args))
    {
	unsigned long code, mods;
	repv *keyp;
	arg1 = rep_CAR(args);
	if (rep_STRINGP(arg1))
	{
	    if (!lookup_event (&code, &mods, rep_STR(arg1)))
		return Fsignal (Qbad_event_desc, rep_LIST_1(arg1));
	}
	else if(Feventp(arg1) != Qnil)
	{
	    code = rep_INT(rep_CAR(arg1));
	    mods = rep_INT(rep_CDR(arg1));
	}
	else
	    return Fsignal(Qbad_event_desc, rep_LIST_1(arg1));

	keyp = &rep_CDR(km);
	while (rep_CONSP(*keyp))
	{
	    repv cell = rep_CAR(*keyp);
	    if (rep_CONSP(cell))
	    {
		if ((rep_INT(EVENT_MODS(KEY_EVENT(cell))) == mods)
		    && (rep_INT(EVENT_CODE(KEY_EVENT(cell))) == code))
		{
		    *keyp = rep_CDR(*keyp);
		}
		else
		    keyp = &rep_CDR(*keyp);
	    }
	    rep_TEST_INT; if(rep_INTERRUPTP) return rep_NULL;
	}
	grab_keymap_event (km, code, mods, FALSE);

	args = rep_CDR(args);
    }
    return km;
}

DEFUN("grab-keymap", Fgrab_keymap, Sgrab_keymap, (repv map), rep_Subr1) /*
::doc:sawfish.wm.events#grab-keymap::
grab-keymap KEYMAP

Grab any events in KEYMAP that need to be grabbed so that bindings in
KEYMAP may be serviced.
::end:: */
{
    rep_DECLARE1(map, rep_CONSP);
    grab_all_keylist_events (map, TRUE);
    return map;
}

DEFUN("ungrab-keymap", Fungrab_keymap, Sungrab_keymap, (repv map), rep_Subr1)/*
::doc:sawfish.wm.events#ungrab-keymap::
ungrab-keymap KEYMAP

Ungrab any events in KEYMAP that would have been grabbed so that bindings in
KEYMAP may be serviced.
::end:: */
{
    rep_DECLARE1(map, rep_CONSP);
    grab_all_keylist_events (map, FALSE);
    return map;
}

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

DEFUN("keymapp", Fkeymapp, Skeymapp, (repv arg), rep_Subr1) /*
::doc:sawfish.wm.events#keymapp::
keymapp ARG

Returns t if ARG can be used as a keymap.
::end:: */
{
    return (rep_CONSP(arg) && rep_CAR(arg) == Qkeymap) ? Qt : Qnil;
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

/* Find the lisp modifier mask used by the meta and alt keys. This code
   shamelessly stolen from Emacs 19. :-) */

DEFSTRING(meta_l, "Meta_L");
DEFSTRING(meta_r, "Meta_R");
DEFSTRING(alt_l, "Alt_L");
DEFSTRING(alt_r, "Alt_R");
DEFSTRING(hyper_l, "Hyper_L");
DEFSTRING(hyper_r, "Hyper_R");
DEFSTRING(super_l, "Super_L");
DEFSTRING(super_r, "Super_R");

static void
nconc (repv x, repv y)
{
    repv *ptr = &x;

    while (rep_CONSP (*ptr))
	ptr = rep_CDRLOC (*ptr);

    *ptr = y;
}

static void
find_meta(void)
{
    int min_code, max_code;
    KeySym *syms;
    int syms_per_code;
    XModifierKeymap *mods;
    repv meta_syms = Qnil, alt_syms = Qnil;
    repv hyper_syms = Qnil, super_syms = Qnil;

#if defined (XlibSpecificationRelease) && XlibSpecificationRelease >= 4
    XDisplayKeycodes(dpy, &min_code, &max_code);
#else
    min_code = dpy->min_keycode;
    max_code = dpy->max_keycode;
#endif

    /* reset to default values. */
    meta_mod = alt_mod = hyper_mod = super_mod = 0;
    num_lock_mod = scroll_lock_mod = 0;

    Fset (Qmeta_keysyms, Qnil);
    Fset (Qalt_keysyms, Qnil);
    Fset (Qhyper_keysyms, Qnil);
    Fset (Qsuper_keysyms, Qnil);

    syms = XGetKeyboardMapping(dpy, min_code, max_code - min_code + 1,
			       &syms_per_code);
    mods = XGetModifierMapping(dpy);

    {
	int row, col;

	for(row = 3; row < 8; row++)
	{
	    for(col = 0; col < mods->max_keypermod; col++)
	    {
		KeyCode code = mods->modifiermap[(row * mods->max_keypermod)
						+ col];
		int code_col;
		if(code == 0)
		    continue;
		for(code_col = 0; code_col < syms_per_code; code_col++)
		{
		    int sym = syms[((code - min_code) * syms_per_code)
				  + code_col];
		    /* Is this a fake key? */
		    if (code_col == 0 && sym == NoSymbol)
			break;
		    switch(sym)
		    {
		    case XK_Meta_L: case XK_Meta_R:
			meta_mod = 1 << row;
			meta_syms = Fcons (sym == XK_Meta_L ? rep_VAL(&meta_l)
					   : rep_VAL(&meta_r), meta_syms);
			break;

		    case XK_Alt_L: case XK_Alt_R:
			alt_mod = 1 << row;
			alt_syms = Fcons (sym == XK_Alt_L ? rep_VAL(&alt_l)
					  : rep_VAL(&alt_r), alt_syms);
			break;

		    case XK_Hyper_L: case XK_Hyper_R:
			hyper_mod = 1 << row;
			hyper_syms = Fcons (sym == XK_Hyper_L
					    ? rep_VAL(&hyper_l)
					    : rep_VAL(&hyper_r), hyper_syms);
			break;

		    case XK_Super_L: case XK_Super_R:
			super_mod = 1 << row;
			super_syms = Fcons (sym == XK_Super_L
					    ? rep_VAL(&super_l)
					    : rep_VAL(&super_r), super_syms);
			break;

		    case XK_Num_Lock:
			num_lock_mod = 1 << row;
			break;

		    case XK_Scroll_Lock:
			scroll_lock_mod = 1 << row;
			break;
		    }
		}
	    }
	}
    }

    XFree((char *)syms);
    XFreeModifiermap(mods);

    if (meta_mod == 0 && alt_mod == 0 && hyper_mod == 0 && super_mod == 0)
	return;

    if (meta_mod == 0)
	meta_mod = alt_mod;
    if (meta_mod == 0)
	meta_mod = hyper_mod;
    if (meta_mod == 0)
	meta_mod = super_mod;

    if (meta_mod == alt_mod)
    {
	nconc (meta_syms, alt_syms);
	alt_syms = meta_syms;
    }
    if (meta_mod == hyper_mod)
    {
	nconc (meta_syms, hyper_syms);
	hyper_syms = meta_syms;
    }
    if (meta_mod == super_mod)
    {
	nconc (meta_syms, super_syms);
	super_syms = meta_syms;
    }

    Fset (Qmeta_keysyms, meta_syms);
    Fset (Qalt_keysyms, alt_syms);
    Fset (Qhyper_keysyms, hyper_syms);
    Fset (Qsuper_keysyms, super_syms);
}

static void
build_lock_mods (void)
{
    int i;
    total_lock_combs = 2 * (num_lock_mod ? 2 : 1) * (scroll_lock_mod ? 2 : 1);
    memset (all_lock_combs, 0, sizeof (all_lock_combs));
    for (i = 0; i < total_lock_combs; i++)
    {
	if (i & 1)
	    all_lock_combs[i] |= LockMask;
	if (i & 2)
	    all_lock_combs[i] |= num_lock_mod ? num_lock_mod : scroll_lock_mod;
	if (i & 4)
	    all_lock_combs[i] |= scroll_lock_mod;
    }
    all_lock_mask = LockMask | num_lock_mod | scroll_lock_mod;
}

void
update_keyboard_mapping (void)
{
    find_meta ();
    build_lock_mods ();
}

static void
set_wm_modifier (unsigned long mods)
{
    wm_mod = indirect_modifiers (mods);
}

DEFUN ("set-wm-modifier", Fset_wm_modifier,
       Sset_wm_modifier, (repv mods), rep_Subr1) /*
::doc:sawfish.wm.events#set-wm-modifier::
set-wm-modifier MODIFIERS

Set the value of the `Window Manager' modifier to MODIFIERS, an integer.
::end:: */
{
    rep_DECLARE1 (mods, rep_INTP);
    set_wm_modifier (rep_INT (mods));
    return Qt;
}

DEFUN ("wm-modifier", Fwm_modifier, Swm_modifier, (void), rep_Subr0) /*
::doc:sawfish.wm.events#wm-modifier::
wm-modifier

Returns the current value of the `Window Manager' modifier, an integer.
::end:: */
{
    return rep_MAKE_INT (wm_mod);
}

/* Key and button grabbing */

static void
grab_event (Window grab_win, repv ev)
{
    switch (rep_INT(EVENT_MODS(ev)) & EV_TYPE_MASK)
    {
	unsigned int code, state;
	int i;

    case EV_TYPE_KEY:
	if (translate_event_to_x_key (ev, &code, &state))
	{
	    if (state != AnyModifier)
	    {
		for (i = 0; i < total_lock_combs; i++)
		{
		    XGrabKey (dpy, code, state | all_lock_combs[i], grab_win,
			      False, GrabModeSync, GrabModeSync);
		}
	    }
	    else
	    {
		XGrabKey (dpy, code, state, grab_win,
			  False, GrabModeSync, GrabModeSync);
	    }
	}
	break;

    case EV_TYPE_MOUSE:
	if (translate_event_to_x_button (ev, &code, &state))
	{
	    if (state != AnyModifier)
	    {
		for (i = 0; i < total_lock_combs; i++)
		{
		    XGrabButton (dpy, code, state | all_lock_combs[i],
				 grab_win, False, POINTER_GRAB_EVENTS,
				 GrabModeSync, GrabModeSync, None, None);
		}
	    }
	    else if (code != 0)
	    {
		XGrabButton (dpy, code, AnyModifier, grab_win,
			     False, POINTER_GRAB_EVENTS,
			     GrabModeSync, GrabModeSync, None, None);
	    }
	    else
	    {
		/* sawfish treats mouse buttons as modifiers, not as
		   codes, so for us AnyModifier includes all buttons.. */
#ifdef HAVE_NINE_MOUSEBUTTONS
		for (i = 0; i < 9; i++)
#else
		for (i = 0; i < 5; i++)
#endif
		{
		    XGrabButton (dpy, all_buttons[i], AnyModifier,
				 grab_win, False, POINTER_GRAB_EVENTS,
				 GrabModeSync, GrabModeSync, None, None);
		}
	    }
	}
    }
}

static void
ungrab_event (Window grab_win, repv ev)
{
    switch (rep_INT(EVENT_MODS(ev)) & EV_TYPE_MASK)
    {
	unsigned int code, state;
	int i;

    case EV_TYPE_KEY:
	if (translate_event_to_x_key (ev, &code, &state))
	{
	    if (state != AnyModifier)
	    {
		for (i = 0; i < total_lock_combs; i++)
		{
		    XUngrabKey (dpy, code, state | all_lock_combs[i],
				grab_win);
		}
	    }
	    else
		XUngrabKey (dpy, code, state, grab_win);
	}
	break;

    case EV_TYPE_MOUSE:
	if (translate_event_to_x_button (ev, &code, &state))
	{
	    if (state != AnyModifier)
	    {
		for (i = 0; i < total_lock_combs; i++)
		{
		    XUngrabButton (dpy, code, state | all_lock_combs[i],
				   grab_win);
		}
	    }
	    else if (code != 0)
	    {
		XUngrabButton (dpy, code, AnyModifier, grab_win);
	    }
	    else
	    {
#ifdef HAVE_NINE_MOUSEBUTTONS
		for (i = 0; i < 9; i++)
#else
		for (i = 0; i < 5; i++)
#endif
		    XUngrabButton (dpy, all_buttons[i], AnyModifier, grab_win);
	    }
	}
    }
}

static void
grab_keymap_event (repv km, long code, long mods, bool grab)
{
    Lisp_Window *w;
    repv ev = MAKE_EVENT(rep_MAKE_INT(code), rep_MAKE_INT(mods));
    repv global = Fsymbol_value (Qglobal_keymap, Qt);
    if (rep_SYMBOLP(km))
	km = Fsymbol_value (km, Qt);
    for (w = window_list; w != 0; w = w->next)
    {
	if (!WINDOW_IS_GONE_P (w))
	{
	    repv tem = Fwindow_get (rep_VAL(w), Qkeymap, Qnil);
	    if (rep_SYMBOLP(tem) && tem != Qnil)
		tem = Fsymbol_value (tem, Qt);
	    if (km == global || tem == km)
		(grab ? grab_event : ungrab_event) (w->id, ev);
	}
    }
}

static void
grab_all_keylist_events (repv map, bool grab)
{
    repv tem = rep_CDR(map);
    while (rep_CONSP(tem) && !rep_INTERRUPTP)
    {
	repv key = KEY_EVENT(rep_CAR(tem));
	grab_keymap_event (map, rep_INT(EVENT_CODE(key)),
			   rep_INT(EVENT_MODS(key)), grab);
	tem = rep_CDR(tem);
	rep_TEST_INT;
    }
}

static void
grab_keylist_events (Window grab_win, repv list, bool grab)
{
    while (!rep_INTERRUPTP && rep_CONSP(list))
    {
	(grab ? grab_event : ungrab_event) (grab_win,
					    KEY_EVENT(rep_CAR(list)));
	list = rep_CDR(list);
	rep_TEST_INT;
    }
}

void
grab_keymap_events (Window grab_win, repv keymap, bool grab)
{
    /* If it's a symbol, dereference it. */
    while(rep_SYMBOLP(keymap) && !rep_NILP(keymap) && !rep_INTERRUPTP)
    {
	repv tem = Fsymbol_value(keymap, Qt);
	if(tem == keymap)
	    break;
	keymap = tem;
	rep_TEST_INT;
    }

    if (rep_CONSP(keymap))
	grab_keylist_events (grab_win, rep_CDR(keymap), grab);
}

/* Grab all bound events in client window W. */
void
grab_window_events (Lisp_Window *w, bool grab)
{
    repv tem;
    tem = Fsymbol_value (Qglobal_keymap, Qt);
    if (tem != Qnil && !rep_VOIDP(tem) && !WINDOW_IS_GONE_P (w))
	grab_keymap_events (w->id, tem, grab);
    tem = Fwindow_get (rep_VAL(w), Qkeymap, Qnil);
    if (tem && tem != Qnil && !WINDOW_IS_GONE_P (w))
	grab_keymap_events (w->id, tem, grab);
}

static void
keymap_prop_change (Lisp_Window *w, repv prop, repv old, repv new)
{
    if (prop == Qkeymap && !WINDOW_IS_GONE_P (w))
    {
	/* A bit of a hack */
	grab_keymap_events (w->id, old, FALSE);
	grab_keymap_events (w->id, new, TRUE);
    }
}

/* initialisation */

void
keys_init(void)
{
    repv tem;

    rep_INTERN_SPECIAL(global_keymap);
    rep_INTERN_SPECIAL(root_window_keymap);
    rep_INTERN_SPECIAL(override_keymap);
    rep_INTERN_SPECIAL(unbound_key_hook);
    rep_INTERN_SPECIAL(eval_key_release_events);
    rep_INTERN_SPECIAL(eval_modifier_events);
    Fset (Qeval_key_release_events, Qnil);
    Fset (Qeval_modifier_events, Qnil);
    rep_INTERN(keymap);

    tem = rep_push_structure ("sawfish.wm.events");
    rep_ADD_SUBR(Smake_keymap);
    rep_ADD_SUBR(Sbind_keys);
    rep_ADD_SUBR(Sunbind_keys);
    rep_ADD_SUBR(Sgrab_keymap);
    rep_ADD_SUBR(Sungrab_keymap);
    rep_ADD_SUBR(Scurrent_event_string);
    rep_ADD_SUBR(Scurrent_event);
    rep_ADD_SUBR(Sproxy_current_event);
    rep_ADD_SUBR(Sallow_events);
    rep_ADD_SUBR(Slast_event);
    rep_ADD_SUBR(Sevent_name);
    rep_ADD_SUBR(Slookup_event);
    rep_ADD_SUBR(Slookup_event_binding);
    rep_ADD_SUBR(Ssearch_keymap);
    rep_ADD_SUBR(Skeymapp);
    rep_ADD_SUBR(Seventp);
    rep_ADD_SUBR(Sevent_match);
    rep_ADD_SUBR(Sforget_button_press);
    rep_ADD_SUBR(Sx_lookup_keysym);
    rep_ADD_SUBR(Sx_keysym_name);
    rep_ADD_SUBR(Ssynthesize_event);
    rep_ADD_SUBR(Sset_wm_modifier);
    rep_ADD_SUBR(Swm_modifier);
    rep_pop_structure (tem);

    rep_INTERN(async_pointer);
    rep_INTERN(async_keyboard);
    rep_INTERN(sync_pointer);
    rep_INTERN(sync_keyboard);
    rep_INTERN(replay_pointer);
    rep_INTERN(replay_keyboard);
    rep_INTERN(sync_both);
    rep_INTERN(async_both);

    rep_INTERN_SPECIAL(meta_keysyms);
    Fset (Qmeta_keysyms, Qnil);
    rep_INTERN_SPECIAL(alt_keysyms);
    Fset (Qalt_keysyms, Qnil);
    rep_INTERN_SPECIAL(hyper_keysyms);
    Fset (Qhyper_keysyms, Qnil);
    rep_INTERN_SPECIAL(super_keysyms);
    Fset (Qsuper_keysyms, Qnil);
    rep_INTERN_SPECIAL(multi_click_delay);
    Fset (Qmulti_click_delay, rep_MAKE_INT(DEFAULT_DOUBLE_CLICK_TIME));

    rep_INTERN_SPECIAL(this_command);
    rep_INTERN_SPECIAL(last_command);
    rep_INTERN_SPECIAL(prefix_arg);
    rep_INTERN_SPECIAL(current_prefix_arg);
    Fset (Qthis_command, Qnil);
    Fset (Qlast_command, Qnil);
    Fset (Qprefix_arg, Qnil);
    Fset (Qcurrent_prefix_arg, Qnil);

    rep_INTERN(display_message);
    rep_INTERN(call_command);

    rep_mark_static(&next_keymap_path);
 
    register_property_monitor (Qkeymap, keymap_prop_change);

    if (!batch_mode_p ())
	update_keyboard_mapping ();
}
