/* keys.c -- Key binding and evaluating (this should be called events.c)
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
#include "keys.h"
#include <string.h>
#include <stdlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

/* max number of milliseconds between successive-clicks */
#define DOUBLE_CLICK_TIME 250

/* current_event holds the event we're processing (or 0s), last_event
   contains the previously processed event.  */
static u_long current_event[2], last_event[2];

/* Data for testing double-clicks */
static Time last_click;
static u_long last_click_button;
static int click_count;

/* These control which types of keyboard events we actually evaluate */
static bool eval_modifier_events = FALSE, eval_key_release_events = FALSE;

DEFSYM(global_keymap, "global-keymap");
DEFSYM(root_window_keymap, "root-window-keymap");
DEFSYM(override_keymap, "override-keymap");
DEFSYM(unbound_key_hook, "unbound-key-hook");
DEFSYM(keymap, "keymap");

DEFSYM(async_pointer, "async-pointer");
DEFSYM(async_keyboard, "async-keyboard");
DEFSYM(sync_pointer, "sync-pointer");
DEFSYM(sync_keyboard, "sync-keyboard");
DEFSYM(replay_pointer, "replay-pointer");
DEFSYM(replay_keyboard, "replay-keyboard");
DEFSYM(sync_both, "sync-both");
DEFSYM(async_both, "async-both");

/* The X modifiers being used for Meta and Alt */
static u_long meta_mod, alt_mod, num_lock_mod;

DEFSYM(meta_keysyms, "meta-keysyms");
DEFSYM(alt_keysyms, "alt-keysyms");

static void grab_keymap_event (repv km, long code, long mods, bool grab);
static void grab_all_keylist_events (repv map, bool grab);

static int all_buttons[5] = { Button1, Button2, Button3, Button4, Button5 };
static int all_lock_combs[4] = { 0, LockMask, 0, LockMask };


/* Translate from X events to Lisp events */

/* Translate the X key or button event XEV to *CODE and *MODS */
static bool
translate_event(u_long *code, u_long *mods, XEvent *xev)
{
    bool ret = FALSE;
    switch(xev->type)
    {
    case KeyRelease:
	if (!eval_key_release_events)
	    break;
	/* FALL THROUGH */

    case KeyPress:
	*mods = xev->xkey.state & ~(LockMask | num_lock_mod);
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
	   && (eval_modifier_events || !IsModifierKey (*code)))
	{
	    *mods |= EV_TYPE_KEY;
	    ret = TRUE;
	}
	break;

    case ButtonPress:
	if(xev->xbutton.button == last_click_button
	   && xev->xbutton.time < (last_click + DOUBLE_CLICK_TIME))
	{
	    click_count++;
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
	*code = EV_CODE_MOUSE_UP;
    button:
	*mods = xev->xbutton.state & ~(LockMask | num_lock_mod);
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
	}
	ret = TRUE;
	break;

    case MotionNotify:
	*code = EV_CODE_MOUSE_MOVE;
	*mods = xev->xmotion.state & ~(LockMask | num_lock_mod);
	*mods |= EV_TYPE_MOUSE;
	ret = TRUE;
	break;
    }
    return ret;
}

/* Translate the Lisp key event EV to X keycode *KEYCODE and modifier
   mask *STATE, returning true if successful. */
static bool
translate_event_to_x_key (repv ev, u_int *keycode, u_int *state)
{
    if (rep_INT(EVENT_MODS(ev)) & EV_TYPE_KEY)
    {
	u_int s = rep_INT(EVENT_MODS(ev)) & EV_MOD_MASK;
	if (s & EV_MOD_META)
	    s = (s & ~EV_MOD_META) | meta_mod;
	if (s & EV_MOD_ALT)
	    s = (s & ~EV_MOD_ALT) | alt_mod;
	if (s & EV_MOD_RELEASE)
	    s &= ~EV_MOD_RELEASE;
	if (s == EV_MOD_ANY)
	    s = AnyModifier;
	*state = s;
	*keycode = XKeysymToKeycode (dpy, rep_INT(EVENT_CODE(ev)));
	return TRUE;
    }
    else
	return FALSE;
}

/* Translate the Lisp button event EV to X button identifier *BUTTON and
   modifier mask *STATE, returning true if successful. */
static bool
translate_event_to_x_button (repv ev, u_int *button, u_int *state)
{
    if (rep_INT(EVENT_MODS(ev)) & EV_TYPE_MOUSE
	&& rep_INT(EVENT_CODE(ev)) == EV_CODE_MOUSE_CLICK1)
    {
	u_long mods = rep_INT(EVENT_MODS(ev));
	static struct { u_int button; u_int mask; } buttons[] = {
	    { Button1, Button1Mask },
	    { Button2, Button2Mask },
	    { Button3, Button3Mask },
	    { Button4, Button4Mask },
	    { Button5, Button5Mask },
	    { 0, 0 }
	};
	int i;

	for (i = 0; buttons[i].button != 0; i++)
	{
	    if (mods & buttons[i].mask)
	    {
		u_int s;
		mods &= ~buttons[i].mask;
		s = mods & EV_MOD_MASK;
		if (s & EV_MOD_META)
		    s = (s & ~EV_MOD_META) | meta_mod;
		if (s & EV_MOD_ALT)
		    s = (s & ~EV_MOD_ALT) | alt_mod;
		if (s == EV_MOD_ANY)
		    s = AnyModifier;
		*button = buttons[i].button;
		*state = s;
		return TRUE;
	    }
	}
	/* no actual button specified. if mod_any is set, then just
	   return this; hope the caller does the Right Thing */
	if (mods & EV_MOD_ANY)
	{
	    *state = AnyModifier;
	    *button = buttons[0].button;	/* anything.. */
	    return TRUE;
	}
    }
    return FALSE;
}


/* Keymap searching */

/* Search the keymap KM for a binding of CODE&MODS.
   If CALLBACK is non-nil it's a function to call for the binding found.
   If this function returns true, then this binding is acceptable and
   is returned from the function. */
static repv
search_keymap(repv km, u_long code, u_long mods, bool (*callback)(repv key))
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
	    if(((rep_INT(EVENT_MODS(ev)) == mods)
		|| ((rep_INT(EVENT_MODS(ev)) & EV_MOD_MASK) == EV_MOD_ANY))
	       && rep_INT(EVENT_CODE(ev)) == code)
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
lookup_binding(u_long code, u_long mods, bool (*callback)(repv key),
	       repv context_keymap, Lisp_Window *current_window)
{
    repv k = rep_NULL;

    repv tem = Fsymbol_value (Qoverride_keymap, Qt);
    if (tem != Qnil && !rep_VOIDP(tem))
	k = search_keymap (tem, code, mods, callback);
    else
    {
	/* 1. search keymap for active window decoration */
	k = search_keymap(context_keymap, code, mods, callback);

	if (!k && current_x_event->xany.window == root_window)
	    /* 2. if event from root, search the root-window-keymap */
	    k = search_keymap (Qroot_window_keymap, code, mods, callback);

	if (!k && current_window)
	{
	    /* 2. search focused/pointer window keymap property */
	    tem = Fwindow_get (rep_VAL(current_window), Qkeymap);
	    if (tem && tem != Qnil)
		k = search_keymap(tem, code, mods, callback);
	}
	if(!k)
	    /* 3. search global-keymap */
	    k = search_keymap(Qglobal_keymap, code, mods, callback);
    }

    return (k != rep_NULL && KEYP(k)) ? KEY_COMMAND(k) : rep_NULL;
}

static repv
lookup_event_binding (u_long code, u_long mods, repv context_map)
{
    Lisp_Window *w = focus_window;
    if (current_x_event && mods & EV_TYPE_MOUSE)
    {
	/* If a mouse event, look for bindings in the window that
	   the pointer is in, not where the input focus is. */
	Lisp_Window *tem = find_window_by_id (current_x_event->xany.window);
	if (tem != 0)
	    w = tem;
	else
	{
	    struct frame_part *fp
		= find_frame_part_by_window (current_x_event->xany.window);
	    if (fp != 0)
		w = fp->win;
	}
    }
    return lookup_binding(code, mods, 0, context_map, w);
}

/* Process the event CODE+MODS. OS-INPUT-MSG is the raw input event
   from the window-system, this is only used to cook a string from.  */
repv
eval_input_event(repv context_map)
{
    u_long code, mods;
    repv result = Qnil, cmd;

    if (!translate_event (&code, &mods, current_x_event))
	return Qnil;

    current_event[0] = code;
    current_event[1] = mods;

    cmd = lookup_event_binding (code, mods, context_map);

    if(cmd != rep_NULL)
    {
	/* Found a binding for this event; evaluate it. */
	result = Fcall_command(cmd, Qnil);
    }
    else
    {
	/* An unbound key with no prefix keys. */
	result = Fcall_hook(Qunbound_key_hook, Qnil, Qor);
    }

    last_event[0] = current_event[0];
    last_event[1] = current_event[1];
    current_event[0] = current_event[1] = 0;

    return result;
}


/* Translate text->event and vice versa */

struct key_def {
    const char *name;
    u_long mods, code;
};

static struct key_def default_mods[] = {
    { "S",	  ShiftMask },
    { "C",        ControlMask },
    { "M",        EV_MOD_META },
    { "A",        EV_MOD_ALT },
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
    { "Any",      EV_MOD_ANY },
    { "Release",  EV_MOD_RELEASE },
    { 0, 0 }
};

static struct key_def default_codes[] = {
    { "Click1",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click",    EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click2",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK2 },
    { "Click3",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK3 },
    { "Off",      EV_TYPE_MOUSE, EV_CODE_MOUSE_UP },
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

/* Puts the integers defining the event described in DESC into CODE
   and MODS. */
static bool
lookup_event(u_long *code, u_long *mods, u_char *desc)
{
    char *tem;
    char buf[100];
    *code = *mods = 0;

    /* First handle all modifiers */
    while(*desc && (tem = strchr(desc + 1, '-')) != 0)
    {
	struct key_def *x = default_mods;

	memcpy(buf, desc, tem - (char *)desc);
	buf[tem - (char *)desc] = 0;

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
    if (*mods & EV_MOD_META)
	*mods = (*mods & ~EV_MOD_META) | meta_mod;
    if (*mods & EV_MOD_ALT)
	*mods = (*mods & ~EV_MOD_ALT) | alt_mod;

    /* Then go for the code itself */
    {
	struct key_def *x = default_codes;
	u_int ks;
	while(x->name != 0)
	{
	    if(strcasecmp(desc, x->name) == 0)
	    {
		*mods |= x->mods;
		*code = x->code;
		return TRUE;
	    }
	    x++;
	}
	ks = XStringToKeysym(desc);
	if(ks != NoSymbol)
	{
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
lookup_event_name(u_char *buf, u_long code, u_long mods)
{
    int i;
    struct key_def *x;
    u_long type = mods & EV_TYPE_MASK;

    char *end = buf, *tem;
    *buf = 0;

    if(mods & meta_mod)
	mods = (mods & ~meta_mod) | EV_MOD_META;
    if(mods & alt_mod)
	mods = (mods & ~alt_mod) | EV_MOD_ALT;
    mods &= EV_MOD_MASK;

    for(i = 0; i < 32 && mods != 0; i++)	/* magic numbers!? */
    {
	u_long mask = 1 << i;
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


/* Lisp functions */

DEFUN("make-keymap", Fmake_keymap, Smake_keymap, (void), rep_Subr0) /*
::doc:Smake-keymap::
make-keymap

Return a new keymap suitable for storing bindings in. This is a cons cell
looking like `(keymap . LIST-OF-BINDINGS)', LIST-OF-BINDINGS is initially
nil.
::end:: */
{
    return Fcons(Qkeymap, Qnil);
}

DEFUN("bind-keys", Fbind_keys, Sbind_keys, (repv args), rep_SubrN) /*
::doc:Sbind-keys::
bind-keys KEYMAP { EVENT-DESCRIPTION COMMAND }...

Adds key bindings to KEYMAP. Each EVENT-DESCRIPTION is a string naming an
event to bind to the corresponding COMMAND.

Returns KEYMAP when successful.
::end:: */
{
    repv km, arg1;
    if (!rep_CONSP(args))
	return rep_NULL;
    km = rep_CAR(args);
    args = rep_CDR(args);
    while (rep_CONSP(args) && rep_CONSP(rep_CDR(args)))
    {
	u_long code, mods;
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
::doc:Sunbind-keys::
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
	u_long code, mods;
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
::doc:Sgrab-keymap::
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
::doc:Sungrab-keymap::
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
::doc:Scurrent-event-string::
current-event-string

Returns the string which would have been inserted by the current event if
a Lisp function hadn't been called instead.
::end:: */
{
    KeySym ks;
    u_char buf[256];
    int len;

    if(current_x_event != 0)
	return Fsignal(Qerror, rep_LIST_1(rep_VAL(&not_in_handler)));

    len = XLookupString(&current_x_event->xkey,
			 buf, sizeof (buf) - 1, &ks, NULL);
    if(len > 0)
	return rep_string_dupn(buf, len);
    else
	return rep_null_string();
}

DEFUN("current-event", Fcurrent_event, Scurrent_event, (void), rep_Subr0) /*
::doc:Scurrent-event::
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

DEFUN("current-event-window", Fcurrent_event_window, Scurrent_event_window,
      (void), rep_Subr0) /*
::doc:Scurrent-event-window::
current-event-window

Return the window that received the current event, or the symbol
`root', or nil if no such window.
::end:: */
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
	    return rep_VAL(w);
	else if (current_x_event->xany.window == root_window)
	    return Qroot;
	else
	    return Qnil;
    }
    else
	return Qnil;
}

DEFUN("proxy-current-event", Fproxy_current_event, Sproxy_current_event,
      (repv win, repv mask, repv prop), rep_Subr3) /*
::doc:Sproxy-current-event::
proxy-current-event WINDOW [MASK] [PROPAGATE]

Send the current X event to WINDOW, either a window object, a numeric
window id, or the symbol `root'. If a ButtonPress event the pointer
grab will be released first.
::end:: */
{
    Window w = x_win_from_arg (win);
    if (w == 0)
	return rep_signal_arg_error (win, 1);

    if (current_x_event != 0)
    {
	long e_mask = (rep_INTP(mask) ? rep_INT(mask)
		       : get_event_mask (current_x_event->type));
	if (current_x_event->type == ButtonPress)
	    XUngrabPointer (dpy, last_event_time);
	XSendEvent (dpy, w, prop == Qnil ? False : True,
		    e_mask, current_x_event);
	return Qt;
    }
    else
	return Qnil;
}

DEFUN("allow-events", Fallow_events, Sallow_events, (repv mode), rep_Subr1) /*
::doc:Sallow-events::
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
::doc:Slast-event::
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
::doc:Sevent-name::
event-name EVENT

Returns a string naming the event EVENT.
::end:: */
{
    u_char buf[256];
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
::doc:Slookup-event::
lookup-event EVENT-NAME

Return the event whose name is EVENT-NAME.
::end:: */
{
    u_long code, mods;
    rep_DECLARE1(name, rep_STRINGP);

    if(lookup_event(&code, &mods, rep_STR(name)))
	return MAKE_EVENT(rep_MAKE_INT(code), rep_MAKE_INT(mods));
    else
	return Qnil;
}

DEFUN("lookup-event-binding", Flookup_event_binding, Slookup_event_binding, (repv ev), rep_Subr1) /*
::doc:Slookup-event-binding::
lookup-event-binding EVENT

Return the command currently associated with the event EVENT.

Note that `currently associated' means that the currently active set of
keymaps is used to resolve the binding. This means the window and
frame-part that received the current event for pointer events, or the
currently focused window for keyboard events.
::end:: */
{
    repv res, context = Qnil;
    u_long code, mods;

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
::doc:Ssearch-keymap::
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

DEFUN("keymapp", Fkeymapp, Skeymapp, (repv arg), rep_Subr1) /*
::doc:Skeymapp::
keymapp ARG

Returns t if ARG can be used as a keymap.
::end:: */
{
    return (rep_CONSP(arg) && rep_CAR(arg) == Qkeymap) ? Qt : Qnil;
}

DEFUN("eventp", Feventp, Seventp, (repv arg), rep_Subr1) /*
::doc:Seventp::
eventp ARG

Returns t if the ARG is an input event.
::end:: */
{
    return EVENTP(arg) ? Qt : Qnil;
}

DEFUN("eval-key-release-events", Veval_key_release_events,
      Seval_key_release_events, (repv arg), rep_Var)
{
    if (arg != rep_NULL)
	eval_key_release_events = (arg != Qnil);
    return eval_key_release_events ? Qt : Qnil;
}

DEFUN("eval-modifier-events", Veval_modifier_events,
      Seval_modifier_events, (repv arg), rep_Var)
{
    if (arg != rep_NULL)
	eval_modifier_events = (arg != Qnil);
    return eval_modifier_events ? Qt : Qnil;
}



/* Find the lisp modifier mask used by the meta and alt keys. This code
   shamelessly stolen from Emacs 19. :-) */

DEFSTRING(meta_l, "Meta_L");
DEFSTRING(meta_r, "Meta_R");
DEFSTRING(alt_l, "Alt_L");
DEFSTRING(alt_r, "Alt_R");

static void
find_meta(void)
{
    int min_code, max_code;
    KeySym *syms;
    int syms_per_code;
    XModifierKeymap *mods;
    repv meta_syms = Qnil, alt_syms = Qnil;

#if defined (XlibSpecificationRelease) && XlibSpecificationRelease >= 4
    XDisplayKeycodes(dpy, &min_code, &max_code);
#else
    min_code = dpy->min_keycode;
    max_code = dpy->max_keycode;
#endif

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

		    case XK_Num_Lock:
			num_lock_mod = 1 << row;
			break;
		    }
		}
	    }
	}
    }

    XFree((char *)syms);
    XFreeModifiermap(mods);

    if (meta_mod == 0 && alt_mod == 0)
	return;

    if (meta_mod == 0)
	meta_mod = alt_mod;

    if (alt_mod == 0)
	alt_mod = meta_mod;

    if (meta_mod == alt_mod)
    {
	meta_syms = Fnconc (rep_list_2 (meta_syms, alt_syms));
	alt_syms = meta_syms;
    }

    Fset (Qmeta_keysyms, meta_syms);
    Fset (Qalt_keysyms, alt_syms);
}


/* Key and button grabbing */

static void
grab_event (Window grab_win, repv ev)
{
    switch (rep_INT(EVENT_MODS(ev)) & EV_TYPE_MASK)
    {
	u_int code, state;
	int i;

    case EV_TYPE_KEY:
	if (translate_event_to_x_key (ev, &code, &state))
	{
	    if (state != AnyModifier)
	    {
		for (i = 0; i < 4; i++)
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
		for (i = 0; i < 4; i++)
		{
		    XGrabButton (dpy, code, state | all_lock_combs[i],
				 grab_win, False,
				 ButtonPressMask | ButtonReleaseMask,
				 GrabModeSync, GrabModeSync, None, None);
		}
	    }
	    else
	    {
		/* sawmill treats mouse buttons as modifiers, not as
		   codes, so for us AnyModifier includes all buttons.. */
		for (i = 0; i < 5; i++)
		{
		    XGrabButton (dpy, all_buttons[i], AnyModifier,
				 grab_win, False, ButtonPressMask
				 | ButtonReleaseMask, GrabModeSync,
				 GrabModeSync, None, None);
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
	u_int code, state;
	int i;

    case EV_TYPE_KEY:
	if (translate_event_to_x_key (ev, &code, &state))
	{
	    if (state != AnyModifier)
	    {
		for (i = 0; i < 4; i++)
		XUngrabKey (dpy, code, state | all_lock_combs[i], grab_win);
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
		for (i = 0; i < 4; i++)
		{
		    XUngrabButton (dpy, code,
				   state | all_lock_combs[i], grab_win);
		}
	    }
	    else
	    {
		for (i = 0; i < 5; i++)
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
	if (w->id != 0)
	{
	    repv tem = Fwindow_get (rep_VAL(w), Qkeymap);
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
    if (tem != Qnil && !rep_VOIDP(tem))
	grab_keymap_events (w->id, tem, grab);
    tem = Fwindow_get (rep_VAL(w), Qkeymap);
    if (tem && tem != Qnil)
	grab_keymap_events (w->id, tem, grab);
}


/* initialisation */

void
keys_init(void)
{
    rep_INTERN(global_keymap);
    rep_INTERN(root_window_keymap);
    rep_INTERN(override_keymap);
    rep_INTERN(unbound_key_hook);
    rep_INTERN(keymap);

    rep_ADD_SUBR(Smake_keymap);
    rep_ADD_SUBR(Sbind_keys);
    rep_ADD_SUBR(Sunbind_keys);
    rep_ADD_SUBR(Sgrab_keymap);
    rep_ADD_SUBR(Sungrab_keymap);
    rep_ADD_SUBR(Scurrent_event_string);
    rep_ADD_SUBR(Scurrent_event);
    rep_ADD_SUBR(Scurrent_event_window);
    rep_ADD_SUBR(Sproxy_current_event);
    rep_ADD_SUBR(Sallow_events);
    rep_ADD_SUBR(Slast_event);
    rep_ADD_SUBR(Sevent_name);
    rep_ADD_SUBR(Slookup_event);
    rep_ADD_SUBR(Slookup_event_binding);
    rep_ADD_SUBR(Ssearch_keymap);
    rep_ADD_SUBR(Skeymapp);
    rep_ADD_SUBR(Seventp);

    rep_ADD_SUBR(Seval_key_release_events);
    rep_ADD_SUBR(Seval_modifier_events);

    rep_INTERN(async_pointer);
    rep_INTERN(async_keyboard);
    rep_INTERN(sync_pointer);
    rep_INTERN(sync_keyboard);
    rep_INTERN(replay_pointer);
    rep_INTERN(replay_keyboard);
    rep_INTERN(sync_both);
    rep_INTERN(async_both);

    rep_INTERN(meta_keysyms);
    Fset (Qmeta_keysyms, Qnil);
    rep_INTERN(alt_keysyms);
    Fset (Qalt_keysyms, Qnil);

    if (rep_SYM(Qbatch_mode)->value == Qnil)
	find_meta ();

    all_lock_combs[2] |= num_lock_mod;
    all_lock_combs[3] |= num_lock_mod;
}
