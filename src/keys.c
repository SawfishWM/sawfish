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

#define KEYTAB_HASH_FUN(c,m) ((c) * 33)
#define KEYTAB_SIZE 127

/* max number of milliseconds between double-clicks */
#define DOUBLE_CLICK_TIME 250

/* current_event holds the event we're processing (or 0s), last_event
   contains the previously processed event.  */
static u_long current_event[2], last_event[2];

/* Data for testing double-clicks */
static Time last_click;
static u_long last_click_button;

DEFSYM(global_keymap, "global-keymap");
DEFSYM(root_window_keymap, "root-window-keymap");
DEFSYM(override_keymap, "override-keymap");
DEFSYM(unbound_key_hook, "unbound-key-hook");
DEFSYM(keymap, "keymap");

/* The X modifier being used for Meta */
static u_long meta_mod;

static void grab_keymap_event (repv km, long code, long mods, bool grab);


/* Translate from X events to Lisp events */

/* Translate the X key or button event XEV to *CODE and *MODS */
static void
translate_event(u_long *code, u_long *mods, XEvent *xev)
{
    switch(xev->type)
    {
    case KeyPress:
	*mods = xev->xkey.state;
	if(*mods & ShiftMask)
	{
	    /* Some keys don't have keysym at index 1, if not treat it as
	       normal keysym shifted.  */
	    *code = XKeycodeToKeysym(xev->xany.display, xev->xkey.keycode, 1);
	    if(*code == NoSymbol)
		*code = XKeycodeToKeysym(xev->xany.display,
					 xev->xkey.keycode, 0);
	    else
		*mods &= ~ShiftMask;
	}
	else
	    *code = XKeycodeToKeysym(xev->xany.display, xev->xkey.keycode, 0);
	if((*code != NoSymbol) && !IsModifierKey(*code))
	    *mods |= EV_TYPE_KEY;
	break;

    case ButtonPress:
	if(xev->xbutton.button == last_click_button
	   && xev->xbutton.time < (last_click + DOUBLE_CLICK_TIME))
	{
	    *code = EV_CODE_MOUSE_CLICK2;
	}
	else
	    *code = EV_CODE_MOUSE_CLICK1;
	last_click = xev->xbutton.time;
	last_click_button = xev->xbutton.button;
	goto button;

    case ButtonRelease:
	*code = EV_CODE_MOUSE_UP;
    button:
	*mods = xev->xbutton.state;
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
	break;

    case MotionNotify:
	*code = EV_CODE_MOUSE_MOVE;
	*mods = xev->xmotion.state;
	*mods |= EV_TYPE_MOUSE;
	break;
    }
}

/* Translate the Lisp key event EV to X keycode *KEYCODE and modifier
   mask *STATE, returning true if successful. */
static bool
translate_event_to_x_key (repv ev, u_int *keycode, u_int *state)
{
    if (rep_INT(EVENT_MODS(ev)) & EV_TYPE_KEY)
    {
	*keycode = XKeysymToKeycode (dpy, rep_INT(EVENT_CODE(ev)));
	*state = rep_INT(EVENT_MODS(ev)) & EV_MOD_MASK;
	if (*state == EV_MOD_ANY)
	    *state = AnyModifier;
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
		*button = buttons[i].button;
		mods &= ~buttons[i].mask;
		*state = mods & EV_MOD_MASK;
		if (*state == EV_MOD_ANY)
		    *state = AnyModifier;
		return TRUE;
	    }
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
    if(rep_VECTORP(km))
    {
	if(rep_VECT_LEN(km) != KEYTAB_SIZE)
	    return rep_NULL;
	km = rep_VECTI(km, KEYTAB_HASH_FUN(code, mods) % KEYTAB_SIZE);
    }
    else if rep_CONSP(km)
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
	       repv context_keymap)
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

	if (!k && focus_window)
	{
	    /* 2. search focused window keymap property */
	    tem = Fwindow_get (rep_VAL(focus_window), Qkeymap);
	    if (tem && tem != Qnil)
		k = search_keymap(tem, code, mods, callback);
	}
	if(!k)
	    /* 3. search global-keymap */
	    k = search_keymap(Qglobal_keymap, code, mods, callback);
    }

    return (k != rep_NULL && KEYP(k)) ? KEY_COMMAND(k) : rep_NULL;
}

/* Process the event CODE+MODS. OS-INPUT-MSG is the raw input event
   from the window-system, this is only used to cook a string from.  */
repv
eval_input_event(repv context_map)
{
    u_long code, mods;
    repv result = Qnil, cmd;

    translate_event (&code, &mods, current_x_event);

    current_event[0] = code;
    current_event[1] = mods;
    cmd = lookup_binding(code, mods, 0, context_map);
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
    { "Shift",    ShiftMask },
    { "SFT",      ShiftMask },
    { "C",        ControlMask },
    { "Ctrl",     ControlMask },
    { "Control",  ControlMask },
    { "M",        EV_MOD_META },
    { "Meta",     EV_MOD_META },
    { "Mod1",     Mod1Mask },
    { "Mod2",     Mod2Mask },
    { "Mod3",     Mod3Mask },
    { "Mod4",     Mod4Mask },
    { "Button1",  Button1Mask },
    { "Button2",  Button2Mask },
    { "Button3",  Button3Mask },
    { "Button4",  Button4Mask },
    { "Button5",  Button5Mask },
    { "Any",      EV_MOD_ANY },
    { 0, 0 }
};

static struct key_def default_codes[] = {
    { "Click1",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click2",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK2 },
    { "Off",      EV_TYPE_MOUSE, EV_CODE_MOUSE_UP },
    { "PointerUp", EV_TYPE_MOUSE, EV_CODE_MOUSE_UP },
    { "Move",     EV_TYPE_MOUSE, EV_CODE_MOUSE_MOVE },
    { "PointerMove", EV_TYPE_MOUSE, EV_CODE_MOUSE_MOVE },

    { "SPC",      EV_TYPE_KEY, XK_space },
    { "Space",    EV_TYPE_KEY, XK_space },
    { "Spacebar", EV_TYPE_KEY, XK_space },
    { "TAB",      EV_TYPE_KEY, XK_Tab },
    { "RET",      EV_TYPE_KEY, XK_Return },
    { "Return",   EV_TYPE_KEY, XK_Return },
    { "ESC",      EV_TYPE_KEY, XK_Escape },
    { "Escape",   EV_TYPE_KEY, XK_Escape },
    { "BS",       EV_TYPE_KEY, XK_BackSpace },
    { "Backspace", EV_TYPE_KEY, XK_BackSpace },
    { "DEL",      EV_TYPE_KEY, XK_Delete },
    { "Delete",   EV_TYPE_KEY, XK_Delete },
    { "Help",     EV_TYPE_KEY, XK_Help },
    { "Up",       EV_TYPE_KEY, XK_Up },
    { "Down",     EV_TYPE_KEY, XK_Down },
    { "Right",    EV_TYPE_KEY, XK_Right },
    { "Left",     EV_TYPE_KEY, XK_Left },

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
    mods &= ~EV_TYPE_MASK;

    for(i = 0; i < 16 && mods != 0; i++)	/* magic numbers!? */
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

Return a new keymap suitable for storing bindings in. This is a 127
element vector, each element is an empty list of bindings for that hash
bucket. Compare with the make-sparse-keymap function.
::end:: */
{
    return Fmake_vector(rep_MAKE_INT(KEYTAB_SIZE), Qnil);
}

DEFUN("make-sparse-keymap", Fmake_sparse_keymap, Smake_sparse_keymap,
      (repv base), rep_Subr1) /*
::doc:Smake-sparse-keymap::
make-sparse-keymap [BASE-KEYMAP]

Return a new keymap suitable for storing bindings in. This is a cons cell
looking like `(keymap . LIST-OF-BINDINGS)', LIST-OF-BINDINGS is initially
nil.

If BASE-KEYMAP is non-nil, it should be an existing sparse keymap whose
bindings are to be inherited by the new keymap.
::end:: */
{
    if(!rep_NILP(base) && (!rep_CONSP(base) || rep_CAR(base) != Qkeymap))
	return rep_signal_arg_error(base, 1);

    return Fcons(Qkeymap, base);
}

DEFUN("bind-keys", Fbind_keys, Sbind_keys, (repv args), rep_SubrN) /*
::doc:Sbind-keys::
bind-keys KEYMAP { EVENT-DESCRIPTION COMMAND }...

Adds key bindings to KEYMAP. Each EVENT-DESCRIPTION is a string naming an
event to bind to the corresponding COMMAND.

Returns KEYMAP when successful.
::end:: */
{
    bool rc = TRUE;
    repv km, arg1, res = rep_NULL;
    if(!rep_CONSP(args))
	return rep_NULL;
    km = rep_CAR(args);
    args = rep_CDR(args);
    while(rc && rep_CONSP(args) && rep_CONSP(rep_CDR(args)))
    {
	u_long code, mods;
	repv key;
	arg1 = rep_CAR(args);
	args = rep_CDR(args);
	if(rep_STRINGP(arg1))
	{
	    if(!lookup_event(&code, &mods, rep_STR(arg1)))
		goto end;
	}
	else if(!rep_NILP(Feventp(arg1)))
	{
	    code = rep_INT(rep_CAR(arg1));
	    mods = rep_INT(rep_CDR(arg1));
	}
	else
	{
	    Fsignal(Qbad_event_desc, rep_LIST_1(arg1));
	    goto end;
	}
	rc = FALSE;
	key = MAKE_KEY(MAKE_EVENT(rep_MAKE_INT(code), rep_MAKE_INT(mods)), rep_CAR(args));
	if(key != rep_NULL)
	{
	    if(rep_VECTORP(km))
	    {
		u_long hash = KEYTAB_HASH_FUN(code, mods) % KEYTAB_SIZE;
		repv old = rep_VECTI(km, hash);
		rep_VECTI(km, hash) = Fcons(key, old);
	    }
	    else
		rep_CDR(km) = Fcons(key, rep_CDR(km));
	    args = rep_CDR(args);
	    grab_keymap_event (km, code, mods, TRUE);
	    rc = TRUE;
	}
	else
	    goto end;
    }
    if(rc)
	res = km;
end:
    return res;
}

DEFUN("unbind-keys", Funbind_keys, Sunbind_keys, (repv args), rep_SubrN) /*
::doc:Sunbind-keys::
unbind-keys KEY-MAP EVENT-DESCRIPTION...
::end:: */
{
    bool rc = TRUE;
    repv km, arg1, res = rep_NULL;
    if(!rep_CONSP(args))
	return rep_NULL;
    km = rep_CAR(args);
    if(!((rep_VECTORP(km) && rep_VECT_LEN(km) == KEYTAB_SIZE)
       || rep_CONSP(km)))
	return(rep_signal_arg_error(km, 1));
    args = rep_CDR(args);
    while(rc && rep_CONSP(args))
    {
	u_long code, mods;
	repv *keyp;
	arg1 = rep_CAR(args);
	if(rep_STRINGP(arg1))
	{
	    if(!lookup_event(&code, &mods, rep_STR(arg1)))
		goto end;
	}
	else if(!rep_NILP(Feventp(arg1)))
	{
	    code = rep_INT(rep_CAR(arg1));
	    mods = rep_INT(rep_CDR(arg1));
	}
	else
	{
	    Fsignal(Qbad_event_desc, rep_LIST_1(arg1));
	    goto end;
	}
	rc = FALSE;
	if(rep_VECTORP(km))
	    keyp = &rep_VECTI(km, KEYTAB_HASH_FUN(code, mods) % KEYTAB_SIZE);
	else
	    keyp = &rep_CDR(km);
	while(rep_CONSP(*keyp))
	{
	    repv cell = rep_CAR(*keyp);
	    if(rep_CONSP(cell))
	    {
		if((rep_INT(EVENT_MODS(KEY_EVENT(cell))) == mods)
		   && (rep_INT(EVENT_CODE(KEY_EVENT(cell))) == code))
		{
		    *keyp = rep_CDR(*keyp);
		    /* Keybindings are supposed to nest so only delete the
		    first entry for this event  */
		    break;
		}
		else
		    keyp = &rep_CDR(*keyp);
	    }
	    else
		/* An inherited keymap. Only delete bindings from
		   the initial keymap. */
		break;

	    rep_TEST_INT; if(rep_INTERRUPTP) return rep_NULL;
	}
	/* Do we ungrab this event? */
	{
	    repv tem = *keyp;
	    while (rep_CONSP(tem))
	    {
		repv cell = rep_CAR(tem);
		if(rep_CONSP(cell))
		{
		    if((rep_INT(EVENT_MODS(KEY_EVENT(cell))) == mods)
		       && (rep_INT(EVENT_CODE(KEY_EVENT(cell))) == code))
		    {
			/* A second binding. Don't ungrab. */
			break;
		    }
		}
		tem = rep_CDR(tem);
		rep_TEST_INT; if(rep_INTERRUPTP) return rep_NULL;
	    }
	    if (!rep_CONSP(tem))
		grab_keymap_event (km, code, mods, FALSE);
	}

	rc = TRUE;
	args = rep_CDR(args);
    }
    if(rc)
	res = Qt;
end:
    return(res);
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
::end:: */
{
    if (current_x_event != 0)
    {
	Lisp_Window *w = find_window_by_id (current_x_event->xany.window);
	return (w != 0) ? rep_VAL(w) : Qnil;
    }
    else
	return Qnil;
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

    if(lookup_event_name(buf, rep_INT(EVENT_CODE(ev)), rep_INT(EVENT_MODS(ev))))
	return rep_string_dup(buf);
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
::end:: */
{
    repv res;
    if(!EVENTP(ev))
	return(rep_signal_arg_error(ev, 1));

    /* XXX replace Qnil by context-sensitive keymap.. */
    res = lookup_binding(rep_INT(EVENT_CODE(ev)),
			 rep_INT(EVENT_MODS(ev)), 0, Qnil);
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
    res = search_keymap(km, rep_INT(EVENT_CODE(ev)), rep_INT(EVENT_MODS(ev)), 0);
    return res ? res : Qnil;
}

DEFUN("keymapp", Fkeymapp, Skeymapp, (repv arg), rep_Subr1) /*
::doc:Skeymapp::
keymapp ARG

Returns t if ARG can be used as a keymap.
::end:: */
{
    if((rep_VECTORP(arg) && rep_VECT_LEN(arg) == KEYTAB_SIZE)
       || (rep_CONSP(arg) && rep_CAR(arg) == Qkeymap))
	return Qt;
    else
	return Qnil;
}

DEFUN("eventp", Feventp, Seventp, (repv arg), rep_Subr1) /*
::doc:Seventp::
eventp ARG

Returns t if the ARG is an input event.
::end:: */
{
    return EVENTP(arg) ? Qt : Qnil;
}


/* Return the lisp modifier mask used as the meta key. This code
   shamelessly stolen from Emacs 19. :-) */
static u_long
find_meta(void)
{
    u_long meta_mod = 0, alt_mod = 0;

    int min_code, max_code;
    KeySym *syms;
    int syms_per_code;
    XModifierKeymap *mods;

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
			break;

		    case XK_Alt_L: case XK_Alt_R:
			alt_mod = 1 << row;
			break;
		    }
		}
	    }
	}
    }

    if(meta_mod == 0)
	meta_mod = alt_mod;

    XFree((char *)syms);
    XFreeModifiermap(mods);

    return meta_mod;
}


/* Key and button grabbing

   XXX There's a big problem with this -- if the keymap changes after
   XXX the grabs have been made the changes are propagated to the set
   XXX of passive grabs.. */

static void
grab_event (Window grab_win, repv ev)
{
    switch (rep_INT(EVENT_MODS(ev)) & EV_TYPE_MASK)
    {
	u_int code, state;

    case EV_TYPE_KEY:
	if (translate_event_to_x_key (ev, &code, &state))
	{
	    XGrabKey (dpy, code, state, grab_win,
		      False, GrabModeAsync, GrabModeAsync);
	}
	break;

    case EV_TYPE_MOUSE:
	if (translate_event_to_x_button (ev, &code, &state))
	{
	    XGrabButton (dpy, code, state, grab_win,
			 False, ButtonPressMask | ButtonReleaseMask,
			 GrabModeAsync, GrabModeAsync, None, None);
	}
    }
}

static void
ungrab_event (Window grab_win, repv ev)
{
    switch (rep_INT(EVENT_MODS(ev)) & EV_TYPE_MASK)
    {
	u_int code, state;

    case EV_TYPE_KEY:
	if (translate_event_to_x_key (ev, &code, &state))
	    XUngrabKey (dpy, code, state, grab_win);
	break;

    case EV_TYPE_MOUSE:
	if (translate_event_to_x_button (ev, &code, &state))
	    XUngrabButton (dpy, code, state, grab_win);
    }
}

static void
grab_keymap_event (repv km, long code, long mods, bool grab)
{
    Lisp_Window *w;
    repv ev = MAKE_EVENT(rep_MAKE_INT(code), rep_MAKE_INT(mods));
    repv global = Fsymbol_value (Qglobal_keymap, Qt);
    for (w = window_list; w != 0; w = w->next)
    {
	if (w->id != 0)
	{
	    repv tem = Fwindow_get (rep_VAL(w), Qkeymap);
	    if (km == global || tem == km)
		(grab ? grab_event : ungrab_event) (w->id, ev);
	}
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
    else if (rep_VECTORP(keymap))
    {
	int i;
	for (i = 0; i < rep_VECT_SIZE(keymap); i++)
	    grab_keylist_events (grab_win, rep_VECTI(keymap, i), grab);
    }
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
    rep_ADD_SUBR(Smake_sparse_keymap);
    rep_ADD_SUBR(Sbind_keys);
    rep_ADD_SUBR(Sunbind_keys);
    rep_ADD_SUBR(Scurrent_event_string);
    rep_ADD_SUBR(Scurrent_event);
    rep_ADD_SUBR(Scurrent_event_window);
    rep_ADD_SUBR(Slast_event);
    rep_ADD_SUBR(Sevent_name);
    rep_ADD_SUBR(Slookup_event);
    rep_ADD_SUBR(Slookup_event_binding);
    rep_ADD_SUBR(Ssearch_keymap);
    rep_ADD_SUBR(Skeymapp);
    rep_ADD_SUBR(Seventp);

    meta_mod = find_meta ();
}
