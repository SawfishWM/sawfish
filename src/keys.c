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

#include "debug.h"
#include "debug-colors.h"
int debug_keys;

/* max number of milliseconds between successive-clicks */
#define DEFAULT_DOUBLE_CLICK_TIME 250

/* current_event holds the event we're processing (or 0s), last_event
   contains the previously processed event.  */
unsigned long current_event[2], last_event[2];

/* print_prefix means echo all events upto the end of the key-sequence.
   printed_this_prefix says the last event has been echoed. */
static bool print_prefix, printed_this_prefix;

/* Buffer holding the events making this key-sequence. */
#define EVENT_BUFSIZ 20
static unsigned long event_buf[EVENT_BUFSIZ]; /* one event = (code,mods) */
static int event_index;

/* Data for testing double-clicks */
Time last_click;
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

DEFSYM(display_message, "display-message");
DEFSYM(call_command, "call-command");

static repv next_keymap_path;

DEFSYM(multi_click_delay, "multi-click-delay");

/* The X modifiers being used for Meta, Alt, Hyper, Super.
   Values are bits in key/button event -> state field. */
unsigned long meta_mod, alt_mod, hyper_mod, super_mod;

extern int button_num;
extern int all_lock_mask;
extern struct key_def default_mods[];

DEFSYM(grab_counter, "grab-counter");
extern repv Qsync_keyboard;

/* The user-customizable modifier; used for default key bindings. This
   shouldn't include any bits that don't have a fixed meaning. */

unsigned long wm_mod = EV_MOD_META;

/* Button number support */
unsigned int ev_mod_button_mask;
unsigned int state_mask;


/* Translate from X 'state' to names like "super". */
unsigned long
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
bool
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
	*mods &= state_mask;
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
	    *code = EV_CODE_MOUSE_OFF2;
	    break;

	case 3:
	    *code = EV_CODE_MOUSE_OFF3;
	    break;

	default:
	    *code = EV_CODE_MOUSE_OFF1;
	}

    button:
	*mods = xev->xbutton.state & ~all_lock_mask;
	*mods &= state_mask;
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
	case Button6:
	    *mods |= Button6Mask;
	    break;
	case Button7:
	    *mods |= Button7Mask;
	    break;
	case Button8:
	    *mods |= Button8Mask;
	    break;
	}
	ret = TRUE;
	break;

    case MotionNotify:
	*code = EV_CODE_MOUSE_MOVE;
	*mods = xev->xmotion.state & ~all_lock_mask;
	*mods &= state_mask;
	*mods |= EV_TYPE_MOUSE;
	ret = TRUE;
	break;
    }

    if (ret)
	*mods = indirect_modifiers (*mods);

    return ret;
}

/* Keymap searching */

/* Used by search_keymap() and `event-match'.
   Notice the asymmetry between 1 and 2.
 */
bool
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

/* If the keymap is a symbol -> get its value; then  "recurse". */
repv
resolve_keymap(repv keymap)
{
   while(rep_SYMBOLP(keymap) && !rep_NILP(keymap) && !rep_INTERRUPTP)
   {
       repv tem = Fsymbol_value(keymap, Qt);
       if(tem == keymap)
           break;
       keymap = tem;
       rep_TEST_INT;
   }
   return keymap;
}

/* Search the keymap KM for a binding of CODE&MODS.
   If CALLBACK is non-nil it's a function to call for the binding found.
   If this function returns true, then this binding is acceptable and
   is returned from the function. */
repv
search_keymap(repv km, unsigned long code, unsigned long mods, bool (*callback)(repv key))
{
    /* If it's a symbol, dereference it. */
    km = resolve_keymap(km);

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

repv
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

/* If something goes wrong, the input should go a window, which has all keys grabbed */
void
ungrab_into_focused_window (void)
{
    if (focus_request.sent)
    {
        if (debug_keys)
            DB(("not ungrabbing immediately, will wait for FocusIn\n"));
        /* fixme: I should remark that we keep grab! */
        focus_request.grabbed = TRUE;
    }
    else
    {
        if (debug_keys)
            DB(("no need for commit_queued_focus_change (no change in focus)-> we can simply: Fungrab_keyboard\n"));
        Fungrab_keyboard ();
    }
}

/* Process the event CODE+MODS. OS-INPUT-MSG is the raw input event
   from the window-system, this is only used to cook a string from.  */
repv
eval_input_event(repv context_map)
{
    unsigned long code, mods;
    repv result = Qnil, cmd, orig_next_keymap_path = next_keymap_path;

    if (current_x_event->type == KeyRelease &&
       (global_symbol_value (Qeval_key_release_events) == Qnil))
    {
        /* we need the next event (instead of this one). */
        Fallow_events(Qsync_keyboard);
        return Qnil;
    }

    if (!translate_event (&code, &mods, current_x_event))
    {
        /* this should mean that we wanted something else */
        Fallow_events(Qsync_keyboard);
        return Qnil;
    }

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

        if (rep_INT(global_symbol_value(Qgrab_counter)) == 0)
            ungrab_into_focused_window();
        else
        {
            Fallow_events(Qsync_keyboard);
        }
    }
    else if(next_keymap_path != rep_NULL)
    {
	/* We already handled some prefixes. */
	Fset (Qthis_command, Qkeymap);
	result = Qnil;

	/* Grab the input devices for the next event */
	Fgrab_keyboard (focus_window ? rep_VAL(focus_window) : Qnil, Qt, Qt);
        Fallow_events(Qsync_keyboard);
    }
    else if(orig_next_keymap_path != rep_NULL
	    && orig_next_keymap_path != Qglobal_keymap)
    {
	/* A multi-key binding, but no final step; clear the prefix
	   argument for the next command and beep. */
	Fset (Qprefix_arg, Qnil);
	Fbeep();
        Fungrab_keyboard();
    }
    else
    {
	/* An unbound key with no prefix keys. */

	repv hook = global_symbol_value (Qunbound_key_hook);
	if (!rep_VOIDP (hook) && hook != Qnil)
        {
            /* Lisp must handle  allow-event / ungrab-keyboard ! */
            result = Fcall_hook(Qunbound_key_hook, Qnil, Qor);

            if (!(rep_INT(global_symbol_value(Qgrab_counter))))
                ungrab_into_focused_window();
            else
                if (!rep_throw_value)
                    Fallow_events (Qsync_keyboard);
                else
                    DB(("%s: lisp throwing, we don't allow_events now.\n", __FUNCTION__));
        }
        /* fixme! oh this seems serious! But i think it never happened to me.*/
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
    {};
    

    last_event[0] = current_event[0];
    last_event[1] = current_event[1];

    /* Only lose the current event when in the topmost event loop. */
    if (rep_recurse_depth == 0)
	current_event[0] = current_event[1] = 0;

    if (print_prefix)
	print_event_prefix ();

    return result;
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

DEFUN("keymapp", Fkeymapp, Skeymapp, (repv arg), rep_Subr1) /*
::doc:sawfish.wm.events#keymapp::
keymapp ARG

Returns t if ARG can be used as a keymap.
::end:: */
{
    return (rep_CONSP(arg) && rep_CAR(arg) == Qkeymap) ? Qt : Qnil;
}

/* initialisation */

static void button_num_init(void){
  if(rep_get_option("--5-buttons", 0)){
    button_num = 5;
    ev_mod_button_mask = (Button1Mask | Button2Mask | Button3Mask \
			  | Button4Mask | Button5Mask);
    state_mask = (1 << 13) - 1;
    {
      /* delete Button6 - 8 entries from default_mods[] */
      int i, j = 0, k;
      char str[10];
      for ( i = 6; i <= 8; i++){
	snprintf(str, 8, "Button%d", i);
	for (j = 0; default_mods[j].name != 0; j++){
	  if ( strncmp(default_mods[j].name, str, 8) == 0){
	    for( k = j; default_mods[k].name != 0; k++){
	      default_mods[k] = default_mods[k + 1];
	    }
	  }
	}
      }
    }
  }else{
    button_num = 8;
    ev_mod_button_mask = (Button1Mask | Button2Mask | Button3Mask   \
			  | Button4Mask | Button5Mask | Button6Mask \
			  | Button7Mask | Button8Mask );
    state_mask = 0xffffffff;
  }
}

void
keys_init(void)
{
    button_num_init();

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
    rep_ADD_SUBR(Skeymapp);
    rep_pop_structure (tem);

    rep_INTERN_SPECIAL(multi_click_delay);
    Fset (Qmulti_click_delay, rep_MAKE_INT(DEFAULT_DOUBLE_CLICK_TIME));
    rep_INTERN_SPECIAL(grab_counter);
    Fset (Qgrab_counter, rep_MAKE_INT(0));

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
 
    if (!batch_mode_p ())
	update_keyboard_mapping ();
}
