/* Keeping the X key grabs. Binding */

#include "sawfish.h"
#include "keys.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

#include "debug.h"
#include "debug-colors.h"

DEFSTRING(meta_l, "Meta_L");
DEFSTRING(meta_r, "Meta_R");
DEFSTRING(alt_l, "Alt_L");
DEFSTRING(alt_r, "Alt_R");
DEFSTRING(hyper_l, "Hyper_L");
DEFSTRING(hyper_r, "Hyper_R");
DEFSTRING(super_l, "Super_L");
DEFSTRING(super_r, "Super_R");

static void grab_keymap_event (repv km, long code, long mods, bool grab);
static void grab_all_keylist_events (repv map, bool grab);

/* back in keys.c */
extern unsigned long meta_mod, alt_mod, hyper_mod, super_mod;
extern unsigned long wm_mod;
/* The X modifiers bound to the Num_Lock and Scroll_Lock keysyms */
static unsigned long num_lock_mod, scroll_lock_mod;

/*
  locks: currently LockMask, num_lock, and scroll_lock.
  These are used in grabbing. All combinations of lock masks is stored.
*/
static int total_lock_combs;
int all_lock_mask;
static int all_lock_combs[2*2*2];

int button_num;
extern unsigned int ev_mod_button_mask;
static int all_buttons[9] = { Button1, Button2, Button3, Button4, Button5, Button6, Button7, Button8 };

extern unsigned long indirect_modifiers (unsigned long mods);
extern repv resolve_keymap(repv keymap);
extern repv search_keymap(repv km, unsigned long code, unsigned long mods,
                          bool (*callback)(repv key));

DEFSYM(meta_keysyms, "meta-keysyms");
DEFSYM(alt_keysyms, "alt-keysyms");
DEFSYM(hyper_keysyms, "hyper-keysyms");
DEFSYM(super_keysyms, "super-keysyms");

/*
  Translate modifiers back to raw X 'state' from names like "super".
 */
static unsigned long
direct_modifiers (unsigned long mods)
{
    /* mmc: wm_mod is the bit in X modmap. */
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

/* why not another struct for modifiers? */
struct key_def default_mods[] = {
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
    { "Button6",  Button6Mask },
    { "Button7",  Button7Mask },
    { "Button8",  Button8Mask },
    { "Any",      EV_MOD_ANY },
    { "Release",  EV_MOD_RELEASE },
    { 0, 0 }
};

static struct key_def default_codes[] = {
    { "Click",    EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click1",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK1 },
    { "Click2",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK2 },
    { "Click3",   EV_TYPE_MOUSE, EV_CODE_MOUSE_CLICK3 },
    { "Off",      EV_TYPE_MOUSE, EV_CODE_MOUSE_OFF1 },
    { "Off1",     EV_TYPE_MOUSE, EV_CODE_MOUSE_OFF1 },
    { "Off2",     EV_TYPE_MOUSE, EV_CODE_MOUSE_OFF2 },
    { "Off3",     EV_TYPE_MOUSE, EV_CODE_MOUSE_OFF3 },
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
bool
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
		    && (*mods & (ev_mod_button_mask | EV_MOD_ANY)) == 0)
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
bool
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

Grab any events in KEYMAP on windows, including root, but not
frame parts. This function is useful after keymap changes.
::end:: */
{
    rep_DECLARE1(map, rep_CONSP);
    grab_all_keylist_events (map, TRUE);
    return map;
}

DEFUN("ungrab-keymap", Fungrab_keymap, Sungrab_keymap, (repv map), rep_Subr1)/*
::doc:sawfish.wm.events#ungrab-keymap::
ungrab-keymap KEYMAP

Ungrab any events in KEYMAP on windows, including root, but not
frame parts. This function is useful before keymap changes.
::end:: */
{
    rep_DECLARE1(map, rep_CONSP);
    grab_all_keylist_events (map, FALSE);
    return map;
}

/* Translate the Lisp key event EV to X keycode *KEYCODE and modifier
   mask *STATE, returning true if successful.
   Used by grab and sythesize-event.
*/
bool
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
   modifier mask *STATE, returning true if successful.
   It doesn't distinguish Click and Button-off, by its nature.
   Used by synthesize-event and grab.
*/
unsigned int
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
	    { Button6, Button6Mask },
	    { Button7, Button7Mask },
	    { Button8, Button8Mask },
	    { 0, 0 }
	};
	int i;

	for (i = 0; i < button_num  ; i++)
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

static void
nconc (repv x, repv y)
{
    repv *ptr = &x;

    while (rep_CONSP (*ptr))
	ptr = rep_CDRLOC (*ptr);

    *ptr = y;
}

/* Find the lisp modifier mask used by the meta and alt keys. This code
   shamelessly stolen from Emacs 19. :-) */
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

    if (debug_keys)
        DB(("%s: -> XGetKeyboardMapping\n", __FUNCTION__));
   
    syms = XGetKeyboardMapping(dpy, min_code, max_code - min_code + 1,
			       &syms_per_code);
    if (!syms)
    {
        DB(("XGetKeyboardMapping failed\n"));
        exit(-1);
    }
    mods = XGetModifierMapping(dpy);

    if (debug_keys)
        DB(("XGetModifierMapping: max_keypermod: %d \n", mods->max_keypermod));
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
                        if (debug_keys)
                            DB(("\tfound Meta: on keycode: %d, sym %d, -> bit %d, %d-th\n",
                                code, sym, row, col));
                        /* found the Real modifier for META */
                        meta_mod = 1 << row; 
			meta_syms = Fcons (sym == XK_Meta_L ? rep_VAL(&meta_l)
					   : rep_VAL(&meta_r), meta_syms);
			break;

		    case XK_Alt_L: case XK_Alt_R:
                     if (debug_keys)                              
                       DB(("\tfound Alt: on keycode: %d, sym %d, -> bit %d, %d-th\n", code, sym, row, col));
			alt_mod = 1 << row;
			alt_syms = Fcons (sym == XK_Alt_L ? rep_VAL(&alt_l)
					  : rep_VAL(&alt_r), alt_syms);
			break;

		    case XK_Hyper_L: case XK_Hyper_R:
                     if (debug_keys)
                       DB(("\tfound Hyper: on keycode: %d, sym %d, -> bit %d, %d-th\n", code, sym, row, col));
			hyper_mod = 1 << row;
			hyper_syms = Fcons (sym == XK_Hyper_L
					    ? rep_VAL(&hyper_l)
					    : rep_VAL(&hyper_r), hyper_syms);
			break;

		    case XK_Super_L: case XK_Super_R:
                     if (debug_keys)
                       DB(("\tfound super: on keycode: %d, sym %d, -> bit %d, %d-th\n", code, sym, row, col));
			super_mod = 1 << row;
			super_syms = Fcons (sym == XK_Super_L
					    ? rep_VAL(&super_l)
					    : rep_VAL(&super_r), super_syms);
			break;

		    case XK_Num_Lock:
                     if (debug_keys)
                       DB(("\tfound numlock: on keycode: %d, sym %d, -> bit %d, %d-th\n", code, sym, row, col));
			num_lock_mod = 1 << row;
			break;

		    case XK_Scroll_Lock:
                     if (debug_keys)
                       DB(("\tfound scroll-loc: on keycode: %d, sym %d, -> bit %d, %d-th\n", code, sym, row, col));
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
    /* Store the current (i.e. old values) */
    unsigned long  x_meta_mod, x_alt_mod, x_hyper_mod, x_super_mod;

    x_meta_mod = meta_mod;
    x_alt_mod = alt_mod;
    x_hyper_mod = hyper_mod;
    x_super_mod = super_mod;

    /* now find the new ones: */
    find_meta ();
    build_lock_mods ();

    /* if anything changed, regrab: */
    if (! ( (x_meta_mod == meta_mod)
           && (x_alt_mod == alt_mod)
           && (x_hyper_mod == hyper_mod)
           && (x_super_mod == super_mod)))
    {
        Lisp_Window *w;
         
        /* mmc: on start up also! why?*/
        DB(("Some modifiers changed. We have to ungrab & regrab all keys on all windows\n"));
         
        /* I would like to do it lazily. That is only when a window is focused! */
        Fgrab_keyboard (Qnil, Qt, Qt);
        for (w = window_list; w != 0; w = w->next)
        {
            if (!WINDOW_IS_GONE_P (w))
            {
                grab_window_events (w, FALSE);
                /* re-grab! */
                grab_window_events (w, TRUE);
            }
        }
        Fungrab_keyboard ();
    }
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
		for (i = 0; i < button_num; i++)
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
		for (i = 0; i < button_num; i++)
		  {
		    XUngrabButton (dpy, all_buttons[i], AnyModifier, grab_win);
		  }
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
   if (debug_keys & DB_KEYS_GRAB)
      DB(("%s: %s\n", __FUNCTION__, grab?"grab": "ungrab"));
   for (w = window_list; w != 0; w = w->next) /* in ALL windows ??? */
    {
	if (!WINDOW_IS_GONE_P (w))
	{
	    repv tem = Fwindow_get (rep_VAL(w), Qkeymap, Qnil);
	    if (rep_SYMBOLP(tem) && tem != Qnil)
		tem = Fsymbol_value (tem, Qt);

            /* If this keymap concerns the window ..*/
            if (km == global || tem == km)
            {
                /* .. If a key+modifier is not in one of the 2 relevant keymaps,
                 * it's NOT sufficient to conclude it should be ungrabbed.
                 * It's OR, so we have to test both keymaps! so here the 2nd one:
                 * How about W- vs. other modifiers? it's also OR. */
                if (((km == global) ?
                     search_keymap(tem, code, mods, 0) :
                     (search_keymap(global, code,  mods, 0))))
                {
                    DB(("%s: not Xungrabbing! It is still in %s keymap.\n",
                        __FUNCTION__,
                        (km == global) ? "local" : "global"));
                }
                else
                {
                    (grab ? grab_event : ungrab_event) (w->id, ev);
                }
            }
	}
    }
}

/* Implements `grab-keymap' and `ungrab-keymap', i.e.,
   grab all keys registered in keymap `map'. */
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
    keymap = resolve_keymap(keymap);
    if (rep_CONSP(keymap))
	grab_keylist_events (grab_win, rep_CDR(keymap), grab);
}

/* Grab all bound events in client window W. */
void
grab_window_events (Lisp_Window *w, bool grab)
{
    if (grab == FALSE) {
        if (!WINDOW_IS_GONE_P (w))
            /* mmc: one of my first improvements.... */
            XUngrabKey (dpy, AnyKey,AnyModifier, w->id);
    } else {
        repv tem;

        tem = Fsymbol_value (Qglobal_keymap, Qt);
        if (tem != Qnil && !rep_VOIDP(tem) && !WINDOW_IS_GONE_P (w))
            grab_keymap_events (w->id, tem, grab);
        tem = Fwindow_get (rep_VAL(w), Qkeymap, Qnil);
        if (tem && tem != Qnil && !WINDOW_IS_GONE_P (w))
            grab_keymap_events (w->id, tem, grab);
    }
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
key_bind_init(void)
{
    repv tem;

    tem = rep_push_structure ("sawfish.wm.events");
    rep_ADD_SUBR(Sbind_keys);
    rep_ADD_SUBR(Sunbind_keys);

    rep_ADD_SUBR(Sgrab_keymap);
    rep_ADD_SUBR(Sungrab_keymap);

    rep_ADD_SUBR(Sset_wm_modifier);
    rep_ADD_SUBR(Swm_modifier);

    rep_pop_structure (tem);

    rep_INTERN_SPECIAL(meta_keysyms);
    Fset (Qmeta_keysyms, Qnil);
    rep_INTERN_SPECIAL(alt_keysyms);
    Fset (Qalt_keysyms, Qnil);
    rep_INTERN_SPECIAL(hyper_keysyms);
    Fset (Qhyper_keysyms, Qnil);
    rep_INTERN_SPECIAL(super_keysyms);
    Fset (Qsuper_keysyms, Qnil);


    register_property_monitor (Qkeymap, keymap_prop_change);
}
