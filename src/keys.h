/* keys.h -- Event structures
   $Id$ */

#ifndef SAWMILL_KEYS_H
#define SAWMILL_KEYS_H


/* An event object is (CODE . MODS) */

#define EVENTP(v)	(rep_CONSP(v) && rep_INTP(rep_CAR(v)) && rep_INTP(rep_CDR(v)))
#define MAKE_EVENT(c, m) Fcons(c, m)
#define EVENT_CODE(v)	rep_CAR(v)
#define EVENT_MODS(v)	rep_CDR(v)

/* For keys, the CODE is the standard Keysym value of the key. For mouse
   events CODE is one of the following: */

enum {
    EV_CODE_MOUSE_CLICK1 = 1,
    EV_CODE_MOUSE_CLICK2,
    EV_CODE_MOUSE_MOVE,
    EV_CODE_MOUSE_UP
};

/* The low 16 bits of the MODS is the standard X modifier mask. Bits
   16 to 20 define the type of the event. One of the following: */

enum {
    EV_TYPE_KEY   = 0x00010000,
    EV_TYPE_MOUSE = 0x00020000,

    /* this is used as a placeholder when translating events<->strings,
       it's replaced by the actual meta value */
    EV_MOD_META   = 0x00100000,

    /* Matches any of the modifiers */
    EV_MOD_ANY    = 0x00200000,

    EV_TYPE_MASK  = 0x000f0000,
    EV_MOD_MASK   = 0x00f0ffff,
};

#define EV_MOD_BUTTON_MASK \
    (Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask)


/* In key maps, a `key' is (COMMAND . EVENT) */

#define KEYP(v)		(rep_CONSP(v) && rep_CONSP(rep_CDR(v)))
#define KEY_COMMAND(v)	rep_CAR(v)
#define KEY_EVENT(v)	rep_CDR(v)
#define MAKE_KEY(e, c)	Fcons(c, e)

#endif /* SAWMILL_KEYS_H */
