/* keys.h -- Event macros and constants.
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

#ifndef SAWFISH_KEYS_H
#define SAWFISH_KEYS_H

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
    EV_CODE_MOUSE_OFF1,
    EV_CODE_MOUSE_CLICK3,
    EV_CODE_MOUSE_OFF2,
    EV_CODE_MOUSE_OFF3
};

/* The low 16 bits of the MODS is the standard X modifier mask. Bits
   16 to 20 define the type of the event. One of the following: */

enum {
    EV_TYPE_KEY   = 0x00010000,
    EV_TYPE_MOUSE = 0x00020000,

    /* this is used as a placeholder when translating events<->strings,
       it's replaced by the actual meta value */
    EV_MOD_META   = 0x00100000,
    EV_MOD_ALT    = 0x00200000,
    EV_MOD_HYPER  = 0x01000000,
    EV_MOD_SUPER  = 0x02000000,

    /* this is a customizable modifier; it allows the user to move
       all predefined wm key bindings to a different modifier */
    EV_MOD_WM     = 0x04000000,

    /* Matches any of the modifiers, including no modifier */
    EV_MOD_ANY    = 0x00400000,

    /* a KeyRelease event */
    EV_MOD_RELEASE= 0x00800000,

    EV_TYPE_MASK  = 0x000f0000,
    EV_MOD_MASK   = 0x0ff0ffff,
    EV_VIRT_MOD_MASK = 0x0ff00000
};

/* Support for buttons 6, 7, 8.

   <X11/X.h> doesn't define these, even though XFree supports them.. */

#ifndef Button6
# define Button6 6
#endif
#ifndef Button6Mask
# define Button6Mask (1<<13)
#endif

#ifndef Button7
# define Button7 7
#endif
#ifndef Button7Mask
# define Button7Mask (1<<14)
#endif

#ifndef Button8
# define Button8 8
#endif
#ifndef Button8Mask
# define Button8Mask (1<<15)
#endif

/* In key maps, a `key' is (COMMAND . EVENT) */

#define KEYP(v)		(rep_CONSP(v) && rep_CONSP(rep_CDR(v)))
#define KEY_COMMAND(v)	rep_CAR(v)
#define KEY_EVENT(v)	rep_CDR(v)
#define MAKE_KEY(e, c)	Fcons(c, e)

#endif /* SAWFISH_KEYS_H */
