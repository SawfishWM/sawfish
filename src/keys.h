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
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301 USA. */

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

enum {
    /*
      The low 16 bits of the MODS are the standard X modifier mask, as
      found in <X11/X.h>.  The highest standard modifier mask (1<<15)
      corresponds to AnyModifier, but there's a gap for (1<<13) and
      (1<<14) that we can fill.  According to libinput(4):

        X clients receive events with logical button numbers, where 1, 2, 3 are
        usually interpreted as left, middle, right and logical buttons 4, 5, 6,
        7 are usually interpreted as scroll up, down, left, right.  The  fourth
        and fifth physical buttons on a device will thus send logical buttons 8
        and 9.

      These button 6/7 masks are probably consistent with everybody
      everywhere.  They are definitely consistent with Tcl/Tk:
      https://core.tcl-lang.org/tk/artifact/fe30190927f08326.
    */
    EV_BUTTON6_MASK = 0x00002000, /* the (1<<13) gap */
    EV_BUTTON7_MASK = 0x00004000, /* the (1<<14) gap */

    /*
      Bits 16 to 20 define the type of the event.  We allocate two right
      now.  We have two available.
    */
    EV_TYPE_KEY      = 0x00010000,
    EV_TYPE_MOUSE    = 0x00020000,

    /* Defines the slots for the above EV_TYPE values. */
    EV_TYPE_MASK     = 0x00030000,

    /* Put that button 8+9 pair into an existing gap in the bitset. */
    EV_BUTTON8_MASK  = 0x00040000,
    EV_BUTTON9_MASK  = 0x00080000,

    /* this is used as a placeholder when translating events<->strings,
       it's replaced by the actual meta value */
    EV_MOD_META      = 0x00100000,
    EV_MOD_ALT       = 0x00200000,

    /* Matches any of the modifiers, above and below, including no
       modifier */
    EV_MOD_ANY       = 0x00400000,

    /* a KeyRelease event */
    EV_MOD_RELEASE   = 0x00800000,

    /* more placeholders for translating events<->strings */
    EV_MOD_HYPER     = 0x01000000,
    EV_MOD_SUPER     = 0x02000000,

    /* this is a customizable modifier; it allows the user to move
       all predefined wm key bindings to a different modifier */
    EV_MOD_WM        = 0x04000000,

    /* The combined mask for all of those placeholders.
       translate_event_to_x_button() uses this to distinguish these
       placeholders and wildcards from other bits.
    */
    EV_VIRT_MOD_MASK = 0x07f00000,

   /* This isn't just "modifier" masks, it's all masks known to
      Sawfish, whether from X11 or internal.  These bits are exclusive
      with EV_TYPE_MASK; EV_VIRT_MOD_MASK must be a subset of these
      bits.
    */
    EV_MOD_MASK      = 0x07fcffff,

    /* There are three bits left to play with: 0x08000000, 0x10000000,
       and 0x20000000.  The above masks do not allocate them.

       We can't use bits 0x40000000 and 0x80000000 because rep
       immediate integers are only 30 bits wide (see the "rep_lisp.h"
       comments in the librep sources), and keys.c assumes that we're
       using immediate integers everywhere.  Look at all of the calls
       to rep_INT() in keys.c, for example.

       The internals of "rep_lisp.h" and "rep_config.h.in" suggest
       that 64-bit systems automatically upgrade to 62 bit wide
       immediate integers.  That would give us many more bits to play
       with at the cost of breaking 32-bit systems.  And I don't know
       whether those large values would violate some other assumption,
       probably in X11 somewhere.
     */

};

#ifndef Button6
# define Button6 6
#endif
#ifndef Button6Mask
# define Button6Mask EV_BUTTON6_MASK
#endif

#ifndef Button7
# define Button7 7
#endif
#ifndef Button7Mask
# define Button7Mask EV_BUTTON7_MASK
#endif

#ifndef Button8
# define Button8 8
#endif
#ifndef Button8Mask
# define Button8Mask EV_BUTTON8_MASK
#endif

#ifndef Button9
# define Button9 9
#endif
#ifndef Button9Mask
# define Button9Mask EV_BUTTON9_MASK
#endif

/* In key maps, a `key' is (COMMAND . EVENT) */

#define KEYP(v)		(rep_CONSP(v) && rep_CONSP(rep_CDR(v)))
#define KEY_COMMAND(v)	rep_CAR(v)
#define KEY_EVENT(v)	rep_CDR(v)
#define MAKE_KEY(e, c)	Fcons(c, e)

#endif /* SAWFISH_KEYS_H */
