/* sawmill.h -- Main include file, brings in all the rest
   $Id: sawmill.h,v 1.2 1999/07/25 15:02:21 john Exp

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

#ifndef SAWMILL_H
#define SAWMILL_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <rep.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>
#include <X11/Xlib.h>
#include <Imlib.h>

typedef int bool;

#ifdef rep_HAVE_UNIX
# define HAVE_UNIX 1
#endif

/* Uncomment the following line to get reams of debugging data. But
   redirect it to a non-X terminal (otherwise we can't printf when
   the server is grabbed => deadlock) */
/* #define DEBUG */


/* Type defs */

/* A managed window */
typedef struct lisp_window {
    repv car;
    struct lisp_window *next;
    Window id, saved_id;
    repv plist;
    repv frame_style;

    /* Is the client window mapped? */
    int mapped : 1;

    /* Is the frame visible? (not hidden by hide-window) */
    int visible : 1;

    /* Is the client window reparented to the frame? */
    int reparented : 1;

    /* Are we between reparenting the window and receiving
       a ReparentNotify event? */
    int reparenting : 1;

    /* Is the client window shaped? */
    int shaped : 1;

    /* The WM protocols understood by the client */
    int does_wm_take_focus : 1;
    int does_wm_delete_window : 1;

    /* The position and dimensions of `attr' is always maintained. */
    XWindowAttributes attr;
    XSizeHints hints;
    XWMHints *wmhints;
    Window transient_for_hint;
    char *full_name, *name, *icon_name;
    int frame_vis;

    /* Frame data */
    Window frame;
    struct frame_part *frame_parts;
    u_int frame_x, frame_y;		/* relative to client-window */
    u_int frame_width, frame_height;
    void (*destroy_frame)(struct lisp_window *w);
    void (*focus_change)(struct lisp_window *w);
    void (*rebuild_frame)(struct lisp_window *w);
    void (*property_change)(struct lisp_window *w);
} Lisp_Window;

#define VWIN(v)		((Lisp_Window *)rep_PTR(v))
#define XWINDOWP(v)	rep_CELL16_TYPEP(v, window_type)
#define WINDOWP(v)	(XWINDOWP(v) && VWIN(v)->id != 0)

#define WINDOW_FOCUSED_P(w) (focus_window == w)

/* An allocated font */
typedef struct lisp_font {
    repv car;
    struct lisp_font *next;
    repv name;
    XFontStruct *font;
    repv plist;
} Lisp_Font;

#define FONTP(v)	rep_CELL16_TYPEP(v, font_type)
#define VFONT(v)	((Lisp_Font *)rep_PTR(v))

/* An allocated color */
typedef struct lisp_color {
    repv car;
    struct lisp_color *next;
    repv name;
    XColor color;
} Lisp_Color;

#define COLORP(v)	rep_CELL16_TYPEP(v, color_type)
#define VCOLOR(v)	((Lisp_Color *)rep_PTR(v))

/* An allocated cursor */
typedef struct lisp_cursor {
    repv car;
    struct lisp_cursor *next;
    repv data;
    Cursor cursor;
} Lisp_Cursor;

#define CURSORP(v)	rep_CELL16_TYPEP(v, cursor_type)
#define VCURSOR(v)	((Lisp_Cursor *)rep_PTR(v))

/* A loaded image */
typedef struct lisp_image {
    repv car;
    struct lisp_image *next;
    ImlibImage *image;
    repv plist;
} Lisp_Image;

#define IMAGEP(v)	rep_CELL16_TYPEP(v, image_type)
#define VIMAGE(v)	((Lisp_Image *)rep_PTR(v))

enum frame_part_states {
    fps_normal = 0,
    fps_focused,
    fps_highlighted,
    fps_clicked,
    fps_MAX
};

/* one component of a frame */
struct frame_part {
    struct frame_part *next;
    repv alist;				/* the generator alist */

    Lisp_Window *win;
    int x, y, width, height;
    Window id;
    GC gc;

    int clicked : 1;
    int highlighted : 1;
    int pending_refresh : 1;

    repv text;			/* may be nil, a string, or a function */
    repv x_justify, y_justify;

    repv font[fps_MAX];
    repv fg[fps_MAX];			/* may only be color */
    repv bg[fps_MAX];			/* may be color or image */

    repv cursor;
};

/* codes for the clean_exit_jmp_buf */
enum exit_codes {
    ec_exit = 1,
    ec_restart
};


/* bring in prototypes */

#include "sawmill_subrs.h"


/* Miscellaneous macro defs */

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */
#undef MAX
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#undef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))

#ifndef NULL
# define NULL 0
#endif

#ifndef TRUE
# define TRUE 1
#endif

#ifndef FALSE
# define FALSE 0
#endif

#ifdef DEBUG
# define DB(x) printf x
#else
# define DB(x) do { ; } while (0)
#endif

#endif /* SAWMILL_H */
