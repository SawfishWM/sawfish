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

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <rep.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>
#include <X11/Xlib.h>

#if defined (HAVE_IMLIB)
# include <Imlib.h>
#elif defined (HAVE_GDK_PIXBUF)
# include <gdk-pixbuf/gdk-pixbuf-xlib.h>
#else
# error "Need an image handling library!"
#endif

#ifndef rep_INTERFACE
# define rep_INTERFACE 7		/* rep 0.10 */
#endif

typedef int bool;

#ifdef rep_HAVE_UNIX
# define HAVE_UNIX 1
#endif

/* Uncomment the following line to get reams of debugging data. But
   redirect it to a non-X terminal (otherwise we can't printf when
   the server is grabbed => deadlock)

   You can also define it to zero to store the debugging output in
   one of rep's buffers. Send the sawmill process a SIGUSR2 to print
   the last 4k or so to stderr. */

/* #define DEBUG 1 */


/* Event masks */

/* Events selected on client windows */
#define CLIENT_EVENTS (PropertyChangeMask | StructureNotifyMask \
		       | ColormapChangeMask | VisibilityChangeMask)

/* Events selected on the root window */
#define ROOT_EVENTS (SubstructureRedirectMask | SubstructureNotifyMask \
		     | ButtonPressMask | ButtonReleaseMask | KeyPressMask \
		     | ButtonMotionMask | PointerMotionHintMask \
		     | EnterWindowMask | LeaveWindowMask)

/* Events selected on each frame part */
#define FP_EVENTS (ButtonPressMask | ButtonReleaseMask | ButtonMotionMask \
		   | PointerMotionHintMask | EnterWindowMask \
		   | LeaveWindowMask | KeyPressMask | ExposureMask)

/* Events selected on the frame window */
#define FRAME_EVENTS (ButtonPressMask | ButtonReleaseMask | KeyPressMask \
		      | ButtonMotionMask | PointerMotionHintMask \
		      | EnterWindowMask | LeaveWindowMask | ExposureMask \
		      | FocusChangeMask | SubstructureRedirectMask \
		      | VisibilityChangeMask)

/* Events selected in pointer/button grabs */
#define POINTER_GRAB_EVENTS (ButtonPressMask | ButtonReleaseMask \
			     | PointerMotionMask | PointerMotionHintMask)


/* Type defs */

/* A managed window */
typedef struct lisp_window {
    repv car;
    struct lisp_window *next;
    Window id, saved_id;
    repv plist;
    repv frame_style;

    /* stacking order */
    struct lisp_window *above, *below;

    /* Is the client window mapped? (by its app) */
    u_int mapped : 1;

    /* Is the frame visible? (not hidden by hide-window) */
    u_int visible : 1;

    /* Is the client window hidden by us?
       (controlled by window's `hide-client' property -- used for shading) */
    u_int client_hidden : 1;

    /* Is the client window unmapped by us?
       (because it's !visible or client_hidden) */
    u_int client_unmapped : 1;

    /* Is the client window reparented to the frame? */
    u_int reparented : 1;

    /* Is the client window shaped? */
    u_int shaped : 1;

    /* Have we called the destroy-notify-hook? */
    u_int destroyed : 1;

    /* The WM protocols understood by the client */
    u_int does_wm_take_focus : 1;
    u_int does_wm_delete_window : 1;

    /* Do we need to send a synthetic ConfigureNotify to this window? */
    u_int pending_configure : 1;

    /* Do we need to recalculate the shape mask of the frame? */
    u_int pending_reshape : 1;

    /* The position and dimensions of `attr' is always maintained.
       But the position is the position of the frame, while the
       dimensions are those of the client */
    XWindowAttributes attr;
    XSizeHints hints;
    XWMHints *wmhints;
    Window transient_for_hint;
    Window *cmap_windows;
    int n_cmap_windows;
    repv full_name, name, icon_name;
    int frame_vis;
    repv icon_image;

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
#define WINDOWP(v)	XWINDOWP(v)

#define WINDOW_FOCUSED_P(w) (focus_window == w)
#define WINDOW_IS_GONE_P(w) (w->id == 0)

/* An allocated font */
typedef struct lisp_font {
    repv car;
    struct lisp_font *next;
    repv name;
    union {
	XFontSet set;
	XFontStruct *str;
    } font;
    repv plist;
    int ascent, descent;
} Lisp_Font;

#define FONTP(v)	rep_CELL16_TYPEP(v, font_type)
#define VFONT(v)	((Lisp_Font *)rep_PTR(v))

#define FF_FONT_STRUCT	(1 << (rep_CELL16_TYPE_BITS + 0))
#define FONT_STRUCT_P(v) (VFONT(v)->car & FF_FONT_STRUCT)

/* An allocated color */
typedef struct lisp_color {
    repv car;
    struct lisp_color *next;
    int red, green, blue;		/* each 16 bits */
    int pixel;				/* somewhere in the screen's cmap */
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

#if defined (HAVE_IMLIB)
typedef ImlibImage *image_t;
#elif defined (HAVE_GDK_PIXBUF)
typedef GdkPixbuf *image_t;
#endif

typedef struct pixmap_cache_node_struct pixmap_cache_node;

/* A loaded image */
typedef struct lisp_image {
    repv car;
    struct lisp_image *next;
    image_t image;
#if !defined (HAVE_IMLIB)
    int border[4];
#endif
#if defined (NEED_PIXMAP_CACHE)
    pixmap_cache_node *pixmap_first, *pixmap_last;
#endif
    repv plist;
} Lisp_Image;

#define IMAGEP(v)	rep_CELL16_TYPEP(v, image_type)
#define VIMAGE(v)	((Lisp_Image *)rep_PTR(v))

enum frame_part_states {
    fps_none = -1,
    fps_inactive = 0,
    fps_focused,
    fps_highlighted,
    fps_inactive_highlighted,
    fps_clicked,
    fps_inactive_clicked,
    fps_MAX
};

/* one component of a frame */
struct frame_part {
    repv car;
    struct frame_part *next, *next_alloc;
    repv alist;				/* the generator alist */
    repv local_alist;

    Lisp_Window *win;
    int x, y, width, height;
    Window id;
    GC gc;

    u_int clicked : 1;
    u_int highlighted : 1;
    u_int pending_refresh : 1;
    u_int below_client : 1;
    u_int scale_foreground : 1;

    repv text;			/* may be nil, a string, or a function */
    repv x_justify, y_justify;

    repv font[fps_MAX];
    repv fg[fps_MAX];			/* may be color or image */
    repv bg[fps_MAX];			/* may be color or image */

    /* If renderer != Qnil, this overrides the bg array. It's a
       function to call to render the contents of rendered_image
       to the current state. */
    repv renderer, rendered_image;
    int render_scale, rendered_state;

    repv cursor;

    /* cached state of the window */
    struct {
	int width, height;
	repv font, text;
	repv x_justify, y_justify;
	repv fg, bg;
    } drawn;
};

#define PARTP(v)	rep_CELL16_TYPEP(v, frame_part_type)
#define VPART(v)	((struct frame_part *) rep_PTR (v))

/* codes for the clean_exit_jmp_buf */
enum exit_codes {
    ec_no_exit = 0,
    ec_exit,
    ec_restart,
    ec_session_died
};


/* bring in prototypes */

#ifndef XlibSpecificationRelease
# define XlibSpecificationRelease 4
#endif

/* Work around for X11R5 and earlier */
#ifndef XUrgencyHint
#define XUrgencyHint (1 << 8)
#endif

#include "sawmill_subrs.h"


/* Miscellaneous macro defs */

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */
#undef MAX
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#undef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))

#undef POS
#define POS(x)   MAX(x, 0)
#undef ABS
#define ABS(x)   (((x) >= 0) ? (x) : -(x))

#ifndef NULL
# define NULL 0
#endif

#ifndef TRUE
# define TRUE 1
#endif

#ifndef FALSE
# define FALSE 0
#endif

#if !defined (DEBUG)
# define DB(x) do { ; } while (0)
#elif DEBUG == 0
# define DB(x) db_printf x
#else
# define DB(x) printf x
#endif

#endif /* SAWMILL_H */
