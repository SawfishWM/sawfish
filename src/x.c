/* x.c -- raw X manipulation
   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   Originally written by merlin <merlin@merlin.org>, with additions
   from John Harper

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
#include <X11/Xresource.h>

/* An allocated x-gc */
typedef struct lisp_x_gc {
    repv car;
    struct lisp_x_gc *next;
    GC gc;
    Window id;
} Lisp_X_GC;

#define X_XGCP(v)	rep_CELL16_TYPEP(v, x_gc_type)
#define X_GCP(v)	(X_XGCP(v) && VX_GC(v)->gc != 0)
#define X_VALID_GCP(v,id) (X_XGCP(v) && VX_GC(v)->gc != 0 && VX_GC(v)->id == id)
#define VX_GC(v)	((Lisp_X_GC *)rep_PTR(v))

/* An allocated x-window */
typedef struct lisp_x_window {
    repv car;
    struct lisp_x_window *next;
    Window id;
    repv event_handler;
} Lisp_X_Window;

#define X_XWINDOWP(v) rep_CELL16_TYPEP(v, x_window_type)
#define X_WINDOWP(v)  (X_XWINDOWP(v) && VX_WINDOW(v)->id != 0)
#define VX_WINDOW(v)  ((Lisp_X_Window *)rep_PTR(v))

static Lisp_X_GC *x_gc_list = NULL;
int x_gc_type;

static Lisp_X_Window *x_window_list = NULL;
int x_window_type;

static XID x_window_context;

DEFSYM(x, "x");
DEFSYM(y, "y");
DEFSYM(border_width, "border-width");
DEFSYM(border_color, "border-color");
DEFSYM(expose, "expose");

static Window
window_from_arg (repv arg)
{
    if (rep_INTEGERP (arg))
	return rep_get_long_uint (arg);
    else if (X_WINDOWP (arg))
	return VX_WINDOW(arg)->id;
    else if (WINDOWP(arg) && VWIN(arg)->id != 0)
	return VWIN(arg)->id;
    else if (PARTP(arg) && VPART(arg)->id != 0)
	return VPART(arg)->id;
    else
	return 0;
}


/* GC Functions */

static long
x_gc_parse_attrs (XGCValues *values, repv attrs)
{
    long valueMask = 0;

    while (rep_CONSP (attrs)) {
        repv tem = rep_CAR (attrs);

        if (rep_CONSP (tem)) {
            repv car = rep_CAR (tem);
            if ((car == Qforeground) && COLORP(rep_CDR(tem))) {
                values->foreground = VCOLOR(rep_CDR(tem))->pixel;
                valueMask |= GCForeground;
            } else if ((car == Qbackground) && COLORP(rep_CDR(tem))) {
                values->background = VCOLOR(rep_CDR(tem))->pixel;
                valueMask |= GCBackground;
            }
        }

        attrs = rep_CDR (attrs);
    }

    return valueMask;
}

DEFUN("x-create-gc", Fx_create_gc, Sx_create_gc, (repv window, repv attrs), rep_Subr2) /*
::doc:x-create-gc::
x-create-gc WINDOW ATTRS

Creates a new GC for the specified window. ATTRS should be an alist
mapping attributes to values. Known attributes are `foreground' and
`background'.
::end:: */
{
    Window id = window_from_arg (window);
    Lisp_X_GC *g;
    GC gc;
    XGCValues values;
    long valueMask;

    if (dpy == 0)
	return Qnil;

    rep_DECLARE(1, window, id != 0);
    rep_DECLARE2(attrs, rep_LISTP);

    valueMask = x_gc_parse_attrs (&values, attrs);
    gc = XCreateGC (dpy, id, valueMask, &values);

    g = rep_ALLOC_CELL(sizeof(Lisp_X_GC));
    rep_data_after_gc += sizeof (Lisp_X_GC);
    g->car = x_gc_type;
    g->next = x_gc_list;
    x_gc_list = g;
    g->gc = gc;
    g->id = id;

    return rep_VAL(g);
}

DEFUN("x-change-gc", Fx_change_gc, Sx_change_gc, (repv gc, repv attrs), rep_Subr2) /*
::doc:x-change-gc::
x-change-gc X-GC ATTRS

Sets attributes of the X-GC. ATTRS should be a list of cons cells mapping
attributes to values. Known attributes are `foreground' and `background'.
::end:: */
{
    XGCValues values;
    long valueMask;

    rep_DECLARE1(gc, X_GCP);
    rep_DECLARE2(attrs, rep_LISTP);

    valueMask = x_gc_parse_attrs (&values, attrs);

    if (valueMask)
      XChangeGC (dpy, VX_GC(gc)->gc, valueMask, &values);

    return Qt;
}

DEFUN("x-destroy-gc", Fx_destroy_gc, Sx_destroy_gc, (repv gc), rep_Subr1) /*
::doc:x-destroy-gc::
x-destroy-gc X-GC

Destroy the X-GC.
::end:: */
{
    rep_DECLARE1(gc, X_GCP);

    XFreeGC (dpy, VX_GC(gc)->gc);
    VX_GC(gc)->gc = NULL;

    return Qt;
}

DEFUN("x-gc-p", Fx_gc_p, Sx_gc_p, (repv gc), rep_Subr1) /*
::doc:x-gc-p::
x-gcp ARG

Return t if ARG is a X-GC object.
::end:: */
{
    return X_GCP(gc) ? Qt : Qnil;
}


/* Window functions */

static inline repv
x_window_from_id (Window id)
{
    repv win;
    return XFindContext (dpy, id, x_window_context,
			 (XPointer *) &win) ? Qnil : win;
}

static long
x_window_parse_changes (XWindowChanges *changes, repv attrs)
{
    long changesMask = 0;

    while (rep_CONSP (attrs)) {
        repv tem = rep_CAR (attrs);

        if (rep_CONSP (tem)) {
            repv car = rep_CAR (tem);
            if ((car == Qx) && rep_INTP(rep_CDR(tem))) {
                changes->x = rep_INT(rep_CDR(tem));
                changesMask |= CWX;
            } else if ((car == Qy) && rep_INTP(rep_CDR(tem))) {
                changes->y = rep_INT(rep_CDR(tem));
                changesMask |= CWY;
            } else if ((car == Qwidth) && rep_INTP(rep_CDR(tem))) {
                changes->width = rep_INT(rep_CDR(tem));
                changesMask |= CWWidth;
            } else if ((car == Qheight) && rep_INTP(rep_CDR(tem))) {
                changes->height = rep_INT(rep_CDR(tem));
                changesMask |= CWHeight;
            } else if ((car == Qborder_width) && rep_INTP(rep_CDR(tem))) {
                changes->border_width = rep_INT(rep_CDR(tem));
                changesMask |= CWBorderWidth;
            }
        }

        attrs = rep_CDR (attrs);
    }

    return changesMask;
}

static long
x_window_parse_attributes (XSetWindowAttributes *attributes, repv attrs)
{
    long attributesMask = 0;

    while (rep_CONSP (attrs)) {
        repv tem = rep_CAR (attrs);

        if (rep_CONSP (tem)) {
            repv car = rep_CAR (tem);
            if ((car == Qbackground) && COLORP(rep_CDR(tem))) {
                attributes->background_pixel = VCOLOR(rep_CDR(tem))->pixel;
                attributesMask |= CWBackPixel;
            } else if ((car == Qborder_color) && COLORP(rep_CDR(tem))) {
                attributes->border_pixel = VCOLOR(rep_CDR(tem))->pixel;
                attributesMask |= CWBorderPixel;
            }
        }

        attrs = rep_CDR (attrs);
    }

    return attributesMask;
}

static void
x_window_event_handler (XEvent *ev)
{
    repv win = x_window_from_id (ev->xany.window);
    if (VX_WINDOW (win)->event_handler != Qnil)
    {
	repv type = Qnil, args = Qnil;
	if (win != Qnil)
	{
	    switch (ev->type)
	    {
	    case Expose:
		type = Qexpose;
		break;

		/* XXX other event types..? */
	    }
	}
	if (type != Qnil)
	{
	    args = Fcons (type, args);
	    rep_funcall (VX_WINDOW(win)->event_handler, args, rep_FALSE);
	}
    }
}

DEFUN("x-create-window", Fx_create_window, Sx_create_window, (repv xy, repv wh, repv bw, repv attrs, repv ev), rep_Subr5) /*
::doc:x-create-window::
x-create-window (X . Y) (W . H) BW ATTRS [EVENT-HANDLER]

Creates a new X-WINDOW with the specified position, dimensions and
border width. ATTRS should be a list of cons cells mapping attributes
to values. Known attributes are `background' and `border-color'. The
window is automatically mapped and raised.
::end:: */
{
    Lisp_X_Window *w;
    Window id;
    XSetWindowAttributes attributes;
    long attributesMask;
    int _x, _y, _w, _h, _bw;

    rep_DECLARE(1, xy, rep_CONSP (xy)
		&& rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE(2, wh, rep_CONSP (wh)
		&& rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));
    rep_DECLARE3(bw, rep_INTP);
    rep_DECLARE4(attrs, rep_LISTP);

    _x = rep_INT (rep_CAR (xy));
    _y = rep_INT (rep_CDR (xy));
    _w = rep_INT (rep_CAR (wh));
    _h = rep_INT (rep_CDR (wh));
    _bw = rep_INT (bw);

    attributesMask = x_window_parse_attributes (&attributes, attrs);
    attributes.override_redirect = True;
    attributes.event_mask = ExposureMask;
    attributes.colormap = screen_cmap;
    attributesMask |= CWOverrideRedirect | CWEventMask | CWColormap;

    id = XCreateWindow (dpy, root_window, _x, _y, _w, _h, _bw,
                        screen_depth, InputOutput, screen_visual,
                        attributesMask, &attributes);

    w = rep_ALLOC_CELL(sizeof(Lisp_X_Window));
    rep_data_after_gc += sizeof (Lisp_X_Window);
    w->car = x_window_type;
    w->next = x_window_list;
    x_window_list = w;
    w->id = id;
    w->event_handler = ev;

    register_event_handler (id, x_window_event_handler);
    XSaveContext (dpy, id, x_window_context, (XPointer) w);

    return rep_VAL (w);
}

DEFUN("x-map-window", Fx_map_window, Sx_map_window, (repv win, repv unraised), rep_Subr2) /*
::doc:x-map-window::
x-map-window X-WINDOW [UNRAISED]
::end:: */
{
    rep_DECLARE1(win, X_WINDOWP);
    if (unraised == Qnil)
	XMapRaised (dpy, VX_WINDOW(win)->id);
    else
	XMapWindow (dpy, VX_WINDOW(win)->id);
    return Qt;
}

DEFUN("x-unmap-window", Fx_unmap_window, Sx_unmap_window, (repv win), rep_Subr1) /*
::doc:x-unmap-window::
x-unmap-window X-WINDOW
::end:: */
{
    rep_DECLARE1(win, X_WINDOWP);
    XUnmapWindow (dpy, VX_WINDOW(win)->id);
    return Qt;
}

DEFUN("x-configure-window", Fx_configure_window, Sx_configure_window, (repv window, repv attrs), rep_Subr2) /*
::doc:x-configure-window::
x-configure-window WINDOW ATTRS

Reconfigures the X-WINDOW. ATTRS should be a list of cons cells mapping
attributes to values. Known attributes are 'x, 'y', 'width, 'height and
'border-width. The window is automatically raised.
::end:: */
{
    XWindowChanges changes;
    long changesMask;

    rep_DECLARE1(window, X_WINDOWP);
    rep_DECLARE2(attrs, rep_LISTP);

    changesMask = x_window_parse_changes (&changes, attrs);
    changes.stack_mode = TopIf;
    changesMask |= CWStackMode;

    if (changesMask)
      XConfigureWindow (dpy, VX_WINDOW(window)->id, changesMask, &changes);

    return Qt;
}

DEFUN("x-change-window-attributes", Fx_change_window_attributes, Sx_change_window_attributes, (repv window, repv attrs), rep_Subr2) /*
::doc:x-change-window-attributes::
x-change-window-attributes WINDOW ATTRS

Sets attributes of the X-WINDOW. ATTRS should be a list of cons cells mapping
attributes to values. Known attributes are 'background and 'border-color.
::end:: */
{
    XSetWindowAttributes attributes;
    long attributesMask;

    rep_DECLARE1(window, X_WINDOWP);
    rep_DECLARE2(attrs, rep_LISTP);

    attributesMask = x_window_parse_attributes (&attributes, attrs);

    if (attributesMask)
      XChangeWindowAttributes (dpy, VX_WINDOW(window)->id, attributesMask, &attributes);

    return Qt;
}

DEFUN("x-destroy-window", Fx_destroy_window, Sx_destroy_window, (repv window), rep_Subr1) /*
::doc:x-destroy-window::
x-destroy-window WINDOW

Destroys the X-WINDOW.
::end:: */
{
    rep_DECLARE1(window, X_WINDOWP);

    XDeleteContext (dpy, VX_WINDOW(window)->id, x_window_context);
    deregister_event_handler (VX_WINDOW(window)->id); 
    XDestroyWindow (dpy, VX_WINDOW(window)->id);
    VX_WINDOW(window)->id = 0;

    return Qt;
}

DEFUN("x-window-id", Fx_window_id, Sx_window_id, (repv window), rep_Subr1) /*
::doc:x-window-id::
x-window-id WINDOW

Return the X11 window-id (an integer) associated with X-WINDOW.
::end:: */
{
    rep_DECLARE1(window, X_WINDOWP);

    return rep_MAKE_INT (VX_WINDOW(window)->id);
}

DEFUN("x-windowp", Fx_windowp, Sx_windowp, (repv window), rep_Subr1) /*
::doc:x-windowp::
x-windowp ARG

Return t if ARG is a X-WINDOW object.
::end:: */
{
    return X_WINDOWP(window) ? Qt : Qnil;
}


/* Drawing functions */

DEFUN("x-clear-window", Fx_clear_window, Sx_clear_window, (repv window), rep_Subr1) /*
::doc:x-clear-window::
x-clear-window WINDOW

Clears the window associated with WINDOW to its background color.
::end:: */
{
    Window id = window_from_arg (window);
    rep_DECLARE(1, window, id != 0);

    XClearWindow (dpy, id);
    return Qt;
}

DEFUN("x-draw-string", Fx_draw_string, Sx_draw_string, (repv window, repv gc, repv xy, repv string, repv font), rep_Subr5) /*
::doc:x-draw-string::
x-draw-string WINDOW GC (X . Y) STRING [FONT]

Draws the specified string at the specified location in the optional
specified font in the window associated with WINDOW.
::end:: */
{
    Window id = window_from_arg (window);
    int x = 0;
    int y = 0;
    unsigned char *str;

    rep_DECLARE(1, window, id != 0);
    rep_DECLARE(2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE(3, xy, rep_CONSP (xy)
		&& rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE4(string, rep_STRINGP);
    if (font == Qnil)
	font = Fsymbol_value (Qdefault_font, Qt);
    rep_DECLARE5(font, FONTP);

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    str = rep_STR (string);

    x_draw_string (id, font, VX_GC(gc)->gc, x, y, str, strlen (str));
    return Qt;
}

DEFUN("x-draw-line", Fx_draw_line, Sx_draw_line, (repv window, repv gc, repv start, repv end), rep_Subr4) /*
::doc:x-draw-line::
x-draw-line WINDOW GC (X1 . Y1) (X2 . Y2)
::end:: */
{
    Window id = window_from_arg (window);
    int x1 = 0, y1 = 0;
    int x2 = 0, y2 = 0;

    rep_DECLARE(1, window, id != 0);
    rep_DECLARE(2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE(3, start, rep_CONSP (start)
		&& rep_INTP (rep_CAR (start)) && rep_INTP (rep_CDR (start)));
    rep_DECLARE(4, end, rep_CONSP (end)
		&& rep_INTP (rep_CAR (end)) && rep_INTP (rep_CDR (end)));

    x1 = rep_INT (rep_CAR (start));
    y1 = rep_INT (rep_CDR (start));
    x2 = rep_INT (rep_CAR (end));
    y2 = rep_INT (rep_CDR (end));

    XDrawLine (dpy, id, VX_GC(gc)->gc, x1, y1, x2, y2);
    return Qt;
}

DEFUN("x-draw-rectangle", Fx_draw_rectangle, Sx_draw_rectangle, (repv window, repv gc, repv xy, repv wh), rep_Subr4) /*
::doc:x-draw-rectangle::
x-draw-rectangle WINDOW GC (X . Y) (WIDTH . HEIGHT)
::end:: */
{
    Window id = window_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;

    rep_DECLARE(1, window, id != 0);
    rep_DECLARE(2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE(3, xy, rep_CONSP (xy)
		&& rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE(4, wh, rep_CONSP (wh)
		&& rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = rep_INT (rep_CAR (wh));
    h = rep_INT (rep_CDR (wh));

    XDrawRectangle (dpy, id, VX_GC(gc)->gc, x, y, w, h);
    return Qt;
}

DEFUN("x-fill-rectangle", Fx_fill_rectangle, Sx_fill_rectangle, (repv window, repv gc, repv xy, repv wh), rep_Subr4) /*
::doc:x-fill-rectangle::
x-fill-rectangle WINDOW GC (X . Y) (WIDTH . HEIGHT)
::end:: */
{
    Window id = window_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;

    rep_DECLARE(1, window, id != 0);
    rep_DECLARE(2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE(3, xy, rep_CONSP (xy)
		&& rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE(4, wh, rep_CONSP (wh)
		&& rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = rep_INT (rep_CAR (wh));
    h = rep_INT (rep_CDR (wh));

    XFillRectangle (dpy, id, VX_GC(gc)->gc, x, y, w, h);
    return Qt;
}

DEFUN("x-draw-image", Fx_draw_image, Sx_draw_image, (repv img, repv window, repv xy, repv wh), rep_Subr4) /*
::doc:x-draw-image::
x-draw-image IMAGE WINDOW (X . Y) [(WIDTH . HEIGHT)]
::end:: */
{
    Window id = window_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;

    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE(2, window, id != 0);
    rep_DECLARE(3, xy, rep_CONSP (xy)
		&& rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    if (wh != Qnil)
	rep_DECLARE(4, wh, rep_CONSP (wh)
		    && rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = (wh == Qnil) ? VIMAGE(img)->image->rgb_width : rep_INT (rep_CAR (wh));
    h = (wh == Qnil) ? VIMAGE(img)->image->rgb_height : rep_INT (rep_CDR (wh));

    Imlib_paste_image (imlib_id, VIMAGE(img)->image, id, x, y, w, h);

    /* Imlib sometimes calls XSync (), which could hide events */
    rep_mark_input_pending (ConnectionNumber(dpy));

    return Qt;
}


/* gc type hooks */

static int
x_gc_cmp (repv w1, repv w2)
{
    return w1 != w2;
}

static void
x_gc_prin (repv stream, repv obj)
{
    char buf[256];
    sprintf (buf, "#<x-gc>");
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
x_gc_mark (repv obj)
{
}

static void
x_gc_sweep (void)
{
    Lisp_X_GC *w = x_gc_list;
    x_gc_list = 0;
    while (w != 0)
    {
	Lisp_X_GC *next = w->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
            if (w->gc != 0)
                Fx_destroy_gc(rep_VAL(w));
	    rep_FREE_CELL(w);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = x_gc_list;
	    x_gc_list = w;
	}
	w = next;
    }
}


/* window type hooks */

static int
x_window_cmp (repv w1, repv w2)
{
    return w1 != w2;
}

static void
x_window_prin (repv stream, repv obj)
{
    char buf[256];
    sprintf (buf, "#<x-window 0x%lx>", VX_WINDOW(obj)->id);
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
x_window_mark (repv obj)
{
    rep_MARKVAL (VX_WINDOW (obj)->event_handler);
}

static void
x_window_sweep (void)
{
    Lisp_X_Window *w = x_window_list;
    x_window_list = 0;
    while (w != 0)
    {
	Lisp_X_Window *next = w->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
            if (w->id != 0)
                Fx_destroy_window(rep_VAL(w));
	    rep_FREE_CELL(w);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = x_window_list;
	    x_window_list = w;
	}
	w = next;
    }
}


/* initialisation */

repv
rep_dl_init (void)
{
    x_gc_type = rep_register_new_type ("x-gc", x_gc_cmp, x_gc_prin, x_gc_prin,
				       x_gc_sweep, x_gc_mark,
				       0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sx_create_gc);
    rep_ADD_SUBR(Sx_change_gc);
    rep_ADD_SUBR(Sx_destroy_gc);
    rep_ADD_SUBR(Sx_gc_p);

    x_window_context = XUniqueContext ();

    x_window_type = rep_register_new_type ("x-window", x_window_cmp,
					   x_window_prin, x_window_prin,
				           x_window_sweep, x_window_mark,
				           0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sx_create_window);
    rep_ADD_SUBR(Sx_map_window);
    rep_ADD_SUBR(Sx_unmap_window);
    rep_ADD_SUBR(Sx_configure_window);
    rep_ADD_SUBR(Sx_change_window_attributes);
    rep_ADD_SUBR(Sx_destroy_window);
    rep_ADD_SUBR(Sx_windowp);
    rep_ADD_SUBR(Sx_window_id);

    rep_ADD_SUBR(Sx_clear_window);
    rep_ADD_SUBR(Sx_draw_string);
    rep_ADD_SUBR(Sx_draw_line);
    rep_ADD_SUBR(Sx_draw_rectangle);
    rep_ADD_SUBR(Sx_fill_rectangle);
    rep_ADD_SUBR(Sx_draw_image);

    rep_INTERN(x);
    rep_INTERN(y);
    rep_INTERN(border_width);
    rep_INTERN(border_color);

    return Qx;
}
