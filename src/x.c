/* x.c -- raw X manipulation
   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   Originally written by merlin <merlin@merlin.org>, with additions
   from John Harper

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

/* AIX requires this to be the first thing in the file.  */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
   char *alloca ();
#   endif
#  endif
# endif
#endif

#include "sawfish.h"
#include <string.h>
#include <X11/Xresource.h>

#ifdef HAVE_X11_EXTENSIONS_XDBE_H
# include <X11/extensions/Xdbe.h>
#endif

static int have_dbe;

/* An allocated x-gc */
typedef struct lisp_x_gc {
    repv car;
    struct lisp_x_gc *next;
    GC gc;
    Lisp_Color fg_copy;
    Window id;
} Lisp_X_GC;

#define X_XGCP(v)	rep_CELL16_TYPEP(v, x_gc_type)
#define X_GCP(v)	(X_XGCP(v) && VX_GC(v)->gc != 0)
#define X_VALID_GCP(v,id) X_GCP(v)
#define VX_GC(v)	((Lisp_X_GC *)rep_PTR(v))

/* An allocated x-window */
typedef struct lisp_x_window {
    repv car;
    struct lisp_x_window *next;
    Drawable id;
    repv event_handler;
    int is_window : 1;
    int is_pixmap : 1;
    int is_bitmap : 1;			/* depth == 1 */
    int width, height;
} Lisp_X_Window;

#define X_XDRAWABLEP(v) rep_CELL16_TYPEP(v, x_window_type)
#define X_DRAWABLEP(v)  (X_XDRAWABLEP(v) && VX_DRAWABLE(v)->id != 0)
#define VX_DRAWABLE(v)  ((Lisp_X_Window *)rep_PTR(v))

#define X_WINDOWP(v)	(X_DRAWABLEP (v) && VX_DRAWABLE (v)->is_window)
#define X_PIXMAPP(v)	(X_DRAWABLEP (v) && VX_DRAWABLE (v)->is_pixmap)
#define X_BITMAPP(v)	(X_DRAWABLEP (v) && VX_DRAWABLE (v)->is_bitmap)

static Lisp_X_GC *x_gc_list = NULL;
int x_gc_type;

static Lisp_X_Window *x_window_list = NULL;
int x_window_type;

static XID x_drawable_context, x_dbe_context;

#define N_FNS 16
static repv r_fns[N_FNS];
static int x_fns[N_FNS];

DEFSYM (x, "x");
DEFSYM (y, "y");
DEFSYM (border_width, "border-width");
DEFSYM (border_color, "border-color");
DEFSYM (expose, "expose");
DEFSYM (convex, "convex");
DEFSYM (non_convex, "non-convex");
DEFSYM (line_width, "line-width");
DEFSYM (line_style, "line-style");
DEFSYM (cap_style,  "cap-style");
DEFSYM (join_style, "join-style");
DEFSYM (fill_style, "fill-style");
DEFSYM (fill_rule, "fill-rule");
DEFSYM (arc_mode, "arc-mode");
DEFSYM (tile, "tile");
DEFSYM (stipple, "stipple");
DEFSYM (ts_x_origin, "ts-x-origin");
DEFSYM (ts_y_origin, "ts-y-origin");
DEFSYM (clip_mask, "clip-mask");
DEFSYM (clip_x_origin, "clip-x-origin");
DEFSYM (clip_y_origin, "clip-y-origin");

DEFSYM (LineSolid, "line-solid");
DEFSYM (LineOnOffDash, "line-on-off-dash");
DEFSYM (LineDoubleDash, "line-double-dash");
DEFSYM (CapNotLast, "cap-not-last");
DEFSYM (CapButt, "cap-butt");
DEFSYM (CapRound, "cap-round");
DEFSYM (CapProjecting, "cap-projecting");
DEFSYM (JoinMiter, "join-miter");
DEFSYM (JoinRound, "join-round");
DEFSYM (JoinBevel, "join-bevel");
DEFSYM (FillSolid, "fill-solid");
DEFSYM (FillTiled, "fill-tiled");
DEFSYM (FillStippled, "fill-stippled");
DEFSYM (FillOpaqueStippled, "fill-opaque-stippled");
DEFSYM (EvenOddRule, "even-odd-rule");
DEFSYM (WindingRule, "winding-rule");
DEFSYM (ArcChord, "arc-chord");
DEFSYM (ArcPieSlice, "arc-pie-slice");
DEFSYM (function, "function");

DEFSYM (clear, "clear");
DEFSYM (and, "and");
DEFSYM (andReverse, "and-reverse");
DEFSYM (copy, "copy");
DEFSYM (andInverted, "and-inverted");
DEFSYM (noop, "no-op");
DEFSYM (xor, "xor");
DEFSYM (or, "or");
DEFSYM (nor, "nor");
DEFSYM (equiv, "equiv");
DEFSYM (invert, "invert");
DEFSYM (orReverse, "or-reverse");
DEFSYM (copyInverted, "copy-inverted");
DEFSYM (orInverted, "or-inverted");
DEFSYM (nand, "nand");
DEFSYM (set, "set");

static inline repv
x_window_from_id (Window id)
{
    union {
	repv r;
	XPointer p;
    } win;

    return XFindContext (dpy, id, x_drawable_context,
			 &win.p) ? Qnil : win.r;
}

#ifdef HAVE_X11_EXTENSIONS_XDBE_H
static inline XdbeBackBuffer
x_back_buffer_from_id (Window id)
{
    XPointer buf;
    return (XFindContext (dpy, id, x_dbe_context, &buf)
	    ? 0 : (XdbeBackBuffer) buf);
}
#endif

static Window
window_from_arg (repv arg)
{
    Window id;

    if (rep_INTEGERP (arg))
	id = rep_get_long_uint (arg);
    else if (X_WINDOWP (arg))
	id = VX_DRAWABLE (arg)->id;
    else if (WINDOWP (arg) && VWIN (arg)->id != 0)
	id = VWIN (arg)->id;
    else if (PARTP (arg) && VPART (arg)->id != 0)
	id = VPART (arg)->id;
    else if (arg == Qroot)
	id = root_window;
    else
	id = 0;

    return id;
}

static inline Drawable
drawable_from_arg (repv arg)
{
    Drawable id;
    if (X_DRAWABLEP (arg))
	id = VX_DRAWABLE (arg)->id;
    else
	id = window_from_arg (arg);
    return id;
}

static int
x_function_from_sym (repv sym)
{
    int i;

    for (i = 0; i < N_FNS; i++)
    {
        if (sym == r_fns[i])
            return x_fns[i];
    }
    return GXcopy;
}

/* GC Functions */

static long
x_gc_parse_attrs (Lisp_X_GC *gc, XGCValues *values, repv attrs)
{
    long valueMask = 0;

    while (rep_CONSP (attrs))
    {
	repv tem = rep_CAR (attrs);

	if (rep_CONSP (tem))
	{
	    repv car = rep_CAR (tem);
	    repv cdr = rep_CDR (tem);

	    if ((car == Qforeground) && COLORP (cdr))
	    {
		gc->fg_copy = *VCOLOR (cdr);
		values->foreground = VCOLOR (cdr)->pixel;
		valueMask |= GCForeground;
	    }
	    else if ((car == Qbackground) && COLORP (cdr))
	    {
		values->background = VCOLOR (cdr)->pixel;
		valueMask |= GCBackground;
	    }
	    else if ((car == Qline_width) && rep_INTP (cdr))
	    {
		values->line_width = rep_INT (cdr);
		valueMask |= GCLineWidth;
	    }
	    else if (car == Qline_style)
	    {
		if (cdr == QLineSolid)
		    values->line_style = LineSolid;
		else if (cdr == QLineOnOffDash)
		    values->line_style = LineOnOffDash;
		else if (cdr == QLineDoubleDash)
		    values->line_style = LineDoubleDash;
		else
		    values->line_style = LineSolid;
		valueMask |= GCLineStyle;
	    }
	    else if (car == Qcap_style)
	    {
		if (cdr == QCapNotLast)
		    values->cap_style = CapNotLast;
		else if (cdr == QCapButt)
		    values->cap_style = CapButt;
		else if (cdr == QCapRound)
		    values->cap_style = CapRound;
		else if (cdr == QCapProjecting)
		    values->cap_style = CapProjecting;
		else
		    values->cap_style = CapButt;
		valueMask |= GCCapStyle;
	    }
	    else if (car == Qjoin_style)
	    {
		if (cdr == QJoinMiter)
		    values->join_style = JoinMiter;
		else if (cdr == QJoinRound)
		    values->join_style = JoinRound;
		else if (cdr == QJoinBevel)
		    values->join_style = JoinBevel;
		else
		    values->join_style = JoinMiter;
		valueMask |= GCJoinStyle;
	    }
	    else if (car == Qfill_style)
	    {
		if (cdr == QFillSolid)
		    values->fill_style = FillSolid;
		else if (cdr == QFillTiled)
		    values->fill_style = FillTiled;
		else if (cdr == QFillStippled)
		    values->fill_style = FillStippled;
		else if (cdr == QFillOpaqueStippled)
		    values->fill_style = FillOpaqueStippled;
		else
		    values->fill_style = FillSolid;
		valueMask |= GCFillStyle;
	    }
	    else if (car == Qfill_rule)
	    {
		if (cdr == QEvenOddRule)
		    values->fill_rule = EvenOddRule;
		else if (cdr == QWindingRule)
		    values->fill_rule = WindingRule;
		else
		    values->fill_rule = EvenOddRule;
		valueMask |= GCFillRule;
	    }
	    else if (car == Qarc_mode)
	    {
		if (cdr == QArcChord)
		    values->arc_mode = ArcChord;
		else if (cdr == QArcPieSlice)
		    values->arc_mode = ArcPieSlice;
		else
		    values->arc_mode = ArcPieSlice;
		valueMask |= GCArcMode;
	    }
	    else if ((car == Qtile) && X_PIXMAPP (cdr))
	    {
		values->tile = VX_DRAWABLE (cdr)->id;
		valueMask |= GCTile;
	    }
	    else if ((car == Qstipple) && X_BITMAPP (cdr))
	    {
		values->stipple = VX_DRAWABLE (cdr)->id;
		valueMask |= GCStipple;
	    }
	    else if ((car == Qts_x_origin) && rep_INTP (cdr))
	    {
		values->ts_x_origin = rep_INT (cdr);
		valueMask |= GCTileStipXOrigin;
	    }
	    else if ((car == Qts_y_origin) && rep_INTP (cdr))
	    {
		values->ts_y_origin = rep_INT (cdr);
		valueMask |= GCTileStipYOrigin;
	    }
	    else if ((car == Qclip_mask) && X_BITMAPP (cdr))
	    {
		values->clip_mask = VX_DRAWABLE (cdr)->id;
		valueMask |= GCClipMask;
	    }
	    else if ((car == Qclip_x_origin) && rep_INTP (cdr))
	    {
		values->clip_x_origin = rep_INT (cdr);
		valueMask |= GCClipXOrigin;
	    }
	    else if ((car == Qclip_y_origin) && rep_INTP (cdr))
	    {
		values->clip_y_origin = rep_INT (cdr);
		valueMask |= GCClipYOrigin;
	    }
	    else if (car == Qfunction && rep_SYMBOLP (cdr))
	    {
		values->function = x_function_from_sym (cdr);
		valueMask |= GCFunction;
	    }
	}

	attrs = rep_CDR (attrs);
    }

    return valueMask;
}

static repv
new_gc (Drawable id)
{
    XGCValues gcv;
    GC gc;
    Lisp_X_GC *g;

    gc = XCreateGC (dpy, id, 0, &gcv);

    g = rep_ALLOC_CELL (sizeof (Lisp_X_GC));
    rep_data_after_gc += sizeof (Lisp_X_GC);
    g->car = x_gc_type;
    g->next = x_gc_list;
    x_gc_list = g;

    g->gc = gc;
    g->id = id;

    g->fg_copy.pixel = 0;
    g->fg_copy.red = g->fg_copy.green = g->fg_copy.blue = 0;

    return rep_VAL (g);
}

DEFUN ("x-create-gc", Fx_create_gc, Sx_create_gc,
       (repv window, repv attrs), rep_Subr2) /*
::doc:sawfish.wm.util.x#x-create-gc::
x-create-gc WINDOW ATTRS

Creates a new GC for the specified window. ATTRS should be an alist
mapping attributes to values. Known attributes are `foreground' and
`background'.
::end:: */
{
    Drawable id;
    repv gc;

    if (dpy == 0)
	return Qnil;

    id = drawable_from_arg (window);

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE2 (attrs, rep_LISTP);

    gc = new_gc (id);
    if (gc != rep_NULL)
    {
	XGCValues gcv;
	unsigned long mask;

	mask = x_gc_parse_attrs (VX_GC (gc), &gcv, attrs);
	if (mask != 0)
	    XChangeGC (dpy, VX_GC (gc)->gc, mask, &gcv);
    }

    return gc;
}

DEFUN ("x-create-root-xor-gc", Fx_create_root_xor_gc,
       Sx_create_root_xor_gc, (void), rep_Subr0) /*
::doc:sawfish.wm.util.x#x-create-root-xor-gc:
x-create-root-xor-gc
::end:: */
{
    XGCValues gcv;
    long black, white;
    repv gc;

    if (dpy == 0)
	return Qnil;

    black = BlackPixel (dpy, screen_num);
    white = WhitePixel (dpy, screen_num);

    gcv.line_width = 0;
    /* I don't understand this, but it works */
    gcv.function = GXxor;
    gcv.foreground = black ^ white;
    gcv.plane_mask = black ^ white;
    gcv.subwindow_mode = IncludeInferiors;

    gc = new_gc (root_window);
    if (gc != rep_NULL)
    {
	XChangeGC (dpy, VX_GC (gc)->gc, GCFunction | GCForeground
		   | GCSubwindowMode | GCLineWidth | GCPlaneMask, &gcv);
    }

    return gc;
}

DEFUN ("x-change-gc", Fx_change_gc, Sx_change_gc,
       (repv gc, repv attrs), rep_Subr2) /*
::doc:sawfish.wm.util.x#x-change-gc::
x-change-gc X-GC ATTRS

Sets attributes of the X-GC. ATTRS should be a list of cons cells mapping
attributes to values. Known attributes are `foreground' and `background'.
::end:: */
{
    XGCValues values;
    long valueMask;

    rep_DECLARE1 (gc, X_GCP);
    rep_DECLARE2 (attrs, rep_LISTP);

    valueMask = x_gc_parse_attrs (VX_GC (gc), &values, attrs);

    if (valueMask)
      XChangeGC (dpy, VX_GC (gc)->gc, valueMask, &values);

    return Qt;
}

DEFUN ("x-destroy-gc", Fx_destroy_gc, Sx_destroy_gc, (repv gc), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-destroy-gc::
x-destroy-gc X-GC

Destroy the X-GC.
::end:: */
{
    rep_DECLARE1 (gc, X_GCP);

    XFreeGC (dpy, VX_GC (gc)->gc);
    VX_GC (gc)->gc = NULL;

    return Qt;
}

DEFUN ("x-gc-p", Fx_gc_p, Sx_gc_p, (repv gc), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-gc-p::
x-gcp ARG

Return t if ARG is a X-GC object.
::end:: */
{
    return X_GCP (gc) ? Qt : Qnil;
}

/* Window functions */

static long
x_window_parse_changes (XWindowChanges *changes, repv attrs)
{
    long changesMask = 0;

    while (rep_CONSP (attrs))
    {
	repv tem = rep_CAR (attrs);

	if (rep_CONSP (tem))
	{
	    repv car = rep_CAR (tem);
	    if ((car == Qx) && rep_INTP (rep_CDR (tem)))
	    {
		changes->x = rep_INT (rep_CDR (tem));
		changesMask |= CWX;
	    }
	    else if ((car == Qy) && rep_INTP (rep_CDR (tem)))
	    {
		changes->y = rep_INT (rep_CDR (tem));
		changesMask |= CWY;
	    }
	    else if ((car == Qwidth) && rep_INTP (rep_CDR (tem)))
	    {
		changes->width = rep_INT (rep_CDR (tem));
		changesMask |= CWWidth;
	    }
	    else if ((car == Qheight) && rep_INTP (rep_CDR (tem)))
	    {
		changes->height = rep_INT (rep_CDR (tem));
		changesMask |= CWHeight;
	    }
	    else if ((car == Qborder_width) && rep_INTP (rep_CDR (tem)))
	    {
		changes->border_width = rep_INT (rep_CDR (tem));
		changesMask |= CWBorderWidth;
	    }
	}

	attrs = rep_CDR (attrs);
    }

    return changesMask;
}

static void
x_window_note_changes (Lisp_X_Window *w, long mask, XWindowChanges *changes)
{
    if (mask & CWWidth)
	w->width = changes->width;
    if (mask & CWHeight)
	w->height = changes->height;
}

static long
x_window_parse_attributes (XSetWindowAttributes *attributes, repv attrs)
{
    long attributesMask = 0;

    while (rep_CONSP (attrs))
    {
	repv tem = rep_CAR (attrs);

	if (rep_CONSP (tem))
	{
	    repv car = rep_CAR (tem);
	    if ((car == Qbackground) && COLORP (rep_CDR (tem)))
	    {
		attributes->background_pixel = VCOLOR (rep_CDR (tem))->pixel;
		attributesMask |= CWBackPixel;
	    }
	    else if ((car == Qborder_color) && COLORP (rep_CDR (tem)))
	    {
		attributes->border_pixel = VCOLOR (rep_CDR (tem))->pixel;
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
    if (win != Qnil && VX_DRAWABLE (win)->event_handler != Qnil)
    {
	repv type = Qnil, args = Qnil;
	switch (ev->type)
	{
	case Expose:
	    /* Since we don't provide a method of detecting which
	       part of the window to redraw, ignore all but the last
	       expose event. (Another option is to set the clip
	       rectangle?) */
	    if (ev->xexpose.count == 0)
		type = Qexpose;
	    break;

	    /* XXX other event types..? */
	}
	if (type != Qnil)
	{
	    args = Fcons (type, Fcons (win, args));
	    rep_funcall (VX_DRAWABLE (win)->event_handler, args, rep_FALSE);
	}
    }
}

static Lisp_X_Window *
create_x_drawable (Drawable id, int width, int height)
{
    Lisp_X_Window *w = rep_ALLOC_CELL (sizeof (Lisp_X_Window));
    rep_data_after_gc += sizeof (Lisp_X_Window);
    w->car = x_window_type;
    w->next = x_window_list;
    x_window_list = w;
    w->id = id;
    w->width = width;
    w->height = height;
    w->is_window = w->is_pixmap = w->is_bitmap = 0;
    w->event_handler = Qnil;
    XSaveContext (dpy, id, x_drawable_context, (XPointer) w);
    return w;
}

DEFUN ("x-create-window", Fx_create_window, Sx_create_window,
       (repv xy, repv wh, repv bw, repv attrs, repv ev), rep_Subr5) /*
::doc:sawfish.wm.util.x#x-create-window::
x-create-window (X . Y) (W . H) BW ATTRS [EVENT-HANDLER]

Creates a new X-WINDOW with the specified position, dimensions and
border width. ATTRS should be a list of cons cells mapping attributes
to values. Known attributes are `background' and `border-color'. The
window is created unmapped.
::end:: */
{
    Lisp_X_Window *w;
    Window id;
    XSetWindowAttributes attributes;
    long attributesMask;
    int _x, _y, _w, _h, _bw;

    rep_DECLARE (1, xy, rep_CONSP (xy)
		&& rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE (2, wh, rep_CONSP (wh)
		&& rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));
    rep_DECLARE3 (bw, rep_INTP);
    rep_DECLARE4 (attrs, rep_LISTP);

    _x = rep_INT (rep_CAR (xy));
    _y = rep_INT (rep_CDR (xy));
    _w = rep_INT (rep_CAR (wh));
    _h = rep_INT (rep_CDR (wh));
    _bw = rep_INT (bw);

    attributesMask = x_window_parse_attributes (&attributes, attrs);
    attributes.override_redirect = True;
    attributes.event_mask = ExposureMask;
    attributes.colormap = image_cmap;
    if (! (attributesMask & CWBorderPixel))
    {
	attributes.border_pixel = BlackPixel (dpy,
					      BlackPixel (dpy, screen_num));
	attributesMask |= CWBorderPixel;
    }

    attributesMask |= CWOverrideRedirect | CWEventMask | CWColormap;

    id = XCreateWindow (dpy, root_window, _x, _y, _w, _h, _bw,
			image_depth, InputOutput, image_visual,
			attributesMask, &attributes);

    w = create_x_drawable (id, _w, _h);
    w->event_handler = ev;
    w->is_window = 1;

    register_event_handler (id, x_window_event_handler);

    return rep_VAL (w);
}

DEFUN ("x-create-pixmap", Fx_create_pixmap,
       Sx_create_pixmap, (repv wh), rep_Subr1)
{
    Lisp_X_Window *w;
    Pixmap id;
    int _w, _h;

    rep_DECLARE (1, wh, rep_CONSP (wh)
		&& rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));

    _w = rep_INT (rep_CAR (wh));
    _h = rep_INT (rep_CDR (wh));

    id = XCreatePixmap (dpy, root_window, _w, _h, image_depth);
    w = create_x_drawable (id, _w, _h);
    w->is_pixmap = 1;

    return rep_VAL (w);
}

DEFUN ("x-create-bitmap", Fx_create_bitmap,
       Sx_create_bitmap, (repv wh), rep_Subr1)
{
    Lisp_X_Window *w;
    Pixmap id;
    int _w, _h;

    rep_DECLARE (1, wh, rep_CONSP (wh)
		&& rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));

    _w = rep_INT (rep_CAR (wh));
    _h = rep_INT (rep_CDR (wh));

    id = XCreatePixmap (dpy, root_window, _w, _h, 1);
    w = create_x_drawable (id, _w, _h);
    w->is_bitmap = 1;

    return rep_VAL (w);
}

DEFUN ("x-map-window", Fx_map_window, Sx_map_window,
       (repv win, repv unraised), rep_Subr2) /*
::doc:sawfish.wm.util.x#x-map-window::
x-map-window X-WINDOW [UNRAISED]
::end:: */
{
    rep_DECLARE1 (win, X_WINDOWP);
    if (unraised == Qnil)
	XMapRaised (dpy, VX_DRAWABLE (win)->id);
    else
	XMapWindow (dpy, VX_DRAWABLE (win)->id);
    return Qt;
}

DEFUN ("x-unmap-window", Fx_unmap_window,
       Sx_unmap_window, (repv win), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-unmap-window::
x-unmap-window X-WINDOW
::end:: */
{
    rep_DECLARE1 (win, X_WINDOWP);
    XUnmapWindow (dpy, VX_DRAWABLE (win)->id);
    return Qt;
}

DEFUN ("x-configure-window", Fx_configure_window,
       Sx_configure_window, (repv window, repv attrs), rep_Subr2) /*
::doc:sawfish.wm.util.x#x-configure-window::
x-configure-window X-WINDOW ATTRS

Reconfigures the X-WINDOW. ATTRS should be an alist mapping attribute
names to values. Known attributes include the symbols `x', `y',
`width', `height' and `border-width'.
::end:: */
{
    XWindowChanges changes;
    long changesMask;

    rep_DECLARE1 (window, X_WINDOWP);
    rep_DECLARE2 (attrs, rep_LISTP);

    changesMask = x_window_parse_changes (&changes, attrs);

    if (changesMask)
    {
	XConfigureWindow (dpy, VX_DRAWABLE (window)->id,
			  changesMask, &changes);
	x_window_note_changes (VX_DRAWABLE (window), changesMask, &changes);
    }

    return Qt;
}

DEFUN ("x-change-window-attributes", Fx_change_window_attributes,
       Sx_change_window_attributes, (repv window, repv attrs), rep_Subr2) /*
::doc:sawfish.wm.util.x#x-change-window-attributes::
x-change-window-attributes WINDOW ATTRS

Sets attributes of the X-WINDOW. ATTRS should be an alist mapping
attribute names to values. Known attributes include the symbols
`background' and `border-color'.
::end:: */
{
    XSetWindowAttributes attributes;
    long attributesMask;

    rep_DECLARE1 (window, X_WINDOWP);
    rep_DECLARE2 (attrs, rep_LISTP);

    attributesMask = x_window_parse_attributes (&attributes, attrs);

    if (attributesMask)
    {
	XChangeWindowAttributes (dpy, VX_DRAWABLE (window)->id,
				 attributesMask, &attributes);
    }

    return Qt;
}

DEFUN ("x-destroy-drawable", Fx_destroy_drawable,
       Sx_destroy_drawable, (repv drawable), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-destroy-drawable::
x-destroy-drawable DRAWABLE

Destroys the X-DRAWABLE.
::end:: */
{
    rep_DECLARE1 (drawable, X_DRAWABLEP);

    XDeleteContext (dpy, VX_DRAWABLE (drawable)->id, x_drawable_context);
    if (X_WINDOWP (drawable))
    {
	deregister_event_handler (VX_DRAWABLE (drawable)->id);
	XDestroyWindow (dpy, VX_DRAWABLE (drawable)->id);
    }
    else if (X_PIXMAPP (drawable) || X_BITMAPP (drawable))
	XFreePixmap (dpy, VX_DRAWABLE (drawable)->id);
    VX_DRAWABLE (drawable)->id = 0;

    return Qt;
}

DEFUN ("x-destroy-window", Fx_destroy_window,
       Sx_destroy_window, (repv window), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-destroy-window::
x-destroy-window WINDOW

Destroys the X-WINDOW.
::end:: */
{
    return Fx_destroy_drawable (window);
}

DEFUN ("x-drawable-id", Fx_drawable_id,
       Sx_drawable_id, (repv drawable), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-drawable-id::
x-drawable-id DRAWABLE

Return the X11 drawable-id (an integer) associated with X-DRAWABLE.
::end:: */
{
    rep_DECLARE1 (drawable, X_DRAWABLEP);

    return rep_MAKE_INT (VX_DRAWABLE (drawable)->id);
}

DEFUN ("x-drawable-width", Fx_drawable_width,
       Sx_drawable_width, (repv drawable), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-drawable-width::
x-drawable-width DRAWABLE

Return the width in pixels of X-DRAWABLE.
::end:: */
{
    rep_DECLARE1 (drawable, X_DRAWABLEP);

    return rep_MAKE_INT (VX_DRAWABLE (drawable)->width);
}

DEFUN ("x-drawable-height", Fx_drawable_height,
       Sx_drawable_height, (repv drawable), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-drawable-height::
x-drawable-height DRAWABLE

Return the height in pixels of X-DRAWABLE.
::end:: */
{
    rep_DECLARE1 (drawable, X_DRAWABLEP);

    return rep_MAKE_INT (VX_DRAWABLE (drawable)->height);
}

DEFUN ("x-window-id", Fx_window_id, Sx_window_id, (repv window), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-window-id::
x-window-id WINDOW

Return the X11 window-id (an integer) associated with X-WINDOW.
::end:: */
{
    return Fx_drawable_id (window);
}

DEFUN ("x-drawable-p", Fx_drawable_p,
       Sx_drawable_p, (repv window), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-drawable-p::
x-drawable-p ARG

Return t if ARG is a X-DRAWABLE object.
::end:: */
{
    return X_DRAWABLEP (window) ? Qt : Qnil;
}

DEFUN ("x-window-p", Fx_window_p, Sx_window_p, (repv window), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-window-p::
x-window-p ARG

Return t if ARG is a X-WINDOW object.
::end:: */
{
    return X_WINDOWP (window) ? Qt : Qnil;
}

DEFUN ("x-pixmap-p", Fx_pixmap_p, Sx_pixmap_p, (repv pixmap), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-pixmap-p::
x-pixmap-p ARG

Return t if ARG is a X-PIXMAP object.
::end:: */
{
    return X_PIXMAPP (pixmap) ? Qt : Qnil;
}

DEFUN ("x-bitmap-p", Fx_bitmap_p, Sx_bitmap_p, (repv bitmap), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-bitmap-p::
x-bitmap-p ARG

Return t if ARG is a X-BITMAP object.
::end:: */
{
    return X_BITMAPP (bitmap) ? Qt : Qnil;
}

DEFUN ("x-window-back-buffer", Fx_window_back_buffer,
      Sx_window_back_buffer, (repv win), rep_Subr1)
{
    Window id = window_from_arg (win);
#ifdef HAVE_X11_EXTENSIONS_XDBE_H
    XdbeBackBuffer buf;

    rep_DECLARE (1, win, id != 0);

    buf = x_back_buffer_from_id (id);
    if (buf == 0)
    {
	buf = XdbeAllocateBackBufferName (dpy, id, XdbeBackground);
	XSaveContext (dpy, id, x_dbe_context, (XPointer) buf);
    }

    if (buf == 0)
	buf = id;

    return (buf == 0) ? Qnil : rep_MAKE_INT (buf);
#else
    return (id == 0) ? Qnil : rep_MAKE_INT (id);
#endif
}

DEFUN ("x-window-swap-buffers", Fx_window_swap_buffers,
      Sx_window_swap_buffers, (repv win), rep_Subr1)
{
#ifdef HAVE_X11_EXTENSIONS_XDBE_H
    Window id = window_from_arg (win);
    XdbeBackBuffer buf;

    rep_DECLARE (1, win, id != 0);

    buf = x_back_buffer_from_id (id);
    if (buf != 0)
    {
	XdbeSwapInfo info;
	info.swap_window = id;
	info.swap_action = XdbeBackground;
	XdbeSwapBuffers (dpy, &info, 1);
    }
#endif
    return Qt;
}

/* Drawing functions */

DEFUN ("x-clear-window", Fx_clear_window,
       Sx_clear_window, (repv window), rep_Subr1) /*
::doc:sawfish.wm.util.x#x-clear-window::
x-clear-window WINDOW

Clears the window associated with WINDOW to its background color.
::end:: */
{
    Window id = window_from_arg (window);
    rep_DECLARE (1, window, id != 0);

    XClearWindow (dpy, id);
    return Qt;
}

DEFUN ("x-draw-string", Fx_draw_string, Sx_draw_string,
       (repv window, repv gc, repv xy, repv string, repv font), rep_Subr5) /*
::doc:sawfish.wm.util.x#x-draw-string::
x-draw-string WINDOW GC (X . Y) STRING [FONT]

Draws the specified string at the specified location in the optional
specified font in the window associated with WINDOW.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0;
    int y = 0;
    char *str;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, xy, rep_CONSP (xy)
                 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE4 (string, rep_STRINGP);
    if (font == Qnil)
	font = global_symbol_value (Qdefault_font);
    rep_DECLARE5 (font, FONTP);

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    str = rep_STR (string);

    x_draw_string (id, font, VX_GC (gc)->gc,
                   &VX_GC (gc)->fg_copy, x, y, str, strlen (str));
    return Qt;
}

DEFUN ("x-draw-text", Fx_draw_text, Sx_draw_text,
       (repv window, repv gc, repv xy, repv string), rep_Subr5) /*
::doc:sawfish.wm.util.x#x-draw-text::
x-draw-text WINDOW GC (X . Y) STRING

Draws the specified string at the specified location in the window
* associated with WINDOW.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0;
    int y = 0;
    char *str;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, xy, rep_CONSP (xy)
                 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE4 (string, rep_STRINGP);

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    str = rep_STR (string);

    XTextItem text={str,strlen(str),0,0};

    XDrawText (dpy, id, VX_GC (gc)->gc, x, y, &text, 1);

    return Qt;
}

DEFUN ("x-draw-line", Fx_draw_line, Sx_draw_line,
       (repv window, repv gc, repv start, repv end), rep_Subr4) /*
::doc:sawfish.wm.util.x#x-draw-line::
x-draw-line WINDOW GC (X1 . Y1) (X2 . Y2)

Draws a line from (X1, Y1) to (X2, Y2) in WINDOW, using GC.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x1 = 0, y1 = 0;
    int x2 = 0, y2 = 0;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, start, rep_CONSP (start)
		 && rep_INTP (rep_CAR (start)) && rep_INTP (rep_CDR (start)));
    rep_DECLARE (4, end, rep_CONSP (end)
		 && rep_INTP (rep_CAR (end)) && rep_INTP (rep_CDR (end)));

    x1 = rep_INT (rep_CAR (start));
    y1 = rep_INT (rep_CDR (start));
    x2 = rep_INT (rep_CAR (end));
    y2 = rep_INT (rep_CDR (end));

    XDrawLine (dpy, id, VX_GC (gc)->gc, x1, y1, x2, y2);
    return Qt;
}

DEFUN ("x-draw-rectangle", Fx_draw_rectangle, Sx_draw_rectangle,
       (repv window, repv gc, repv xy, repv wh), rep_Subr4) /*
::doc:sawfish.wm.util.x#x-draw-rectangle::
x-draw-rectangle WINDOW GC (X . Y) (WIDTH . HEIGHT)

Draws a rectangle with top-left corner (X1, Y1) and dimensions (WIDTH,
HEIGHT) in WINDOW, using GC.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, xy, rep_CONSP (xy)
		 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE (4, wh, rep_CONSP (wh)
		 && rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = rep_INT (rep_CAR (wh));
    h = rep_INT (rep_CDR (wh));

    XDrawRectangle (dpy, id, VX_GC (gc)->gc, x, y, w, h);
    return Qt;
}

DEFUN ("x-fill-rectangle", Fx_fill_rectangle, Sx_fill_rectangle,
       (repv window, repv gc, repv xy, repv wh), rep_Subr4) /*
::doc:sawfish.wm.util.x#x-fill-rectangle::
x-fill-rectangle WINDOW GC (X . Y) (WIDTH . HEIGHT)

Draws a filled rectangle with top-left corner (X, Y) and dimensions
(WIDTH, HEIGHT) in WINDOW, using GC.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, xy, rep_CONSP (xy)
		 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE (4, wh, rep_CONSP (wh)
		 && rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = rep_INT (rep_CAR (wh));
    h = rep_INT (rep_CDR (wh));

    XFillRectangle (dpy, id, VX_GC (gc)->gc, x, y, w, h);
    return Qt;
}

DEFUN ("x-draw-arc", Fx_draw_arc, Sx_draw_arc,
       (repv window, repv gc, repv xy, repv wh, repv angle), rep_Subr5) /*
::doc:sawfish.wm.util.x#x-draw-arc::
x-draw-arc WINDOW GC (X . Y) (WIDTH . HEIGHT) (ANGLE1 . ANGLE2)

Draws a single circular or elliptical arc. Each arc is specified by a
rectangle and two angles.

The center of the circle or ellipse is the center of the rectangle, and
the major and minor axes are specified by the width and height.
Positive angles indicate counter-clockwise motion, and negative angles
indicate clockwise motion.

(See XDrawArc (3X11) for more details.)
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;
    int a1 = 0, a2 = 0;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, xy, rep_CONSP (xy)
		 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE (4, wh, rep_CONSP (wh)
		 && rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));
    rep_DECLARE (5, angle, rep_CONSP (angle)
		 && rep_INTP (rep_CAR (angle)) && rep_INTP (rep_CDR (angle)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = rep_INT (rep_CAR (wh));
    h = rep_INT (rep_CDR (wh));
    a1 = rep_INT (rep_CAR (angle));
    a2 = rep_INT (rep_CDR (angle));

    XDrawArc (dpy, id, VX_GC (gc)->gc, x, y, w, h, a1, a2);
    return Qt;
}

DEFUN ("x-fill-arc", Fx_fill_arc, Sx_fill_arc,
       (repv window, repv gc, repv xy, repv wh, repv angle), rep_Subr5) /*
::doc:sawfish.wm.util.x#x-fill-arc::
x-fill-arc WINDOW GC (X . Y) (WIDTH . HEIGHT) (ANGLE1 . ANGLE2)

Draws a single filled circular or elliptical arc. Each arc is specified
by a rectangle and two angles.

The center of the circle or ellipse is the center of the rectangle, and
the major and minor axes are specified by the width and height.
Positive angles indicate counter-clockwise motion, and negative angles
indicate clockwise motion.

(See XFillArc (3X11) for more details.)
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;
    int a1 = 0, a2 = 0;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, xy, rep_CONSP (xy)
		 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE (4, wh, rep_CONSP (wh)
		 && rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));
    rep_DECLARE (5, angle, rep_CONSP (angle)
		 && rep_INTP (rep_CAR (angle)) && rep_INTP (rep_CDR (angle)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = rep_INT (rep_CAR (wh));
    h = rep_INT (rep_CDR (wh));
    a1 = rep_INT (rep_CAR (angle));
    a2 = rep_INT (rep_CDR (angle));

    XFillArc (dpy, id, VX_GC (gc)->gc, x, y, w, h, a1, a2);
    return Qt;
}

DEFUN ("x-fill-polygon", Fx_fill_polygon, Sx_fill_polygon,
       (repv window, repv gc, repv points, repv mode_), rep_Subr4) /*
::doc:sawfish.wm.util.x#x-fill-arc::
x-fill-arc WINDOW GC POINTS [MODE]

Draws a single filled polygon in WINDOW using GC. Each point is `(X . Y)'.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    repv npoints;
    XPoint *xpoints;
    int i, mode;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, points, rep_LISTP (points));

    if (mode_ == Qconvex)
	mode = Convex;
    else if (mode_ == Qnon_convex)
	mode = Nonconvex;
    else
	mode = Complex;

    npoints = Flength (points);
    if (!npoints)
	return rep_NULL;

    npoints = rep_INT (npoints);
    xpoints = alloca (sizeof (XPoint) * npoints);
    for (i = 0; i < npoints; i++)
    {
	if (!rep_CONSP (points) || !rep_CONSP (rep_CAR (points))
	    || !rep_INTP (rep_CAAR (points)) || !rep_INTP (rep_CDAR (points)))
	{
	    return rep_signal_arg_error (points, 3);
	}
	xpoints[i].x = rep_INT (rep_CAAR (points));
	xpoints[i].y = rep_INT (rep_CDAR (points));
	points = rep_CDR (points);
    }

    XFillPolygon (dpy, id, VX_GC (gc)->gc, xpoints,
		  npoints, mode, CoordModeOrigin);
    return Qt;
}

DEFUN ("x-copy-area", Fx_copy_area, Sx_copy_area,
       (repv window, repv gc, repv xy, repv wh, repv dest), rep_Subr5) /*
::doc:sawfish.wm.util.x#x-fill-rectangle::
x-fill-rectangle WINDOW GC (X . Y) (WIDTH . HEIGHT) (DEST-X . DEST-Y)

Copy a region of WINDOW with top-left corner (X, Y) and dimensions
(WIDTH, HEIGHT), to the position (DEST-X, DEST-Y), using GC.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;
    int dx = 0, dy = 0;

    rep_DECLARE (1, window, id != 0);
    rep_DECLARE (2, gc, X_VALID_GCP (gc, id));
    rep_DECLARE (3, xy, rep_CONSP (xy)
		 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    rep_DECLARE (4, wh, rep_CONSP (wh)
		 && rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));
    rep_DECLARE (5, dest, rep_CONSP (dest)
		 && rep_INTP (rep_CAR (dest)) && rep_INTP (rep_CDR (dest)));

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = rep_INT (rep_CAR (wh));
    h = rep_INT (rep_CDR (wh));
    dx = rep_INT (rep_CAR (dest));
    dy = rep_INT (rep_CDR (dest));

    XCopyArea (dpy, id, id, VX_GC (gc)->gc, x, y, w, h, dx, dy);
    return Qt;
}

DEFUN ("x-draw-image", Fx_draw_image, Sx_draw_image,
       (repv img, repv window, repv xy, repv wh), rep_Subr4) /*
::doc:sawfish.wm.util.x#x-draw-image::
x-draw-image IMAGE WINDOW (X . Y) [(WIDTH . HEIGHT)]

Render the image object IMAGE in WINDOW at position (X, Y). If WIDTH
and HEIGHT are defined the image is first scaled to these dimensions,
otherwise it is drawn using its natural dimensions.
::end:: */
{
    Drawable id = drawable_from_arg (window);
    int x = 0, y = 0;
    int w = 0, h = 0;

    rep_DECLARE1 (img, IMAGEP);
    rep_DECLARE (2, window, id != 0);
    rep_DECLARE (3, xy, rep_CONSP (xy)
		 && rep_INTP (rep_CAR (xy)) && rep_INTP (rep_CDR (xy)));
    if (wh != Qnil)
    {
	rep_DECLARE (4, wh, rep_CONSP (wh)
		     && rep_INTP (rep_CAR (wh)) && rep_INTP (rep_CDR (wh)));
    }

    x = rep_INT (rep_CAR (xy));
    y = rep_INT (rep_CDR (xy));
    w = ((wh == Qnil)
	 ? image_width (VIMAGE (img))
	 : rep_INT (rep_CAR (wh)));
    h = ((wh == Qnil)
	 ? image_height (VIMAGE (img))
	 : rep_INT (rep_CDR (wh)));

    paste_image_to_drawable (VIMAGE (img), id, x, y, w, h);

    return Qt;
}

DEFUN ("x-grab-image-from-drawable", Fx_grab_image_from_drawable,
       Sx_grab_image_from_drawable, (repv drawable, repv mask), rep_Subr2)
{
    Drawable d = drawable_from_arg (drawable);
    Drawable m = drawable_from_arg (mask);
    rep_DECLARE (1, drawable, d != 0);
    return Fmake_image_from_x_drawable (rep_MAKE_INT (d),
					m == 0 ? Qnil : rep_MAKE_INT (m));
}

/* Utility functions */

DEFUN ("x-gc-set-dashes", Fx_gc_set_dashes, Sx_gc_set_dashes,
       (repv _gc, repv _dash_list, repv _offset), rep_Subr3) /*
::doc:sawfish.wm.util.x#x-gc-set-dashes::
x-gc-set-dashes GC ((PIXELS-ON . PIXELS-OFF) [...]) [OFFSET]

Set the dash style to the specified list, optionally setting
the offset.
::end:: */
{
    char *dash_list;
    int num_dashes;
    int offset;

    int i;
    repv tmp;

    rep_DECLARE1 (_gc, X_GCP);
    rep_DECLARE2 (_dash_list, rep_LISTP);
    offset = rep_INTP (_offset) ? rep_INT (_offset) : 0;

    /* Count the number of dashes we've got. */
    i = rep_INT (Flength (_dash_list));
    if (i == 0)
	return Qnil;			/* XXX this looks wrong? */

    /* Each cons represents 2 elements, on/off */
    dash_list  = alloca (2*i);
    num_dashes = 2*i;

    for (i = 0, tmp = _dash_list; tmp != Qnil; i += 2, tmp = rep_CDR (tmp))
    {
	repv cons = rep_CAR (tmp);

	if (rep_CONSP (cons)
	    && rep_INTP (rep_CAR (cons))
	    && rep_INTP (rep_CDR (cons)))
	{
	    dash_list[i] = rep_INT (rep_CAR (cons));
	    dash_list[i+1] = rep_INT (rep_CDR (cons));
	}
	else
	{
	    dash_list[i] = 1;
	    dash_list[i+1] = 1;
	}
    }

    XSetDashes (dpy, VX_GC (_gc)->gc, offset, dash_list, num_dashes);
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
	if (!rep_GC_CELL_MARKEDP (rep_VAL (w)))
	{
	    if (w->gc != 0)
		Fx_destroy_gc (rep_VAL (w));
	    rep_FREE_CELL (w);
	}
	else
	{
	    rep_GC_CLR_CELL (rep_VAL (w));
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
    sprintf (buf, "#<x-drawable 0x%lx>", VX_DRAWABLE (obj)->id);
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
x_window_mark (repv obj)
{
    rep_MARKVAL (VX_DRAWABLE (obj)->event_handler);
}

static void
x_window_sweep (void)
{
    Lisp_X_Window *w = x_window_list;
    x_window_list = 0;
    while (w != 0)
    {
	Lisp_X_Window *next = w->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL (w)))
	{
	    if (w->id != 0)
		Fx_destroy_window (rep_VAL (w));
	    rep_FREE_CELL (w);
	}
	else
	{
	    rep_GC_CLR_CELL (rep_VAL (w));
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
    repv tem;
    x_gc_type = rep_register_new_type ("x-gc", x_gc_cmp, x_gc_prin, x_gc_prin,
				       x_gc_sweep, x_gc_mark,
				       0, 0, 0, 0, 0, 0, 0);

    tem = rep_push_structure ("sawfish.wm.util.x");
    /* ::alias:x sawfish.wm.util.x:: */
    rep_alias_structure ("x");

    rep_ADD_SUBR (Sx_create_gc);
    rep_ADD_SUBR (Sx_create_root_xor_gc);
    rep_ADD_SUBR (Sx_change_gc);
    rep_ADD_SUBR (Sx_destroy_gc);
    rep_ADD_SUBR (Sx_gc_p);

    x_drawable_context = XUniqueContext ();

    x_window_type = rep_register_new_type ("x-window", x_window_cmp,
					   x_window_prin, x_window_prin,
					   x_window_sweep, x_window_mark,
					   0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR (Sx_create_window);
    rep_ADD_SUBR (Sx_create_pixmap);
    rep_ADD_SUBR (Sx_create_bitmap);
    rep_ADD_SUBR (Sx_map_window);
    rep_ADD_SUBR (Sx_unmap_window);
    rep_ADD_SUBR (Sx_configure_window);
    rep_ADD_SUBR (Sx_change_window_attributes);
    rep_ADD_SUBR (Sx_destroy_drawable);
    rep_ADD_SUBR (Sx_destroy_window);
    rep_ADD_SUBR (Sx_drawable_p);
    rep_ADD_SUBR (Sx_window_p);
    rep_ADD_SUBR (Sx_pixmap_p);
    rep_ADD_SUBR (Sx_bitmap_p);
    rep_ADD_SUBR (Sx_drawable_id);
    rep_ADD_SUBR (Sx_drawable_width);
    rep_ADD_SUBR (Sx_drawable_height);
    rep_ADD_SUBR (Sx_window_id);
    rep_ADD_SUBR (Sx_window_back_buffer);
    rep_ADD_SUBR (Sx_window_swap_buffers);

    rep_ADD_SUBR (Sx_clear_window);
    rep_ADD_SUBR (Sx_draw_string);
    rep_ADD_SUBR (Sx_draw_text);
    rep_ADD_SUBR (Sx_draw_line);
    rep_ADD_SUBR (Sx_draw_rectangle);
    rep_ADD_SUBR (Sx_fill_rectangle);
    rep_ADD_SUBR (Sx_draw_arc);
    rep_ADD_SUBR (Sx_fill_arc);
    rep_ADD_SUBR (Sx_fill_polygon);
    rep_ADD_SUBR (Sx_copy_area);
    rep_ADD_SUBR (Sx_draw_image);

    rep_ADD_SUBR (Sx_grab_image_from_drawable);

    rep_ADD_SUBR (Sx_gc_set_dashes);

    rep_INTERN (x);
    rep_INTERN (y);
    rep_INTERN (border_width);
    rep_INTERN (border_color);
    rep_INTERN (expose);
    rep_INTERN (convex);
    rep_INTERN (non_convex);
    rep_INTERN (line_width);
    rep_INTERN (line_style);
    rep_INTERN (cap_style);
    rep_INTERN (join_style);
    rep_INTERN (fill_style);
    rep_INTERN (fill_rule);
    rep_INTERN (arc_mode);
    rep_INTERN (tile);
    rep_INTERN (stipple);
    rep_INTERN (ts_x_origin);
    rep_INTERN (ts_y_origin);
    rep_INTERN (clip_mask);
    rep_INTERN (clip_x_origin);
    rep_INTERN (clip_y_origin);

    rep_INTERN (LineSolid);
    rep_INTERN (LineOnOffDash);
    rep_INTERN (LineDoubleDash);
    rep_INTERN (CapNotLast);
    rep_INTERN (CapButt);
    rep_INTERN (CapRound);
    rep_INTERN (CapProjecting);
    rep_INTERN (JoinMiter);
    rep_INTERN (JoinRound);
    rep_INTERN (JoinBevel);
    rep_INTERN (FillSolid);
    rep_INTERN (FillTiled);
    rep_INTERN (FillStippled);
    rep_INTERN (FillOpaqueStippled);
    rep_INTERN (EvenOddRule);
    rep_INTERN (WindingRule);
    rep_INTERN (ArcChord);
    rep_INTERN (ArcPieSlice);
    rep_INTERN (function);

    rep_INTERN (clear);
    rep_INTERN (and);
    rep_INTERN (andReverse);
    rep_INTERN (copy);
    rep_INTERN (andInverted);
    rep_INTERN (noop);
    rep_INTERN (xor);
    rep_INTERN (or);
    rep_INTERN (nor);
    rep_INTERN (equiv);
    rep_INTERN (invert);
    rep_INTERN (orReverse);
    rep_INTERN (copyInverted);
    rep_INTERN (orInverted);
    rep_INTERN (nand);
    rep_INTERN (set);

    x_fns[0] = GXclear; r_fns[0] = Qclear;
    x_fns[1] = GXand; r_fns[1] = Qand;
    x_fns[2] = GXandReverse; r_fns[2] = QandReverse;
    x_fns[3] = GXcopy; r_fns[3] = Qcopy;
    x_fns[4] = GXandInverted; r_fns[4] = QandInverted;
    x_fns[5] = GXnoop; r_fns[5] = Qnoop;
    x_fns[6] = GXxor; r_fns[6] = Qxor;
    x_fns[7] = GXor; r_fns[7] = Qor;
    x_fns[8] = GXnor; r_fns[8] = Qnor;
    x_fns[9] = GXequiv; r_fns[9] = Qequiv;
    x_fns[10] = GXinvert; r_fns[10] = Qinvert;
    x_fns[11] = GXorReverse; r_fns[11] = QorReverse;
    x_fns[12] = GXcopyInverted; r_fns[12] = QcopyInverted;
    x_fns[13] = GXorInverted; r_fns[13] = QorInverted;
    x_fns[14] = GXnand; r_fns[14] = Qnand;
    x_fns[15] = GXset; r_fns[15] = Qset;

#ifdef HAVE_X11_EXTENSIONS_XDBE_H
    if (dpy != 0)
    {
	int major, minor;
	if (XdbeQueryExtension (dpy, &major, &minor))
	{
	    have_dbe = TRUE;
	    x_dbe_context = XUniqueContext ();
	}
    }
#endif

    return rep_pop_structure (tem);
}
