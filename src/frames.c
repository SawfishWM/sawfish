/* frames.c -- window frame manipulation
   $Id$

   Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>

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
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/extensions/shape.h>
#include <assert.h>

static XID window_fp_context;

int frame_part_type;
static struct frame_part *allocated_parts;

DEFSYM(internal, "internal");
DEFSYM(tiled, "tiled");
DEFSYM(center, "center");
DEFSYM(right, "right");
DEFSYM(left, "left");
DEFSYM(top, "top");
DEFSYM(bottom, "bottom");
DEFSYM(text, "text");
DEFSYM(x_justify, "x-justify");
DEFSYM(y_justify, "y-justify");
DEFSYM(background, "background");
DEFSYM(foreground, "foreground");
DEFSYM(renderer, "renderer");
DEFSYM(render_scale, "render-scale");
DEFSYM(font, "font");
DEFSYM(width, "width");
DEFSYM(height, "height");
DEFSYM(left_edge, "left-edge");
DEFSYM(top_edge, "top-edge");
DEFSYM(right_edge, "right-edge");
DEFSYM(bottom_edge, "bottom-edge");
DEFSYM(cursor, "cursor");
DEFSYM(focused, "focused");
DEFSYM(highlighted, "highlighted");
DEFSYM(clicked, "clicked");
DEFSYM(inactive, "inactive");
DEFSYM(inactive_highlighted, "inactive-highlighted");
DEFSYM(inactive_clicked, "inactive-clicked");
DEFSYM(hide_client, "hide-client");
DEFSYM(class, "class");
DEFSYM(removable, "removable");
DEFSYM(removed_classes, "removed-classes");
DEFSYM(frame_part_classes, "frame-part-classes");
DEFSYM(override_frame_part_classes, "override-frame-part-classes");
DEFSYM(below_client, "below-client");
DEFSYM(scale_foreground, "scale-foreground");
DEFSYM(hidden, "hidden");
DEFSYM(border_width, "border-width");
DEFSYM(border_color, "border-color");

static repv state_syms[fps_MAX];

static bool frame_draw_mutex;
bool frame_state_mutex;

/* type hooks */

static struct frame_part *
fp_new (Lisp_Window *win, repv alist)
{
    struct frame_part *fp = rep_ALLOC_CELL (sizeof (struct frame_part));
    rep_data_after_gc += sizeof (struct frame_part);
    memset (fp, 0, sizeof (struct frame_part));
    fp->car = frame_part_type;
    fp->win = win;
    fp->alist = alist;
    fp->local_alist = Qnil;
    fp->next_alloc = allocated_parts;
    allocated_parts = fp;
    return fp;
}

static int
fp_cmp (repv w1, repv w2)
{
    return w1 != w2;
}

static void
fp_prin (repv stream, repv win)
{
    char buf[128];
    sprintf (buf, "#<frame-part %lx>", VPART(win)->id);
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
fp_mark (repv obj)
{
    struct frame_part *fp;
    for (fp = VPART(obj); fp != 0; fp = fp->next)
    {
	int i;
	rep_MARKVAL(fp->alist);
	rep_MARKVAL(fp->local_alist);
	rep_MARKVAL(rep_VAL(fp->win));
	for (i = 0; i < fps_MAX; i++)
	{
	    rep_MARKVAL(fp->font[i]);
	    rep_MARKVAL(fp->fg[i]);
	    rep_MARKVAL(fp->bg[i]);
	}
	rep_MARKVAL(rep_VAL(fp->cursor));
	rep_MARKVAL(rep_VAL(fp->renderer));
	rep_MARKVAL(rep_VAL(fp->rendered_image));
	rep_MARKVAL(fp->drawn.font);
	rep_MARKVAL(fp->drawn.text);
	rep_MARKVAL(fp->drawn.x_justify);
	rep_MARKVAL(fp->drawn.y_justify);
	rep_MARKVAL(fp->drawn.fg);
	rep_MARKVAL(fp->drawn.bg);
    }
}

static void
fp_sweep (void)
{
    struct frame_part *fp = allocated_parts;
    allocated_parts = 0;
    while (fp != 0)
    {
	struct frame_part *next = fp->next_alloc;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(fp)))
	{
	    assert (fp->next == 0);
	    assert (fp->id == 0);
#if 0
	    /* XXX This assertion has been reported to trigger (when opening
	       ddd and acroread -- both motif apps?). I don't see how this
	       happens but since fp->id is zero and fp->win will get gc'd,
	       we're not going to leak any resources, so.. */
	    assert (fp->win == 0);
#endif
	    rep_FREE_CELL(fp);
	}
	else
	{
	    fp->next_alloc = allocated_parts;
	    allocated_parts = fp;
	    rep_GC_CLR_CELL(rep_VAL(fp));
	}
	fp = next;
    }
}

/* building frames from component lists

   build the frame from the list of components. Each element
   in the list is an alist representing one component of the
   frame, possible tags include:

	class . SYMBOL

	background . IMAGE-OR-COLOR
	background . (NORMAL FOCUSED HIGHLIGHTED CLICKED)
	background . ((STATE . VALUE) ...)
	foreground . COLOR
	foreground . (NORMAL FOCUSED HIGHLIGHTED CLICKED)
	foreground . ((STATE . VALUE) ...)

	renderer . FUNCTION
	render-scale . INTEGER

	text . STRING-OR-FUNCTION-OR-NIL
	x-justify . left OR right OR center OR NUMBER
	y-justify . top OR bottom OR center OR NUMBER

	font . FONT
	font . (NORMAL FOCUSED HIGHLIGHTED CLICKED)
	font . ((STATE . FONT) ...)

	left-edge . POSITION-REL-LEFT
	right-edge . POSITION-REL-RIGHT
	top-edge . POSITION-REL-TOP
	bottom-edge . POSITION-REL-BOTTOM
	height . PIXELS
	width . PIXELS

	border-width . NUMBER
	border-color . COLOR

	keymap . KEYMAP
	cursor . CURSOR-OR-CURSOR-DEF

	below-client . t
	scale-foreground . t

   STATE's are one of: inactive, focused, highlighted, clicked,
   inactive-highlighted, inactive-clicked.

   Note that all numeric quantities may be defined dynamically by
   substituting a function */

int
current_state (struct frame_part *fp)
{
    if (fp->clicked)
    {
	if (fp->win == focus_window)
	    return fps_clicked;
	else
	    return fps_inactive_clicked;
    }
    else if (fp->highlighted)
    {
	if (fp->win == focus_window)
	    return fps_highlighted;
	else
	    return fps_inactive_highlighted;
    }
    else if (fp->win == focus_window)
	return fps_focused;
    else
	return fps_inactive;
}

/* Call FUN (ARG) with error protection in place. If an error occurs
   a message is printed, and ERROR-RETURN is returned, else just return
   the result of calling FUN. */
static repv
call_protectedly (repv (*fun)(repv), repv arg, repv error_return)
{
    rep_GC_root gc_error_return;
    repv result;
    rep_PUSHGC (gc_error_return, error_return);
    result = fun (arg);
    if (result == rep_NULL)
    {
	repv throw = rep_throw_value, stream;
	rep_throw_value = rep_NULL;
        stream = Fstderr_file();
        if (stream != rep_NULL)
        {
	    rep_stream_puts (stream, "frame error: ", -1, rep_FALSE);
	    if (rep_CAR (throw) == Qerror)
		throw = rep_CDR (throw);
            rep_print_val (stream, throw);
            rep_stream_putc (stream, '\n');
        }
	result = error_return;
    }
    rep_POPGC;
    return result;
}

/* Call lisp function FUN with the single arg ARG, with error protection */
static repv
call_protectedly_1 (repv fun, repv arg, repv error_return)
{
    return call_protectedly (Ffuncall, rep_LIST_2 (fun, arg), error_return);
}

static void
apply_mask (Drawable dest, int x_off, int y_off,
	    int dest_width, int dest_height,
	    Pixmap src, int src_width, int src_height)
{
    if (dest_width - x_off >= src_width
	&& dest_height - y_off >= src_height)
    {
	/* No need for a temporary buffer */
	XShapeCombineMask (dpy, dest, ShapeBounding,
			   x_off, y_off, src, ShapeUnion);
    }
    else
    {
	Pixmap tem;
	XGCValues gcv;
	GC gc;

	tem = XCreatePixmap (dpy, src, dest_width - x_off,
			     dest_height - y_off, 1);

	gcv.graphics_exposures = False;
	gc = XCreateGC (dpy, tem, GCGraphicsExposures, &gcv);

	XCopyArea (dpy, src, tem, gc, 0, 0,
		   dest_width - x_off, dest_height - y_off, 0, 0);
	XShapeCombineMask (dpy, dest, ShapeBounding,
			   x_off, y_off, tem, ShapeUnion);

	XFreeGC (dpy, gc);
	XFreePixmap (dpy, tem);
    }
}

/* Construct the frame window's shape mask from the union of all
   individual shapes (frame parts and the client window, if appropriate).
   If ATOMIC is true, then the frame shape is changed _once_ only, using
   a temporary buffer to construct the new shape, then copying it
   to the frame. */
void
set_frame_shapes (Lisp_Window *w, bool atomic)
{
    Window shape_win;
    int nrects, nparts;
    XRectangle *rects;
    struct frame_part *fp;

    if (atomic)
    {
	XSetWindowAttributes wa;
	int wamask;
	wa.colormap = image_cmap;
	wa.border_pixel = w->border_pixel;
	wamask = CWColormap | CWBorderPixel;
	shape_win = XCreateWindow (dpy, root_window, -100, -100,
				   w->frame_width, w->frame_height,
				   w->border_width, image_depth, InputOutput,
				   image_visual, wamask, &wa);
    }
    else
	shape_win = w->frame;

    nparts = 0;
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
	nparts++;
    nrects = 0;
    rects = alloca (sizeof (XRectangle) * (nparts + 1));

    rects[0].x = rects[0].y = 0;
    rects[0].width = w->frame_width;
    rects[0].height = w->frame_height;
    XShapeCombineRectangles (dpy, shape_win, ShapeBounding,
			     0, 0, rects, 1, ShapeSubtract, Unsorted);

    if (!w->client_hidden)
    {
	if (w->shaped)
	{
	    XShapeCombineShape (dpy, shape_win, ShapeBounding,
				-w->frame_x, -w->frame_y, w->id,
				ShapeBounding, ShapeSet);
	}
	else
	{
	    rects[nrects].x = -w->frame_x;
	    rects[nrects].y = -w->frame_y;
	    rects[nrects].width = w->attr.width;
	    rects[nrects].height = w->attr.height;
	    nrects++;
	}
#ifdef ShapeInput
       	XShapeCombineShape (dpy, shape_win, ShapeInput,
			    -w->frame_x, -w->frame_y, w->id,
			    ShapeBounding, ShapeSubtract);
       	XShapeCombineShape (dpy, shape_win, ShapeInput,
			    -w->frame_x, -w->frame_y, w->id,
			    ShapeInput, ShapeUnion);
#endif
    }

    for (fp = w->frame_parts; fp != 0 && !WINDOW_IS_GONE_P (w); fp = fp->next)
    {
	Pixmap pixmap, mask;
	int state = current_state (fp);

	if (fp->width <= 0 || fp->height <= 0)
	    continue;

	if (IMAGEP(fp->bg[state]))
	{
	    bool tiled = FALSE;
	    Lisp_Image *image = VIMAGE(fp->bg[state]);
	    repv tem;

	    tem = Fimage_get (rep_VAL(image), Qtiled);
	    if (tem && tem != Qnil)
		tiled = TRUE;

	    if (tiled)
	    {
		image_render (image, image_width (image), image_height (image),
			      &pixmap, &mask);
	    }
	    else
	    {
		image_render (image, fp->width, fp->height, &pixmap, &mask);
	    }

	    if (mask != 0)
	    {
		int width = !tiled ? fp->width : image_width (image);
		int height = !tiled ? fp->height : image_height (image);
		int x_off = fp->x - w->frame_x;
		int y_off = fp->y - w->frame_y;

		apply_mask (shape_win, x_off, y_off,
			    x_off + fp->width, y_off + fp->height,
			    mask, width, height);

		if (tiled)
		{
		    int x = width, y = 0;
		    while (y < fp->height)
		    {
			while (x < fp->width)
			{
			    apply_mask (shape_win, x_off + x, y_off + y,
					x_off + fp->width, y_off + fp->height,
					mask, width, height);
			    x += width;
			}
			y += height;
			x = 0;
		    }
		}
	    }
	    else
	    {
		rects[nrects].x = fp->x - w->frame_x;
		rects[nrects].y = fp->y - w->frame_y;
		rects[nrects].width = fp->width;
		rects[nrects].height = fp->height;
		nrects++;
	    }
	    image_free_pixmaps (image, pixmap, mask);
	}
	else
	{
	    rects[nrects].x = fp->x - w->frame_x;
	    rects[nrects].y = fp->y - w->frame_y;
	    rects[nrects].width = fp->width;
	    rects[nrects].height = fp->height;
	    nrects++;
	}
    }

    if (nrects > 0)
    {
	XShapeCombineRectangles (dpy, shape_win, ShapeBounding, 0, 0, rects,
				 nrects, ShapeUnion, Unsorted);
    }

    if (atomic)
    {
	XShapeCombineShape (dpy, w->frame, ShapeBounding,
			    0, 0, shape_win, ShapeBounding, ShapeSet);
#ifdef ShapeInput
	XShapeCombineShape (dpy, w->frame, ShapeInput,
			    0, 0, shape_win, ShapeInput, ShapeSet);
#endif
	XDestroyWindow (dpy, shape_win);
    }

    w->pending_reshape = 0;
}

/* Queue an atomic reshape for the frame of W. It will happen sometime
   in the near future. */
void
queue_reshape_frame (Lisp_Window *w)
{
    w->pending_reshape = 1;
}

void
commit_queued_reshapes (void)
{
    Lisp_Window *w;
    for (w = window_list; w != 0; w = w->next)
    {
	if (!WINDOW_IS_GONE_P (w) && w->pending_reshape)
	    set_frame_shapes (w, TRUE);
    }
}

/* Draw the background of the frame-part FP. This is either a solid color
   or an image (scaled or tiled) */
static void
set_frame_part_bg (struct frame_part *fp)
{
    int state = current_state (fp);
    repv bg = fp->bg[state];
    Lisp_Window *win = fp->win;

    if (fp->id == 0)
	return;

    if (fp->renderer != Qnil && IMAGEP(fp->rendered_image))
    {
	bg = fp->rendered_image;
	if (fp->rendered_state != state)
	{
	    rep_call_lisp2 (fp->renderer, bg, state_syms[state]);
	    fp->rendered_state = state;
	}
	fp->drawn.bg = Qnil;
    }

    if (WINDOW_IS_GONE_P (win))
	return;

    if (COLORP(bg))
    {
	if (bg != fp->drawn.bg)
	{
	    XGCValues gcv;
	    gcv.foreground = VCOLOR(bg)->pixel;
	    XChangeGC (dpy, fp->gc, GCForeground, &gcv);
	    XFillRectangle (dpy, fp->id, fp->gc, 0, 0, fp->width, fp->height);
	    fp->drawn.bg = bg;
	    fp->drawn.fg = rep_NULL;
	}
    }
    else if (IMAGEP(bg))
    {
	Lisp_Image *image = VIMAGE(bg);
	Pixmap bg_pixmap, bg_mask;
	bool tiled = FALSE;
	repv tem;

	if (fp->drawn.bg == bg)
	    return;

	tem = Fimage_get (rep_VAL(image), Qtiled);
	if (tem && tem != Qnil)
	    tiled = TRUE;

	if (tiled)
	{
	    image_render (image, image_width (image), image_height (image),
			  &bg_pixmap, &bg_mask);
	}
	else
	{
	    image_render (image, fp->width, fp->height, &bg_pixmap, &bg_mask);
	}

	/* Some of the Imlib_ functions call XSync on our display. In turn
	   this can cause the error handler to run if a window has been
	   deleted. This then invalidates the window we're updating */
	if (WINDOW_IS_GONE_P (win))
	    return;

	if (bg_mask == 0)
	{
	    /* No mask, so we always want to force the rectangle
	       including the frame part to be shown.. */
	    XRectangle rect;
	    rect.x = rect.y = 0;
	    rect.width = fp->width;
	    rect.height = fp->height;
	    XShapeCombineRectangles (dpy, fp->id, ShapeBounding,
				     0, 0, &rect, 1, ShapeSet, Unsorted);
	}

	if (!tiled)
	{
	    XCopyArea (dpy, bg_pixmap, fp->id, fp->gc, 0, 0,
		       fp->width, fp->height, 0, 0);
	    if (bg_mask != 0)
	    {
		XShapeCombineMask (dpy, fp->id, ShapeBounding,
				   0, 0, bg_mask, ShapeSet);
	    }
	}
	else
	{
	    Window tem = 0;
	    int y = 0;
	    if (bg_mask != 0)
	    {
		XRectangle rect;
		XSetWindowAttributes wa;
		int wamask;
		wa.colormap = image_cmap;
		wa.border_pixel = BlackPixel (dpy, screen_num);
		wamask = CWColormap | CWBorderPixel;
		tem = XCreateWindow (dpy, win->frame, -100, -100,
				     fp->width, fp->height,
				     0, image_depth, InputOutput,
				     image_visual, wamask, &wa);
		rect.x = rect.y = 0;
		rect.width = fp->width;
		rect.height = fp->height;
		XShapeCombineRectangles (dpy, tem, ShapeBounding, 0, 0,
					 &rect, 1, ShapeSubtract, Unsorted);
	    }
	    while (y < fp->height)
	    {
		int x = 0;
		int width = image_width (image);
		int height = image_height (image);
		while (x < fp->width)
		{
		    XCopyArea (dpy, bg_pixmap, fp->id, fp->gc,
			       0, 0, width, height, x, y);
		    if (bg_mask != 0)
		    {
			XShapeCombineMask (dpy, tem, ShapeBounding,
					   x, y, bg_mask, ShapeUnion);
		    }
		    x += width;
		}
		y += height;
	    }
	    if (bg_mask != 0)
	    {
		XShapeCombineShape (dpy, fp->id, ShapeBounding, 0, 0,
				    tem, ShapeBounding, ShapeSet);
		XDestroyWindow (dpy, tem);
	    }
	}
	image_free_pixmaps (image, bg_pixmap, bg_mask);

	/* Imlib sometimes calls XSync (), which could hide events
	   from select () */
	rep_mark_input_pending (ConnectionNumber(dpy));

	fp->drawn.bg = bg;
	fp->drawn.fg = rep_NULL;

#if 0
	/* FIXME: this was added to let different frame states have
	   different shapes, but it's pretty costly.. */
	queue_reshape_frame (fp->win);
#endif
    }
    else if (Ffunctionp (bg) != Qnil)
    {
	rep_call_lisp1 (bg, rep_VAL(fp));
	fp->drawn.bg = bg;
	fp->drawn.fg = rep_NULL;
    }
    else
	fp->drawn.bg = Qnil;
}

/* Draw the foreground pixels in frame-part FP. */
static void
set_frame_part_fg (struct frame_part *fp)
{
    int state = current_state (fp);
    repv font = fp->font[state], fg = fp->fg[state];
    repv string = rep_NULL;
    int length = 0, width, height, x, y;
    Lisp_Window *win = fp->win;

    if (fp->id == 0)
	return;

    if (fg != Qnil && Ffunctionp (fg) != Qnil)
    {
	rep_call_lisp1 (fg, rep_VAL(fp));
    }
    else if (IMAGEP(fg) || fp->text != Qnil)
    {
	if (!COLORP(fg) && !IMAGEP(fg))
	    fg = global_symbol_value (Qdefault_foreground);
	if (!FONTP(font))
	{
	    font = global_symbol_value (Qdefault_font);
	    if (!FONTP(font))
		goto out;
	}

	if (IMAGEP(fg))
	{
	    if (fp->scale_foreground)
	    {
		width = fp->width;
		height = fp->height;
	    }
	    else
	    {
		width = image_width (VIMAGE(fg));
		height = image_height (VIMAGE(fg));
	    }
	}
	else
	{
	    if (rep_STRINGP(fp->text))
	    {
		string = fp->text;
		length = rep_STRING_LEN(fp->text);
	    }
	    else
	    {
		repv result = rep_call_lisp1 (fp->text, rep_VAL(fp->win));
		if (!result || !rep_STRINGP(result))
		    return;
		string = result;
		length = rep_STRING_LEN(result);
	    }

	    width = x_text_width (font, rep_STR(string), length);
	    height = VFONT(font)->ascent + VFONT(font)->descent;
	}

	if (fp->x_justify == Qcenter)
	    x = MAX(0, (fp->width - width) / 2);
	else if (fp->x_justify == Qright)
	    x = MAX(0, fp->width - width);
	else if (rep_INTP(fp->x_justify))
	{
	    x = rep_INT(fp->x_justify);
	    if (x < 0)
		x = MAX(0, fp->width + x - width);
	}
	else
	    x = 0;

	if (fp->y_justify == Qcenter)
	    y = MAX(0, (fp->height - height) / 2);
	else if (fp->y_justify == Qbottom)
	    y = MAX(0, fp->height - height);
	else if (rep_INTP(fp->y_justify))
	{
	    y = rep_INT(fp->y_justify);
	    if (y < 0)
		y = MAX(0, fp->height + y - height);
	}
	else
	    y = 0;

	if (IMAGEP(fg))
	{
	    XGCValues gcv;
	    unsigned long gcv_mask = 0;
	    Pixmap fg_pixmap, fg_mask;

	    if (fp->drawn.fg == fg
		&& fp->drawn.x_justify == fp->x_justify
		&& fp->drawn.y_justify == fp->y_justify)
	    {
		return;
	    }
	    else if (fp->drawn.fg != rep_NULL)
	    {
		/* there's something drawn in this part already,
		   update the background to clear it */
		fp->drawn.bg = rep_NULL;
		set_frame_part_bg (fp);
	    }

	    image_render (VIMAGE(fg), width, height,
			  &fg_pixmap, &fg_mask);

	    /* Some of the Imlib_ functions call XSync on our display. In turn
	       this can cause the error handler to run if a window has been
	       deleted. This then invalidates the window we're updating */
	    if (WINDOW_IS_GONE_P (win))
		return;

	    if (fg_pixmap)
	    {
		if (fg_mask)
		{
		    gcv.clip_mask = fg_mask;
		    gcv.clip_x_origin = x;
		    gcv.clip_y_origin = y;
		    gcv_mask |= GCClipMask | GCClipXOrigin | GCClipYOrigin;
		}

		XChangeGC (dpy, fp->gc, gcv_mask, &gcv);
		XCopyArea (dpy, fg_pixmap, fp->id, fp->gc,
			   0, 0, MIN(fp->width, width),
			   MIN(fp->height, height), x, y);
		if (fg_mask)
		{
		    gcv.clip_mask = None;
		    gcv.clip_x_origin = 0;
		    gcv.clip_y_origin = 0;
		    XChangeGC (dpy, fp->gc, GCClipMask | GCClipXOrigin
			       | GCClipYOrigin, &gcv);
		}
		image_free_pixmaps (VIMAGE(fg), fg_pixmap, fg_mask);
	    }
	    /* Imlib sometimes calls XSync (), which could hide events
	       from select () */
	    rep_mark_input_pending (ConnectionNumber(dpy));

	    fp->drawn.text = Qnil;
	}
	else if (COLORP(fg) && FONTP(font))
	{
	    if ((fp->drawn.text == string
		 || Fequal (fp->drawn.text, string) != Qnil)
		&& fp->drawn.font == font && fp->drawn.fg == fg
		&& fp->drawn.x_justify == fp->x_justify
		&& fp->drawn.y_justify == fp->y_justify)
	    {
		return;
	    }
 	    else if (fp->drawn.fg != rep_NULL)
	    {
		/* there's something drawn in this part already,
		   update the background to clear it */
		fp->drawn.bg = rep_NULL;
		set_frame_part_bg (fp);
	    }

	    x_draw_string (fp->id, font, fp->gc, VCOLOR(fg),
			   x, y + VFONT(font)->ascent,
                           rep_STR(string), length);

	    fp->drawn.text = string;
	}
    }
out:
    fp->drawn.font = font;
    fp->drawn.fg = fg;
    fp->drawn.x_justify = fp->x_justify;
    fp->drawn.y_justify = fp->y_justify;
}

/* Redraw FP. */
void
refresh_frame_part (struct frame_part *fp)
{
    if (!frame_draw_mutex)
    {
	Lisp_Window *w = fp->win;
	if (w == 0)			/* XXX why is this needed? */
	    return;

	if (fp->drawn.width != fp->width || fp->drawn.height != fp->height)
	    fp->drawn.bg = rep_NULL;

	if (!WINDOW_IS_GONE_P (w) && fp->id != 0)
	    set_frame_part_bg (fp);
	if (!WINDOW_IS_GONE_P (w) && fp->id != 0)
	    set_frame_part_fg (fp);

	fp->drawn.width = fp->width;
	fp->drawn.height = fp->height;
	fp->pending_refresh = 0;
    }
    else
	fp->pending_refresh = 1;
}

/* Redraw frame parts in W. */
void
refresh_frame_parts (Lisp_Window *w)
{
    struct frame_part *fp;
    for (fp = w->frame_parts; !WINDOW_IS_GONE_P (w) && fp != 0; fp = fp->next)
	refresh_frame_part (fp);
}

/* Find the frame-part that is drawn in window ID */
struct frame_part *
find_frame_part_by_window (Window id)
{
    union {
	struct frame_part *f;
	XPointer p;
    } fp;

    return XFindContext (dpy, id, window_fp_context, &fp.p) ? 0 : fp.f;
}

/* Destroy the window frame of W, assuming it's a frame-part derived frame */
static void
frame_part_destroyer (Lisp_Window *w)
{
    struct frame_part *fp, *next;
    for (fp = w->frame_parts; fp != 0; fp = next)
    {
	if (fp->clicked)
	{
	    /* This is necessary so that clicked_frame_part is cleared. */
	    bool old_mutex = frame_draw_mutex;
	    frame_draw_mutex = TRUE;
	    unclick_current_fp ();
	    ungrab_pointer ();
	    frame_draw_mutex = old_mutex;
	}

	if (fp->gc)
	{
	    XFreeGC (dpy, fp->gc);
	    fp->gc = 0;
	}

	if (fp->id != 0)
	{
	    XDeleteContext (dpy, fp->id, window_fp_context);
	    XDestroyWindow (dpy, fp->id);
	    fp->id = 0;
	}

	fp->win = 0;
	next = fp->next;
	fp->next = 0;
    }
    w->frame_parts = 0;
}

/* Handle the expose event EV for the frame part FP. */
void
frame_part_exposer (XExposeEvent *ev, struct frame_part *fp)
{
    if (ev->count == 0)
    {
	/* expose events override the drawing mutex,
	   unless the server is grabbed.. */
	bool old_mutex = frame_draw_mutex;
	frame_draw_mutex = (Fserver_grabbed_p () != Qnil);
	fp->drawn.bg = rep_NULL;
	refresh_frame_part (fp);
	frame_draw_mutex = old_mutex;
    }
}

static repv
fp_assq (struct frame_part *fp, repv prop,
	 repv class_alist, repv ov_class_alist)
{
    repv tem;

    tem = Fassq (prop, fp->local_alist);
    if (tem && tem != Qnil)
	return tem;

    tem = Fassq (prop, ov_class_alist);
    if (tem && tem != Qnil)
	return tem;

    tem = Fassq (prop, fp->alist);
    if (tem && tem != Qnil)
	return tem;

    tem = Fassq (prop, class_alist);
    if (tem && tem != Qnil)
	return tem;
    
    return Qnil;
}

static repv
x_fp_assq (struct frame_part *fp, repv prop)
{
    repv ret = Qnil, class, tem;

    ret = Fassq (prop, fp->local_alist);
    if (ret && ret != Qnil)
	return ret;

    class = Fassq (Qclass, fp->alist);
    if (class && class != Qnil)
	class = rep_CDR(class);
    else
	class = rep_NULL;

    tem = global_symbol_value (Qoverride_frame_part_classes);
    if (class != rep_NULL && rep_CONSP(tem))
    {
	tem = Fassq (class, tem);
	if (tem && tem != Qnil)
	    ret = Fassq (prop, rep_CDR(tem));
    }

    if (ret && ret == Qnil)
	ret = Fassq (prop, fp->alist);

    if (ret && ret == Qnil)
    {
	tem = global_symbol_value (Qframe_part_classes);
	if (class != rep_NULL && rep_CONSP(tem))
	{
	    tem = Fassq (class, tem);
	    if (tem && tem != Qnil)
		ret = Fassq (prop, rep_CDR(tem));
	}
    }

    return ret ? ret : Qnil;
}

static repv
get_integer_prop (struct frame_part *fp, repv prop, repv class, repv ov_class)
{
    repv tem = fp_assq (fp, prop, class, ov_class);
    if (tem && tem != Qnil)
    {
	if (rep_INTP(rep_CDR(tem)))
	    tem = rep_CDR(tem);
	else
	    tem = call_protectedly_1 (rep_CDR(tem), rep_VAL(fp->win), Qnil);
	return rep_INTP(tem) ? tem : Qnil;
    }
    else
	return Qnil;
}

static repv
get_boolean_prop (struct frame_part *fp, repv prop, repv class, repv ov_class)
{
    repv tem = fp_assq (fp, prop, class, ov_class);
    if (tem && tem != Qnil)
    {
	if (Ffunctionp (rep_CDR(tem)) == Qnil)
	    tem = rep_CDR(tem);
	else
	    tem = call_protectedly_1 (rep_CDR(tem), rep_VAL(fp->win), Qnil);
	return tem;
    }
    else
	return Qnil;
}

static bool
get_pattern_prop (struct frame_part *fp, repv *data, repv (*conv)(repv data),
		  repv prop, repv class, repv ov_class)
{
    int i;
    repv tem;

    tem = fp_assq (fp, prop, class, ov_class);
    if (tem != Qnil)
    {
	if (Ffunctionp (rep_CDR(tem)) != Qnil)
	    tem = call_protectedly_1 (rep_CDR(tem), rep_VAL(fp->win), Qnil);
	else
	    tem = rep_CDR(tem);
	if (!rep_CONSP(tem))
	{
	    /* single value pattern */
	    data[0] = tem;
	    for (i = 1; i < fps_MAX; i++)
		data[i] = data[0];
	}
	else if (!rep_CONSP(rep_CAR(tem)))
	{
	    /* list of four elements: (NORMAL FOCUSED HIGHLIGHTED CLICKED) */

	    static int map[4] = {
		fps_inactive, fps_focused, fps_highlighted, fps_clicked
	    };

	    for (i = 0; i < 4; i++)
	    {
		data[map[i]] = rep_CAR(tem);
		if (rep_CONSP(rep_CDR(tem)))
		    tem = rep_CDR(tem);
	    }
	}
	else
	{
	    /* alist of elements (STATE . VALUE) */

	    while (rep_CONSP(tem) && rep_CONSP(rep_CAR(tem)))
	    {
		repv state = rep_CAR(rep_CAR(tem));
		int idx = fps_none;

		if (state == Qinactive)
		    idx = fps_inactive;
		else if (state == Qfocused)
		    idx = fps_focused;
		else if (state == Qhighlighted)
		    idx = fps_highlighted;
		else if (state == Qclicked)
		    idx = fps_clicked;
		else if (state == Qinactive_highlighted)
		    idx = fps_inactive_highlighted;
		else if (state == Qinactive_clicked)
		    idx = fps_inactive_clicked;
		if (idx != fps_none)
		    data[idx] = rep_CDR(rep_CAR(tem));

		tem = rep_CDR(tem);
	    }
	}

	/* now handle string conversions */
	for (i = 0; i < fps_MAX; i++)
	{
	    if (rep_STRINGP(data[i]))
		data[i] = call_protectedly (conv, data[i], Qnil);
	}

	/* now fill any gaps in the state table */
	for (i = 0; i < fps_MAX; i++)
	{
	    static int map[fps_MAX] = {
		fps_inactive, fps_inactive, fps_focused,
		fps_inactive, fps_highlighted, fps_inactive_highlighted
	    };

	    if (data[i] == Qnil)
		data[i] = data[map[i]];
	}
    }
    return TRUE;
}

static repv
get_color (repv name)
{
    return Fget_color (name, Qnil);
}

static bool
build_frame_part (struct frame_part *fp)
{
    Lisp_Window *w = fp->win;
    repv class = Qnil, class_elt = Qnil, ov_class_elt = Qnil, tem;
    rep_GC_root gc_class, gc_class_elt, gc_ov_class_elt;
    bool had_left_edge = FALSE, had_top_edge = FALSE;
    bool had_right_edge = FALSE, had_bottom_edge = FALSE;
    bool ret = FALSE;
    int i;

    rep_PUSHGC(gc_class, class);
    rep_PUSHGC(gc_class_elt, class_elt);
    rep_PUSHGC(gc_ov_class_elt, ov_class_elt);

    fp->width = fp->height = -1;
    for (i = 0; i < fps_MAX; i++)
	fp->fg[i] = fp->bg[i] = fp->font[i] = Qnil;

    /* find the class of the part, and the alists of class-local state */
    tem = Fassq (Qclass, fp->alist);
    if (tem && tem != Qnil)
    {
	class = rep_CDR(tem);
	tem = global_symbol_value (Qframe_part_classes);
	if (rep_CONSP(tem))
	{
	    tem = Fassq (class, tem);
	    if (tem && tem != Qnil)
		class_elt = rep_CDR(tem);
	}
	tem = global_symbol_value (Qoverride_frame_part_classes);
	if (rep_CONSP(tem))
	{
	    tem = Fassq (class, tem);
	    if (tem && tem != Qnil)
		ov_class_elt = rep_CDR(tem);
	}
    }

    /* do we ignore this part? */
    tem = get_boolean_prop (fp, Qhidden, class_elt, ov_class_elt);
    if (tem != Qnil)
	goto next_part;
    tem = Fassq (Qremovable, fp->alist);
    if (tem && tem != Qnil && rep_CDR(tem) != Qnil)
    {
       tem = Fwindow_get (rep_VAL(w), Qremoved_classes, Qnil);	/* XXX hoist */
	if (tem && rep_CONSP(tem))
	{
	    tem = Fmemq (class, tem);
	    if (tem && tem != Qnil)
		goto next_part;
	}
    }

    tem = get_boolean_prop (fp, Qbelow_client, class_elt, ov_class_elt);
    fp->below_client = (tem && tem != Qnil);
    tem = get_boolean_prop (fp, Qscale_foreground, class_elt, ov_class_elt);
    fp->scale_foreground = (tem && tem != Qnil);

    /* get text label */
    tem = fp_assq (fp, Qtext, class_elt, ov_class_elt);
    if (tem != Qnil)
	fp->text = rep_CDR(tem);
    else
	fp->text = Qnil;
    tem = fp_assq (fp, Qx_justify, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	tem = rep_CDR(tem);
	if (Ffunctionp (tem) != Qnil)
	    tem = call_protectedly_1 (tem, rep_VAL(w), Qnil);
	fp->x_justify = tem;
    }
    else
	fp->x_justify = Qnil;
    tem = fp_assq (fp, Qy_justify, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	tem = rep_CDR(tem);
	if (Ffunctionp (tem) != Qnil)
	    tem = call_protectedly_1 (tem, rep_VAL(w), Qnil);
	fp->y_justify = tem;
    }
    else
	fp->y_justify = Qnil;

    /* get cursor */
    fp->cursor = Qnil;
    tem = fp_assq (fp, Qcursor, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	tem = rep_CDR(tem);
	if (Ffunctionp (tem) != Qnil)
	    tem = call_protectedly_1 (tem, rep_VAL (w), Qnil);
	if (!CURSORP(tem) && tem != Qnil)
	    tem = call_protectedly (Fget_cursor, tem, Qnil);
	if (CURSORP(tem))
	    fp->cursor = tem;
    }

    /* get renderer function */
    tem = fp_assq (fp, Qrenderer, class_elt, ov_class_elt);
    if (tem != Qnil && Ffunctionp (rep_CDR(tem)) != Qnil)
    {
	fp->renderer = rep_CDR(tem);
	tem = get_integer_prop (fp, Qrender_scale, class_elt, ov_class_elt);
	if (tem != Qnil && rep_INT(tem) > 0)
	    fp->render_scale = rep_INT(tem);
	else
	    fp->render_scale = 1;
    }
    else
	fp->renderer = Qnil;

    /* get border width */
    tem = get_integer_prop (fp, Qborder_width, class_elt, ov_class_elt);
    if (tem != Qnil) {
	w->border_width = rep_INT(tem);
    }

    /* get border color */
    tem = fp_assq (fp, Qborder_color, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	tem = rep_CDR(tem);
	if (Ffunctionp (tem) != Qnil)
	    tem = call_protectedly_1 (tem, rep_VAL (w), Qnil);
	if (!COLORP(tem) && tem != Qnil)
	    tem = call_protectedly (get_color, tem, Qnil);
	if (COLORP(tem)) {
	    w->border_pixel = VCOLOR(tem)->pixel;
	}
    }

    /* get background images or colors */
    if (!get_pattern_prop (fp, fp->bg, get_color,
			   Qbackground, class_elt, ov_class_elt))
    {
	goto next_part;
    }

    /* get foreground colors or images */
    if (!get_pattern_prop (fp, fp->fg, get_color,
			   Qforeground, class_elt, ov_class_elt))
    {
	goto next_part;
    }

    /* get fonts */
    if (!get_pattern_prop (fp, fp->font, Fget_font,
			   Qfont, class_elt, ov_class_elt))
    {
	goto next_part;
    }

    /* If we have a background image for this part, take it as
       the provisional dimensions of the part */
    for (i = 0; i < fps_MAX; i++)
    {
	if (IMAGEP(fp->bg[i]))
	{
	    fp->width = image_width (VIMAGE(fp->bg[i]));
	    fp->height = image_height (VIMAGE(fp->bg[i]));
	    break;
	}
    }

    /* get dimensions.. */
    tem = get_integer_prop (fp, Qwidth, class_elt, ov_class_elt);
    if (tem != Qnil)
	fp->width = rep_INT(tem);
    tem = get_integer_prop (fp, Qheight, class_elt, ov_class_elt);
    if (tem != Qnil)
	fp->height = rep_INT(tem);
    tem = get_integer_prop (fp, Qleft_edge, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	fp->x = rep_INT(tem);
	had_left_edge = TRUE;
    }
    tem = get_integer_prop (fp, Qtop_edge, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	fp->y = rep_INT(tem);
	had_top_edge = TRUE;
    }
    tem = get_integer_prop (fp, Qright_edge, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	had_right_edge = TRUE;
	if (had_left_edge)
	    fp->width = w->attr.width - rep_INT(tem) - fp->x;
	else
	    fp->x = w->attr.width - rep_INT(tem) - fp->width;
    }
    tem = get_integer_prop (fp, Qbottom_edge, class_elt, ov_class_elt);
    if (tem != Qnil)
    {
	had_bottom_edge = TRUE;
	if (had_top_edge)
	    fp->height = w->attr.height - rep_INT(tem) - fp->y;
	else
	    fp->y = w->attr.height - rep_INT(tem) - fp->height;
    }

    if (fp->width < 0)
	fp->width = 0;
    if (fp->height < 0)
	fp->height = 0;

    /* try to remove edges sticking out of small windows. if a part
       specified by only one edge sticks out of the other edge, then
       truncate it */

    if (had_right_edge && !had_left_edge && fp->x < 0)
    {
	fp->width += fp->x;
	fp->x = 0;
    }
    else if (had_left_edge && !had_right_edge
	     && fp->x + fp->width > w->attr.width)
    {
	fp->width = w->attr.width - fp->x;
    }

    if (had_bottom_edge && !had_top_edge && fp->y < 0)
    {
	fp->height += fp->y;
	fp->y = 0;
    }
    else if (had_top_edge && !had_bottom_edge
	     && fp->y + fp->height > w->attr.height)
    {
	fp->height = w->attr.height - fp->y;
    }

    /* if we have a renderer function, create the image to
       render into. */
    if (fp->renderer != Qnil)
    {
	fp->rendered_image
	     = Fmake_sized_image (rep_MAKE_INT(fp->width / fp->render_scale),
				  rep_MAKE_INT(fp->height / fp->render_scale),
				  Qnil);
	fp->rendered_state = fps_none;
    }
    else
	fp->rendered_image = Qnil;

    DB(("  part: x=%d y=%d width=%d height=%d\n",
	fp->x, fp->y, fp->width, fp->height));

    ret = TRUE;

next_part:
    rep_POPGC; rep_POPGC; rep_POPGC;
    return ret;
}

static void
configure_frame_part (struct frame_part *fp)
{
    Lisp_Window *w = fp->win;
    XSetWindowAttributes wa;
    unsigned long wamask;
    if (fp->id == 0)
    {
	if (fp->width > 0 && fp->height > 0)
	{
	    XGCValues gcv;
	    wa.win_gravity = StaticGravity;
	    wa.bit_gravity = StaticGravity;
	    wa.colormap = image_cmap;
	    wa.border_pixel = BlackPixel (dpy, screen_num);
	    wamask = CWWinGravity | CWBitGravity | CWColormap | CWBorderPixel;
	    fp->id = XCreateWindow (dpy, w->frame,
				    fp->x - w->frame_x, fp->y - w->frame_y,
				    fp->width, fp->height,
				    0, image_depth, InputOutput,
				    image_visual, wamask, &wa);
	    gcv.graphics_exposures = False;
	    fp->gc = XCreateGC (dpy, fp->id, GCGraphicsExposures, &gcv);
	    XSelectInput (dpy, fp->id, FP_EVENTS);

	    if (!fp->below_client)
		XMapRaised (dpy, fp->id);
	    else
	    {
		XMapWindow (dpy, fp->id);
		XLowerWindow (dpy, fp->id);
	    }

	    /* stash the fp in the window */
	    XSaveContext (dpy, fp->id, window_fp_context, (XPointer)fp);

	    fp->drawn.fg = rep_NULL;
	    fp->drawn.bg = rep_NULL;
	}
    }
    else
    {
	if (fp->width > 0 && fp->height > 0)
	{
	    XWindowChanges attr;
	    attr.x = fp->x - w->frame_x;
	    attr.y = fp->y - w->frame_y;
	    attr.width = fp->width;
	    attr.height = fp->height;
	    attr.stack_mode = fp->below_client ? Below : Above;
	    XConfigureWindow (dpy, fp->id, CWX | CWY | CWWidth
			      | CWHeight | CWStackMode, &attr);
	    /* Generate an Expose event for the window. */
	    XClearArea (dpy, fp->id, 0, 0, 0, 0, True);
	}
	else
	{
	    XDestroyWindow (dpy, fp->id);
	    fp->id = 0;
	    XFreeGC (dpy, fp->gc);
	    fp->gc = 0;
	}
    }
    if (fp->id != 0)
    {
	XDefineCursor (dpy, fp->id, (fp->cursor != Qnil)
		       ? VCURSOR(fp->cursor)->cursor : None);
    }
}

/* Generate a frame-part frame for window W. If called for a window that
   already has a frame, it will be rebuilt to the current window size. */
static void
list_frame_generator (Lisp_Window *w)
{
    repv gen_list = w->frame_style;
    repv ptr = rep_NULL, tem;
    struct frame_part **last_fp = 0;
    struct frame_part *fp = 0;
    rep_GC_root gc_win, gc_ptr;
    repv win = rep_VAL(w);
    bool regen;				/* are we resizing the frame */
    bool bigger;
    int nparts = 0;
    XSetWindowAttributes wa;
    unsigned long wamask;
    int old_x_off, old_y_off;

    /* bounding box of frame */
    int left_x, top_y, right_x, bottom_y;

    tem = Fwindow_get (rep_VAL(w), Qhide_client, Qnil);
    if (tem && tem != Qnil)
	w->client_hidden = 1;
    else
	w->client_hidden = 0;

    left_x = top_y = 0;
    if (!w->client_hidden)
    {
	right_x = w->attr.width;
	bottom_y = w->attr.height;
    }
    else
	right_x = bottom_y = 0;

    DB(("list_frame_generator(%s)\n", rep_STR(w->name)));

    while (gen_list != Qnil && rep_SYMBOLP(gen_list) && !rep_INTERRUPTP)
    {
	gen_list = Fsymbol_value (gen_list, Qt);
	rep_TEST_INT;
    }

    rep_PUSHGC(gc_win, win);

    /* clear window border */
    w->border_width = 0;
    w->border_pixel = BlackPixel (dpy, screen_num);

    /* construct the component list, and find the bounding box */

    /* if w->destroy_frame is set then we're rebuilding an existing
       frame */
    if (w->destroy_frame == 0)
    {
	ptr = gen_list;
	last_fp = &w->frame_parts;
	assert (w->frame_parts == 0);
	regen = FALSE;
    }
    else
    {
	fp = w->frame_parts;
	regen = TRUE;
    }

    /* This loop is a bit weird. If we're building the frame from scratch
       we loop over the Lisp list of frame part specs. Otherwise we loop
       over the _actual_ list of frame parts */
    rep_PUSHGC(gc_ptr, ptr);
    while ((!regen && rep_CONSP(ptr))
	   || (regen && fp != 0))
    {
	rep_GC_root gc_fp;
	repv fp_;
	if (!regen)
	    fp = fp_new (w, rep_CAR (ptr));
	fp_ = rep_VAL(fp);

	rep_PUSHGC(gc_fp, fp_);
	if (build_frame_part (fp))
	{
	    /* expand frame bounding box */
	    left_x = MIN(left_x, fp->x);
	    right_x = MAX(right_x, fp->x + fp->width);
	    top_y = MIN(top_y, fp->y);
	    bottom_y = MAX(bottom_y, fp->y + fp->height);

	    if (!regen)
	    {
		/* link in fp */
		*last_fp = fp;
		last_fp = &fp->next;
	    }

	    nparts++;
	}
	rep_POPGC;			/* fp */

	if (!regen)
	    ptr = rep_CDR(ptr);
	else
	    fp = fp->next;
    }
    rep_POPGC;				/* ptr */

    bigger = (right_x - left_x > w->frame_width
	      || bottom_y - top_y > w->frame_height);

    old_x_off = w->frame_x;
    old_y_off = w->frame_y;

    /* now we can find the size and offset of the frame. */
    w->frame_width = right_x - left_x;
    w->frame_height = bottom_y - top_y;
    w->frame_x = left_x;
    w->frame_y = top_y;

    DB(("  bounding box: x=%d y=%d width=%d height=%d\n",
	left_x, top_y, w->frame_width, w->frame_height));

    if (w->reparented && bigger)
	set_frame_shapes (w, TRUE);

    /* create the child-of-root frame window, or if it already exists,
       configure it to the correct size.. */

    if (w->frame == 0)
    {
	int depth = image_depth;
	Visual *visual = image_visual;
	Colormap colormap = image_cmap;

	/* If window is using an ARGB visual, the frame also should. */
	if (w->attr.depth == 32)
	{
	    depth = 32;
	    visual = w->attr.visual;
	    colormap = w->attr.colormap;
	}

	wa.override_redirect = True;
	wa.colormap = colormap;
	wa.border_pixel = w->border_pixel;
	wa.save_under = w->attr.save_under;
	wamask = CWOverrideRedirect | CWColormap | CWBorderPixel | CWSaveUnder;

	w->frame = XCreateWindow (dpy, root_window, w->attr.x, w->attr.y,
				  w->frame_width, w->frame_height, w->border_width,
				  depth, InputOutput, visual, wamask, &wa);
    }
    else
    {
	/* adjust frame position to keep absolute client position constant */
	w->attr.x += w->frame_x - old_x_off;
	w->attr.y += w->frame_y - old_y_off;

	XSetWindowBorder (dpy, w->frame, w->border_pixel);
	XSetWindowBorderWidth (dpy, w->frame, w->border_width);
	XMoveResizeWindow (dpy, w->frame, w->attr.x, w->attr.y,
			   w->frame_width, w->frame_height);

	if (w->reparented)
	    XMoveResizeWindow (dpy, w->id, -left_x, -top_y,
			       w->attr.width, w->attr.height);
	else
	    XResizeWindow (dpy, w->id, w->attr.width, w->attr.height);
    }

    w->destroy_frame = frame_part_destroyer;
    w->focus_change = refresh_frame_parts;
    w->rebuild_frame = list_frame_generator;
    w->property_change = refresh_frame_parts;

    if (w->reparented)
	XLowerWindow (dpy, w->id);

    /* create/update windows for each part */
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
	configure_frame_part (fp);

    if (!w->reparented || !bigger)
	set_frame_shapes (w, TRUE);

    /* ICCCM says we must unmap the client window when it's hidden */
    {
	int unmap_client = (!w->visible || w->client_hidden);
	if (w->client_unmapped != unmap_client)
	{
	    before_local_map (w);
	    if (unmap_client)
		XUnmapWindow (dpy, w->id);
	    else
		XMapWindow (dpy, w->id);
	    w->client_unmapped = unmap_client;
	    after_local_map (w);
	    if (focus_window == w)
		focus_on_window (w);
	}
    }

    rep_POPGC;				/* win */
}

/* Return the keymap associated with this frame part, or nil */
repv
get_keymap_for_frame_part (struct frame_part *fp)
{
    repv tem = x_fp_assq (fp, Qkeymap);
    if (tem != Qnil)
	tem = rep_CDR(tem);
    return tem;
}

/* Mark all frame-parts of window W for gc. */
void
mark_frame_parts (Lisp_Window *w)
{
    struct frame_part *fp;
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
	rep_MARKVAL (rep_VAL(fp));
}

/* Reset state of all frame parts in window W. */
void
reset_frame_parts (Lisp_Window *w)
{
    struct frame_part *fp;
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
    {
	int old_state = current_state (fp), new_state;
	if (fp->clicked)
	    unclick_current_fp ();
	fp->highlighted = 0;
	new_state = current_state (fp);
	if (new_state != old_state)
	    refresh_frame_part (fp);
    }
}

/* Make sure stacking in frame is as desired after reparenting
   the client into the frame */
void
restack_frame_parts (Lisp_Window *w)
{
    struct frame_part *fp;
    XLowerWindow (dpy, w->id);
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
    {
	if (fp->id != 0 && fp->below_client)
	    XLowerWindow (dpy, fp->id);
    }
}

/* creating window frames */

/* Create a frame for window W. Called with the server grabbed. If
   w->frame is non-zero, then we'll use this window to construct the
   frame in, otherwise w->frame will be initialised with a new window */
void
create_window_frame (Lisp_Window *w)
{
    DB(("create_window_frame (%s)\n", rep_STR(w->name)));
    if (w->frame_parts == 0)
    {
	w->destroy_frame = 0;
	w->focus_change = 0;
	w->rebuild_frame = 0;
	w->property_change = 0;
	list_frame_generator (w);
    }
    else
	fprintf (stderr, "warning: reframing framed window: %lx\n", (long) w->id);
}

/* Destroy the frame of window W. If LEAVE-FRAME-WIN is non-zero, then
   w->frame won't be destroyed */
void
destroy_window_frame (Lisp_Window *w, bool leave_frame_win)
{
    if (w->frame != 0)
    {
	if (w->destroy_frame != 0)
	{
	    w->destroy_frame (w);
	    w->destroy_frame = 0;
	}
	if (!leave_frame_win && w->frame != 0)
	{
	    XDestroyWindow (dpy, w->frame);
	    w->frame = 0;
	}
    }
}

/* Lisp functions */

DEFUN("frame-draw-mutex", Fframe_draw_mutex,
      Sframe_draw_mutex, (repv arg), rep_Subr1) /*
::doc:sawfish.wm.frames.subrs#frame-draw-mutex::
While this variable is non-nil no frame parts will be redrawn. When it is
set to nil any pending redraws will take place.
::end:: */
{
    repv ret = frame_draw_mutex ? Qt : Qnil;
    frame_draw_mutex = (arg != Qnil);
    if (!frame_draw_mutex)
    {
	Lisp_Window *w;
	for (w = window_list; w != 0; w = w->next)
	{
	    struct frame_part *fp;
	    for (fp = w->frame_parts; fp != 0; fp = fp->next)
	    {
		if (fp->pending_refresh)
		    refresh_frame_part (fp);
	    }
	}
    }
    return ret;
}
	
DEFUN("frame-state-mutex", Fframe_state_mutex,
      Sframe_state_mutex, (repv arg), rep_Subr1) /*
::doc:sawfish.wm.frames.subrs#frame-state-mutex::
While this variable is non-nil the state of frame parts will not be
altered when the pointer enters or leaves its window.
::end:: */
{
    repv ret = frame_state_mutex ? Qt : Qnil;
    frame_state_mutex = (arg != Qnil);
    if (arg == Qclicked
	&& clicked_frame_part != 0
	&& !clicked_frame_part->clicked)
    {
	/* XXX hack alert */
	clicked_frame_part->clicked = 1;
	refresh_frame_part (clicked_frame_part);
    }
    return ret;
}

DEFUN("frame-part-get", Fframe_part_get,
      Sframe_part_get, (repv part, repv prop), rep_Subr2) /*
::doc:sawfish.wm.frames.subrs#frame-part-get::
frame-part-get PART PROPERTY
::end:: */
{
    repv tem;
    rep_DECLARE1 (part, PARTP);
    rep_DECLARE2 (prop, rep_SYMBOLP);
    tem = x_fp_assq (VPART(part), prop);
    return (tem != Qnil) ? rep_CDR(tem) : Qnil;
}

DEFUN("frame-part-put", Fframe_part_put, Sframe_part_put,
      (repv part, repv prop, repv value), rep_Subr3)
{
    repv tem;
    rep_DECLARE1(part, PARTP);
    rep_DECLARE2(prop, rep_SYMBOLP);
    tem = Fassq (prop, VPART(part)->local_alist);
    if (tem != rep_NULL)
    {
	if (tem != Qnil)
	    rep_CDR (tem) = value;
	else
	    VPART(part)->local_alist = Fcons (Fcons (prop, value),
					      VPART(part)->local_alist);
    }
    return value;
}

DEFUN("frame-part-window", Fframe_part_window,
      Sframe_part_window, (repv part), rep_Subr1)
{ 
    rep_DECLARE1(part, PARTP);
    return rep_VAL(VPART(part)->win);
}

DEFUN("frame-part-x-window", Fframe_part_x_window,
      Sframe_part_x_window, (repv part), rep_Subr1)
{ 
    rep_DECLARE1(part, PARTP);
    return VPART(part)->id != 0 ? rep_MAKE_INT(VPART(part)->id) : Qnil;
}

DEFUN("frame-part-position", Fframe_part_position,
      Sframe_part_position, (repv part), rep_Subr1)
{ 
    rep_DECLARE1(part, PARTP);
    return Fcons (rep_MAKE_INT(VPART(part)->x - VPART(part)->win->frame_x),
		  rep_MAKE_INT(VPART(part)->y - VPART(part)->win->frame_y));
}

DEFUN("frame-part-dimensions", Fframe_part_dimensions,
      Sframe_part_dimensions, (repv part), rep_Subr1)
{ 
    rep_DECLARE1(part, PARTP);
    return Fcons (rep_MAKE_INT(VPART(part)->width),
		  rep_MAKE_INT(VPART(part)->height));
}

DEFUN("frame-part-state", Fframe_part_state,
      Sframe_part_state, (repv part), rep_Subr1)
{ 
    rep_DECLARE1(part, PARTP);
    return state_syms[current_state (VPART(part))];
}

DEFUN("map-frame-parts", Fmap_frame_parts,
      Smap_frame_parts, (repv fun, repv win), rep_Subr2)
{
    rep_GC_root gc_win;
    struct frame_part *fp;

    rep_DECLARE (1, fun, Ffunctionp (fun) != Qnil);
    rep_DECLARE2 (win, WINDOWP);

    rep_PUSHGC (gc_win, win);
    fp = VWIN(win)->frame_parts;
    while (fp != 0)
    {
	repv tem = rep_call_lisp1 (fun, rep_VAL(fp));	/* fun,fp protected */
	if (tem == rep_NULL)
	    break;
	fp = fp->next;
    }
    rep_POPGC;
    return Qnil;
}

DEFUN("refresh-frame-part", Frefresh_frame_part,
      Srefresh_frame_part, (repv part), rep_Subr1)
{
    rep_DECLARE1(part, PARTP);
    if (VPART(part)->id != 0)
	refresh_frame_part (VPART(part));
    return Qt;
}

DEFUN("rebuild-frame-part", Frebuild_frame_part,
      Srebuild_frame_part, (repv part), rep_Subr1)
{
    rep_GC_root gc_part;
    rep_DECLARE1(part, PARTP);
    rep_PUSHGC(gc_part, part);
    if (build_frame_part (VPART(part)))
    {
	/* XXX what about reconfiguring the container window..? */
	configure_frame_part (VPART(part));
	queue_reshape_frame (VPART(part)->win);
    }
    rep_POPGC;
    return Qt;
}

DEFUN("refresh-window", Frefresh_window,
      Srefresh_window, (repv win), rep_Subr1)
{
    rep_DECLARE1(win, XWINDOWP);
    if (!WINDOW_IS_GONE_P (VWIN(win)))
	refresh_frame_parts (VWIN(win));
    return Qt;
}

/* initialisation */

void
frames_init (void)
{
    repv tem;
    frame_part_type = rep_register_new_type ("frame-part", fp_cmp, fp_prin,
					     fp_prin, fp_sweep, fp_mark, 0,
					     0, 0, 0, 0, 0, 0);

    tem = rep_push_structure ("sawfish.wm.frames.subrs");
    rep_ADD_SUBR(Sframe_draw_mutex);
    rep_ADD_SUBR(Sframe_state_mutex);
    rep_ADD_SUBR(Sframe_part_get);
    rep_ADD_SUBR(Sframe_part_put);
    rep_ADD_SUBR(Sframe_part_window);
    rep_ADD_SUBR(Sframe_part_x_window);
    rep_ADD_SUBR(Sframe_part_position);
    rep_ADD_SUBR(Sframe_part_dimensions);
    rep_ADD_SUBR(Sframe_part_state);
    rep_ADD_SUBR(Smap_frame_parts);
    rep_ADD_SUBR(Srefresh_frame_part);
    rep_ADD_SUBR(Srebuild_frame_part);
    rep_pop_structure (tem);

    tem = rep_push_structure ("sawfish.wm.windows.subrs");
    rep_ADD_SUBR(Srefresh_window);
    rep_pop_structure (tem);

    rep_INTERN(internal);
    rep_INTERN(tiled);
    rep_INTERN(center);
    rep_INTERN(right);
    rep_INTERN(left);
    rep_INTERN(top);
    rep_INTERN(bottom);
    rep_INTERN(text);
    rep_INTERN(x_justify);
    rep_INTERN(y_justify);
    rep_INTERN(background);
    rep_INTERN(foreground);
    rep_INTERN(renderer);
    rep_INTERN(render_scale);
    rep_INTERN(font);
    rep_INTERN(width);
    rep_INTERN(height);
    rep_INTERN(left_edge);
    rep_INTERN(top_edge);
    rep_INTERN(right_edge);
    rep_INTERN(bottom_edge);
    rep_INTERN(cursor);
    rep_INTERN(focused);
    rep_INTERN(highlighted);
    rep_INTERN(clicked);
    rep_INTERN(inactive);
    rep_INTERN(inactive_highlighted);
    rep_INTERN(inactive_clicked);
    rep_INTERN(hide_client);
    rep_INTERN(class);
    rep_INTERN(removable);
    rep_INTERN(removed_classes);
    rep_INTERN(below_client);
    rep_INTERN(scale_foreground);
    rep_INTERN(hidden);
    rep_INTERN(border_width);
    rep_INTERN(border_color);

    rep_INTERN_SPECIAL(frame_part_classes);
    rep_INTERN_SPECIAL(override_frame_part_classes);

    state_syms[fps_inactive] = Qnil;
    state_syms[fps_focused] = Qfocused;
    state_syms[fps_highlighted] = Qhighlighted;
    state_syms[fps_clicked] = Qclicked;
    state_syms[fps_inactive_highlighted] = Qinactive_highlighted;
    state_syms[fps_inactive_clicked] = Qinactive_clicked;

    if (!batch_mode_p ())
	window_fp_context = XUniqueContext ();
}

void
frames_kill (void)
{
}
