/* frames.c -- window frame manipulation
   $Id$

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

#include "sawmill.h"
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/extensions/shape.h>
#include <alloca.h>

static XID window_fp_context;

DEFSYM(default_frame, "default-frame");
DEFSYM(nil_frame, "nil-frame");
DEFSYM(internal, "internal");
DEFSYM(tiled, "tiled");
DEFSYM(unshaped, "unshaped");
DEFSYM(center, "center");
DEFSYM(right, "right");
DEFSYM(left, "left");
DEFSYM(top, "right");
DEFSYM(bottom, "left");
DEFSYM(text, "text");
DEFSYM(x_justify, "x-justify");
DEFSYM(y_justify, "y-justify");
DEFSYM(background, "background");
DEFSYM(foreground, "foreground");
DEFSYM(font, "font");
DEFSYM(width, "width");
DEFSYM(height, "height");
DEFSYM(left_edge, "left-edge");
DEFSYM(top_edge, "top-edge");
DEFSYM(right_edge, "right-edge");
DEFSYM(bottom_edge, "bottom-edge");
DEFSYM(cursor, "cursor");

static bool frame_draw_mutex;


/* building frames from component lists

   build the frame from the list of components. Each element
   in the list is an alist representing one component of the
   frame, possible tags include:

	background . IMAGE-OR-COLOR
	background . (NORMAL FOCUSED HIGHLIGHTED CLICKED)
	foreground . COLOR
	foreground . (NORMAL FOCUSED HIGHLIGHTED CLICKED)

	text . STRING-OR-FUNCTION-OR-NIL
	x-justify . left OR right OR center OR NUMBER
	y-justify . top OR bottom OR center OR NUMBER

	font . FONT
	font . (NORMAL FOCUSED HIGHLIGHTED CLICKED)

	left-edge . POSITION-REL-LEFT
	right-edge . POSITION-REL-RIGHT
	top-edge . POSITION-REL-TOP
	bottom-edge . POSITION-REL-BOTTOM
	height . PIXELS
	width . PIXELS

	keymap . KEYMAP
	cursor . CURSOR-OR-CURSOR-DEF

   Note that all numeric quantities may be defined dynamically by
   substituting a function */

static inline int
current_state (struct frame_part *fp)
{
    return (fp->clicked ? fps_clicked
	    : fp->highlighted ? fps_highlighted
	    : (fp->win == focus_window) ? fps_focused
	    : fps_normal);
}

/* Set the background of the frame-part FP. This is either a solid color
   or an image (scaled or tiled) */
void
set_frame_part_bg (struct frame_part *fp)
{
    int state = current_state (fp);
    repv bg = fp->bg[state];
    Lisp_Window *win = fp->win;

    if (fp->id == 0)
	return;

    if (COLORP(bg))
    {
	XSetWindowBackground (dpy, fp->id, VCOLOR(bg)->color.pixel);
    }
    else if (IMAGEP(bg))
    {
	Lisp_Image *image = VIMAGE(bg);
	Pixmap bg_pixmap, bg_mask;
	bool tiled = FALSE, shaped = TRUE;
	repv tem;

	tem = Fimage_get (rep_VAL(image), Qtiled);
	if (tem && tem != Qnil)
	    tiled = TRUE;
	if (rep_SYMBOLP(win->frame_style))
	{
	    tem = Fget (win->frame_style, Qunshaped);
	    if (tem && tem != Qnil)
		shaped = FALSE;
	}

	if (tiled)
	{
	    Imlib_render (imlib_id, image->image,
			  image->image->rgb_width,
			  image->image->rgb_height);
	}
	else
	{
	    Imlib_render (imlib_id, image->image, fp->width, fp->height);
	}

	bg_pixmap = Imlib_move_image (imlib_id, image->image);
	bg_mask = Imlib_move_mask (imlib_id, image->image);

	/* Some of the Imlib_ functions call XSync on our display. In turn
	   this can cause the error handler to run if a window has been
	   deleted. This then invalidates the window we're updating */
	if (win->id == 0)
	    return;

	if (bg_pixmap)
	{
	    XSetWindowBackgroundPixmap (dpy, fp->id, bg_pixmap);
	    if (shaped)
	    {
		if (bg_mask)
		{
		    XRectangle rect;
		    rect.x = fp->x - win->frame_x;
		    rect.y = fp->y - win->frame_y;
		    rect.width = fp->width;
		    rect.height = fp->height;
		    XShapeCombineRectangles (dpy, win->frame,
					     ShapeBounding, 0, 0,
					     &rect, 1, ShapeSubtract,
					     Unsorted);

		    XShapeCombineMask (dpy, win->frame, ShapeBounding,
				       fp->x - win->frame_x,
				       fp->y - win->frame_y,
				       bg_mask, ShapeUnion);
		    if (tiled)
		    {
			/* The pixmap will be tiled automatically. But
			   we still need to tile the shape-mask manually */
			int x = image->image->rgb_width;
			int y = 0;
			do {
			    do {
				XShapeCombineMask (dpy, win->frame,
						   ShapeBounding,
						   fp->x - win->frame_x + x,
						   fp->y - win->frame_y + y,
						   bg_mask, ShapeUnion);
				x += image->image->rgb_width;
			    } while (x < fp->width);
			    x = 0;
			    y += image->image->rgb_height;
			} while (y < fp->height);
		    }
		    /* freeing the pixmap below also frees the mask */
		}
		else
		{
		    XRectangle rect;
		    rect.x = fp->x - win->frame_x;
		    rect.y = fp->y - win->frame_y;
		    rect.width = fp->width;
		    rect.height = fp->height;
		    XShapeCombineRectangles (dpy, win->frame,
					     ShapeBounding, 0, 0,
					     &rect, 1, ShapeUnion, Unsorted);
		}
	    }
	    Imlib_free_pixmap (imlib_id, bg_pixmap);
	}
    }
    else
    {
	/* No background. Set it to white. */
	XSetWindowBackground (dpy, fp->id, WhitePixel (dpy, screen_num));
    }

    /* background won't be updated until the window is cleared.. */
}

/* Draw the foreground pixels in frame-part FP. */
void
set_frame_part_fg (struct frame_part *fp)
{
    int state = current_state (fp);
    repv font = fp->font[state], fg = fp->fg[state];

    if (fp->id == 0)
	return;

    XClearWindow (dpy, fp->id);

    if (fp->text == Qnil)
	return;

    if (!COLORP(fg))
	fg = Fsymbol_value (Qdefault_foreground, Qt);
    if (!FONTP(font))
	font = Fsymbol_value (Qdefault_font, Qt);
    if (COLORP(fg) && FONTP(font))
    {
	u_char *string = 0;
	int length, width, height, x, y;
	XGCValues gcv;

	if (rep_STRINGP(fp->text))
	{
	    string = rep_STR(fp->text);
	    length = rep_STRING_LEN(fp->text);
	}
	else if (fp->text == Qnil)
	    return;
	else
	{
	    repv result = rep_call_lisp1 (fp->text, rep_VAL(fp->win));
	    if (!result || !rep_STRINGP(result))
		return;
	    string = rep_STR(result);
	    length = rep_STRING_LEN(result);
	}

	width = XTextWidth (VFONT(font)->font, string, length);
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

	height = VFONT(font)->font->ascent + VFONT(font)->font->descent;
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

	if (fp->gc == 0)
	{
	    gcv.font = VFONT(font)->font->fid;
	    gcv.foreground = VCOLOR(fg)->color.pixel;
	    gcv.function = GXcopy;
	    fp->gc = XCreateGC (dpy, fp->id,
				GCFunction | GCForeground | GCFont, &gcv);
	}
	else
	{
	    gcv.font = VFONT(font)->font->fid;
	    gcv.foreground = VCOLOR(fg)->color.pixel;
	    XChangeGC (dpy, fp->gc, GCForeground | GCFont, &gcv);
	}
	XDrawString (dpy, fp->id, fp->gc, x,
		     y + VFONT(font)->font->ascent, string, length);
    }
}

/* Redraw FP. */
void
refresh_frame_part (struct frame_part *fp)
{
    if (!frame_draw_mutex)
    {
	Lisp_Window *w = fp->win;
	set_frame_part_bg (fp);
	if (w->id != 0)
	    set_frame_part_fg (fp);
    }
    else
	fp->pending_refresh = 1;
}

/* Find the frame-part that is drawn in window ID */
struct frame_part *
find_frame_part_by_window (Window id)
{
    struct frame_part *fp;
    return XFindContext (dpy, id, window_fp_context, (XPointer *)&fp) ? 0 : fp;
}

/* Destroy the window frame of W, assuming it's a frame-part derived frame */
static void
frame_part_destroyer (Lisp_Window *w)
{
    struct frame_part *fp, *next;
    for (fp = w->frame_parts; fp != 0; fp = next)
    {
	if (fp->gc)
	    XFreeGC (dpy, fp->gc);

	if (fp->id != 0)
	{
	    XDeleteContext (dpy, fp->id, window_fp_context);
	    XDestroyWindow (dpy, fp->id);
	}

	next = fp->next;
	rep_free (fp);
    }
    w->frame_parts = 0;
    XDestroyWindow (dpy, w->frame);
}

/* Called when the focus state of window W has changed, assuming a
   frame-part derived frame */
static void
frame_part_focuser (Lisp_Window *w)
{
    struct frame_part *fp;
    for (fp = w->frame_parts; fp != 0 && w->id != 0; fp = fp->next)
    {
	refresh_frame_part (fp);

	/* set_frame_part_bg may trigger the error handler */
	if (w->id == 0)
	    break;
    }
}

/* Handle the expose event EV for the frame part FP. */
void
frame_part_exposer (XExposeEvent *ev, struct frame_part *fp)
{
    if (ev->count == 0)
	set_frame_part_fg (fp);
}

/* Called when a window property changes */
static void
frame_part_prop_change (Lisp_Window *w)
{
    struct frame_part *fp;
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
	set_frame_part_fg (fp);
}

static repv
get_integer_prop (Lisp_Window *w, repv prop, repv elt)
{
    repv tem = Fassq (prop, elt);
    if (tem && tem != Qnil)
    {
	if (rep_INTP(rep_CDR(tem)))
	    tem = rep_CDR(tem);
	else
	    tem = rep_call_lisp1 (rep_CDR(tem), rep_VAL(w));
	return (tem && rep_INTP(tem)) ? tem : Qnil;
    }
    else
	return Qnil;
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
    XSetWindowAttributes wa;
    u_long wamask;
    int i;
    rep_GC_root gc_win;
    repv win = rep_VAL(w);
    bool regen;				/* are we resizing the frame */
    int nparts = 0;

    /* bounding box of frame */
    int left_x = 0, top_y = 0;
    int right_x = w->attr.width, bottom_y = w->attr.height;

    DB(("list_frame_generator(%s)\n", w->name));

    while (gen_list != Qnil && rep_SYMBOLP(gen_list) && !rep_INTERRUPTP)
    {
	gen_list = Fsymbol_value (gen_list, Qt);
	rep_TEST_INT;
    }

    rep_PUSHGC(gc_win, win);

    /* construct the component list, and find the bounding box */
    if (w->frame == 0)
    {
	ptr = gen_list;
	last_fp = &w->frame_parts;
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
    while ((!regen && rep_CONSP(ptr))
	   || (regen && fp != 0))
    {
	repv elt;
	bool had_left_edge = FALSE, had_top_edge = FALSE;

	if (!regen)
	{
	    fp = rep_alloc (sizeof (struct frame_part));
	    memset (fp, 0, sizeof (struct frame_part));
	    fp->win = w;
	    fp->alist = rep_CAR(ptr);
	}
	elt = fp->alist;

	fp->width = fp->height = -1;
	for (i = 0; i < fps_MAX; i++)
	    fp->fg[i] = fp->bg[i] = fp->font[i] = Qnil;

	/* get text label */
	tem = Fassq (Qtext, elt);
	if (tem && tem != Qnil)
	    fp->text = rep_CDR(tem);
	else
	    fp->text = Qnil;
	tem = Fassq (Qx_justify, elt);
	if (tem && tem != Qnil)
	    fp->x_justify = rep_CDR(tem);
	else
	    fp->x_justify = Qnil;
	tem = Fassq (Qy_justify, elt);
	if (tem && tem != Qnil)
	    fp->y_justify = rep_CDR(tem);
	else
	    fp->y_justify = Qnil;

	/* get cursor */
	fp->cursor = Qnil;
	tem = Fassq (Qcursor, elt);
	if (tem && tem != Qnil)
	{
	    if (rep_SYMBOLP(rep_CDR(tem)))
		tem = Fget_cursor (rep_CDR(tem));
	    else
		tem = rep_CDR(tem);
	    if (tem && CURSORP(tem))
		fp->cursor = tem;
	}

	/* get background images or colors */
	tem = Fassq (Qbackground, elt);
	if (tem && tem != Qnil)
	{
	    if (Ffunctionp (rep_CDR(tem)) != Qnil)
		tem = rep_call_lisp1 (rep_CDR(tem), rep_VAL(w));
	    else
		tem = rep_CDR(tem);
	    if (!tem)
		goto next_part;
	    if (IMAGEP(tem) || COLORP(tem) || rep_STRINGP(tem))
	    {
		fp->bg[0] = tem;
		for (i = 1; i < fps_MAX; i++)
		    fp->bg[i] = fp->bg[0];
	    }
	    else if (rep_CONSP(tem))
	    {
		for (i = 0; i < fps_MAX; i++)
		{
		    fp->bg[i] = ((IMAGEP(rep_CAR(tem))
				  || COLORP(rep_CAR(tem))
				  || rep_STRINGP(rep_CAR(tem)))
				 ? rep_CAR(tem) : fp->bg[i-1]);
		    if (rep_CONSP(rep_CDR(tem)))
			tem = rep_CDR(tem);
		}
	    }
	}

	/* get foreground colors */
	tem = Fassq (Qforeground, elt);
	if (tem && tem != Qnil)
	{
	    if (Ffunctionp (rep_CDR(tem)) != Qnil)
		tem = rep_call_lisp1 (rep_CDR(tem), rep_VAL(w));
	    else
		tem = rep_CDR(tem);
	    if (!tem)
		goto next_part;
	    if (COLORP(tem) || rep_STRINGP(tem))
	    {
		fp->fg[0] = tem;
		for (i = 1; i < fps_MAX; i++)
		    fp->fg[i] = fp->fg[0];
	    }
	    else if (rep_CONSP(tem))
	    {
		for (i = 0; i < fps_MAX; i++)
		{
		    fp->fg[i] = ((COLORP(rep_CAR(tem))
				  || rep_STRINGP(rep_CAR(tem)))
				 ? rep_CAR(tem) : fp->fg[i-1]);
		    if (rep_CONSP(rep_CDR(tem)))
			tem = rep_CDR(tem);
		}
	    }
	}

	/* get fonts */
	tem = Fassq (Qfont, elt);
	if (tem && tem != Qnil)
	{
	    if (Ffunctionp (rep_CDR(tem)) != Qnil)
		tem = rep_call_lisp1 (rep_CDR(tem), rep_VAL(w));
	    else
		tem = rep_CDR(tem);
	    if (!tem)
		goto next_part;
	    if (FONTP(tem) || rep_STRINGP(tem))
	    {
		fp->font[0] = tem;
		for (i = 1; i < fps_MAX; i++)
		    fp->font[i] = fp->font[0];
	    }
	    else
	    {
		for (i = 0; i < fps_MAX; i++)
		{
		    fp->font[i] = ((FONTP(rep_CAR(tem))
				    || rep_STRINGP(rep_CAR(tem)))
				   ? rep_CAR(tem) : fp->font[i-1]);
		    if (rep_CONSP(rep_CDR(tem)))
			tem = rep_CDR(tem);
		}
	    }
	}

	/* resolve string bg/fg/font attributes to the actual objects */
	for (i = 0; i < fps_MAX; i++)
	{
	    if (rep_STRINGP(fp->fg[i]))
		fp->fg[i] = Fget_color (fp->fg[i]);
	    if (fp->fg[i] != Qnil && !COLORP(fp->fg[i]))
		goto next_part;

	    if (rep_STRINGP(fp->bg[i]))
		fp->bg[i] = Fget_color (fp->bg[i]);
	    if (fp->bg[i] != Qnil && !IMAGEP(fp->bg[i]) && !COLORP(fp->bg[i]))
		goto next_part;

	    if (rep_STRINGP(fp->font[i]))
		fp->font[i] = Fget_font (fp->font[i]);
	    if (fp->font[i] != Qnil && !FONTP(fp->font[i]))
		goto next_part;
	}

	/* If we have a background image for this part, take it as
	   the provisional dimensions of the part */
	for (i = 0; i < fps_MAX; i++)
	{
	    if (IMAGEP(fp->bg[i]))
	    {
		fp->width = VIMAGE(fp->bg[fps_normal])->image->rgb_width;
		fp->height = VIMAGE(fp->bg[fps_normal])->image->rgb_height;
		break;
	    }
	}

	/* get dimensions.. */
	tem = get_integer_prop (w, Qwidth, elt);
	if (tem != Qnil)
	    fp->width = rep_INT(tem);
	tem = get_integer_prop (w, Qheight, elt);
	if (tem != Qnil)
	    fp->height = rep_INT(tem);
	tem = get_integer_prop (w, Qleft_edge, elt);
	if (tem != Qnil)
	{
	    fp->x = rep_INT(tem);
	    had_left_edge = TRUE;
	}
	tem = get_integer_prop (w, Qtop_edge, elt);
	if (tem != Qnil)
	{
	    fp->y = rep_INT(tem);
	    had_top_edge = TRUE;
	}
	tem = get_integer_prop (w, Qright_edge, elt);
	if (tem != Qnil)
	{
	    if (had_left_edge)
		fp->width = w->attr.width - rep_INT(tem) - fp->x;
	    else
		fp->x = w->attr.width - rep_INT(tem) - fp->width;
	}
	tem = get_integer_prop (w, Qbottom_edge, elt);
	if (tem != Qnil)
	{
	    if (had_top_edge)
		fp->height = w->attr.height - rep_INT(tem) - fp->y;
	    else
		fp->y = w->attr.height - rep_INT(tem) - fp->height;
	}
	if (fp->width < 0)
	    fp->width = right_x - fp->x;
	if (fp->height < 0)
	    fp->height = bottom_y - fp->y;

	DB(("  part: x=%d y=%d width=%d height=%d\n",
	    fp->x, fp->y, fp->width, fp->height));

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

    next_part:
	if (!regen)
	    ptr = rep_CDR(ptr);
	else
	    fp = fp->next;
    }

    /* now we can find the size and offset of the frame. */
    w->frame_width = right_x - left_x;
    w->frame_height = bottom_y - top_y;
    w->frame_x = left_x;
    w->frame_y = top_y;

    DB(("  bounding box: x=%d y=%d width=%d height=%d\n",
	w->frame_x, w->frame_y, w->frame_width, w->frame_height));

    if (!regen)
    {
	/* create the frame */
	wa.background_pixel = WhitePixel (dpy, screen_num);
	wamask = CWBackPixel;
	w->frame = XCreateWindow (dpy, root_window, w->attr.x, w->attr.y,
				  w->frame_width, w->frame_height,
				  0, screen_depth, InputOutput,
				  screen_visual, wamask, &wa);
    }
    else
    {
	XResizeWindow (dpy, w->frame, w->frame_width, w->frame_height);
	if (w->reparented)
	    XMoveWindow (dpy, w->id, -w->frame_x, -w->frame_y);
    }

    w->destroy_frame = frame_part_destroyer;
    w->focus_change = frame_part_focuser;
    w->rebuild_frame = list_frame_generator;
    w->property_change = frame_part_prop_change;

    /* if shaped, make the initial frame shape */
    if (rep_SYMBOLP(w->frame_style))
	tem = Fget (w->frame_style, Qunshaped);
    else
	tem = Qnil;
    if (tem == Qnil || w->shaped)
    {
	XRectangle *rects = alloca (sizeof (XRectangle) * (nparts + 1));
	int i;
	for (i = 0, fp = w->frame_parts; i < nparts; i++, fp = fp->next)
	{
	    rects[i].x = fp->x - w->frame_x;
	    rects[i].y = fp->y - w->frame_y;
	    rects[i].width = fp->width;
	    rects[i].height = fp->height;
	}
	if (!w->shaped)
	{
	    rects[i].x = -w->frame_x;
	    rects[i].y = -w->frame_y;
	    rects[i].width = w->attr.width;
	    rects[i].height = w->attr.height;
	    i++;
	}
	XShapeCombineRectangles (dpy, w->frame, ShapeBounding, 0, 0,
				 rects, i, ShapeSet, Unsorted);
	if (w->shaped)
	{
	    XShapeCombineShape (dpy, w->frame, ShapeBounding,
				-w->frame_x, -w->frame_y, w->id,
				ShapeBounding, ShapeUnion);
	}
    }

    /* create/update windows for each part */
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
    {
	if (fp->id == 0)
	{
	    if (fp->width > 0 && fp->height > 0)
	    {
		wamask = 0;
		fp->id = XCreateWindow (dpy, w->frame,
					fp->x - w->frame_x, fp->y - w->frame_y,
					fp->width, fp->height,
					0, screen_depth, InputOutput,
					screen_visual, wamask, &wa);
		XSelectInput (dpy, fp->id,
			      ButtonPressMask | ButtonReleaseMask
			      | ButtonMotionMask | PointerMotionHintMask
			      | EnterWindowMask | LeaveWindowMask
			      | KeyPressMask | ExposureMask);
		XMapWindow (dpy, fp->id);

		/* stash the fp in the window */
		XSaveContext (dpy, fp->id, window_fp_context, (XPointer)fp);
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
		XConfigureWindow (dpy, fp->id,
				  CWX | CWY | CWWidth | CWHeight, &attr);
	    }
	    else
	    {
		XDestroyWindow (dpy, fp->id);
		fp->id = 0;
	    }
	}
	if (fp->id != 0)
	{
	    XDefineCursor (dpy, fp->id, (fp->cursor != Qnil)
			   ? VCURSOR(fp->cursor)->cursor : None);
	    set_frame_part_bg (fp);
	}
    }
    rep_POPGC;
}

/* Mark all frame-parts of window W for gc. */
void
mark_frame_parts (Lisp_Window *w)
{
    struct frame_part *fp;
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
    {
	int i;
	rep_MARKVAL(fp->alist);
	rep_MARKVAL(rep_VAL(fp->win));
	for (i = 0; i < fps_MAX; i++)
	{
	    rep_MARKVAL(fp->font[i]);
	    rep_MARKVAL(fp->fg[i]);
	    rep_MARKVAL(fp->bg[i]);
	}
	rep_MARKVAL(rep_VAL(fp->cursor));
    }
}

/* Reset state of all frame parts in window W. */
void
reset_frame_parts (Lisp_Window *w)
{
    struct frame_part *fp;
    for (fp = w->frame_parts; fp != 0; fp = fp->next)
    {
	if (fp->clicked)
	    unclick_current_fp ();
	fp->highlighted = 0;
    }
}


/* creating window frames */

/* Create a frame for window W. Called with the server grabbed */
void
create_window_frame (Lisp_Window *w)
{
    DB(("create_window_frame (%s)\n", w->name));
    if (w->frame != 0)
	destroy_window_frame (w);
    w->destroy_frame = 0;
    w->focus_change = 0;
    w->rebuild_frame = 0;
    w->property_change = 0;
    list_frame_generator (w);
}

/* Destroy the frame of window W */
void
destroy_window_frame (Lisp_Window *w)
{
    if (w->frame != 0)
    {
	if (w->destroy_frame != 0)
	    w->destroy_frame (w);
	else if (w->frame != 0)
	    XDestroyWindow (dpy, w->frame);
    }
    w->frame = 0;
}



DEFUN("frame-draw-mutex", Vframe_draw_mutex,
      Sframe_draw_mutex, (repv arg), rep_Var)
{
    if (arg != 0)
    {
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
		    {
			refresh_frame_part (fp);
			fp->pending_refresh = 0;
		    }
		}
	    }
	}
    }
    return frame_draw_mutex ? Qt : Qnil;
}
	

/* initialisation */

void
frames_init (void)
{
    rep_INTERN(default_frame);
    rep_SYM(Qdefault_frame)->value = Qnil;

    rep_INTERN(nil_frame);
    rep_SYM(Qnil_frame)->value = Qnil;

    rep_ADD_SUBR(Sframe_draw_mutex);

    rep_INTERN(internal);
    rep_INTERN(tiled);
    rep_INTERN(unshaped);
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
    rep_INTERN(font);
    rep_INTERN(width);
    rep_INTERN(height);
    rep_INTERN(left_edge);
    rep_INTERN(top_edge);
    rep_INTERN(right_edge);
    rep_INTERN(bottom_edge);
    rep_INTERN(cursor);

    if (rep_SYM(Qbatch_mode)->value == Qnil)
	window_fp_context = XUniqueContext ();
}

void
frames_kill (void)
{
}
