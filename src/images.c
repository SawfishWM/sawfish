/* images.c -- Image handling
   $Id$

   Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>

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

#include "sawmill.h"
#include <assert.h>

static Lisp_Image *image_list;
int image_type;

#if defined HAVE_IMLIB
static ImlibData *imlib_id;
#elif defined HAVE_GDK_PIXBUF
GdkInterpType interp_type = GDK_INTERP_BILINEAR;
#endif

Colormap image_cmap;
Visual *image_visual;
int image_depth;

DEFSYM(image_load_path, "image-load-path");
DEFSYM(image_directory, "image-directory");

DEFSYM(red, "red");
DEFSYM(green, "green");
DEFSYM(blue, "blue");

DEFSYM(default_bevel_percent, "default-bevel-percent");

/* A rough estimate of the memory used by a single image_t */
#if defined HAVE_IMLIB
#define IMAGE_T_SIZE(i) (sizeof (ImlibImage) \
			 + (i)->rgb_width * (i)->rgb_height * 3)
#elif defined HAVE_GDK_PIXBUF
#define IMAGE_T_SIZE(i) (gdk_pixbuf_get_width (i)	\
			 * gdk_pixbuf_get_height (i)	\
			 * gdk_pixbuf_get_n_channels (i))
#endif

static image_t
load_image (char *file)
{
#if defined HAVE_IMLIB
    /* Fuck. Imlib's image caching is so annoying. This is the only way
       I can think of disabling it, even though it may fragment memory.. */
    ImlibImage *im_1 = Imlib_load_image (imlib_id, file);
    if (im_1 != 0)
    {
	ImlibImage *im_2 = Imlib_clone_image (imlib_id, im_1);
	Imlib_kill_image (imlib_id, im_1);
	return im_2;
    }
    else
	return 0;
#elif defined HAVE_GDK_PIXBUF
    return gdk_pixbuf_new_from_file (file, NULL);
#endif
}

/* Make a Lisp image object from the imlib image IM. Its initial properties
   will be taken from the list PLIST. */
static repv
make_image (image_t im, repv plist)
{
    Lisp_Image *f;
    for (f = image_list; f != 0; f = f->next)
    {
	if (f->image == im)
	    /* XXX do what with the plist..? */
	    return rep_VAL(f);
    }
    f = rep_ALLOC_CELL(sizeof(Lisp_Image));
    rep_data_after_gc += sizeof (Lisp_Image) + IMAGE_T_SIZE (im);
    f->car = image_type;
    f->next = image_list;
    image_list = f;
    f->image = im;
#if !defined (HAVE_IMLIB)
    f->border[0] = f->border[1] = f->border[2] = f->border[3] = 0;
#endif
#if defined NEED_PIXMAP_CACHE
    f->pixmap_first = f->pixmap_last = 0;
#endif
    f->plist = plist;
    return rep_VAL(f);
}

static repv
find_image_file (repv file, bool *deletep)
{
    repv path = global_symbol_value (Qimage_load_path), out = Qnil;
    rep_GC_root gc_file, gc_path;
    rep_PUSHGC(gc_file, file);
    rep_PUSHGC(gc_path, path);

    while (out == Qnil && rep_CONSP(path))
    {
	repv all = Fexpand_file_name (file, rep_CAR(path));
	if (all && rep_STRINGP(all))
	{
	    repv tem;
	    rep_GC_root gc_all;
	    rep_PUSHGC(gc_all, all);
	    tem = Ffile_regular_p (all);
	    if (tem && tem != Qnil)
	    {
		bool delete = FALSE;
		file = Flocal_file_name (all);
		if (file == Qnil)
		{
		    /* Non-local file; try to copy to temp file */
		    repv temp = Fmake_temp_name ();
		    rep_GC_root gc_temp;
		    rep_PUSHGC(gc_temp, temp);
		    tem = Fcopy_file (all, temp);
		    rep_POPGC;
		    if (tem && tem != Qnil)
		    {
			file = temp;
			delete = TRUE;
		    }
		}
		if (file && rep_STRINGP(file))
		{
		    *deletep = delete;
		    out = file;
		}
	    }
	    rep_POPGC;
	}
	path = rep_CDR(path);
    }
    rep_POPGC; rep_POPGC;
    return out;
}

Pixmap
make_bitmap (repv file, int *widthp, int *heightp, int *x_hotp, int *y_hotp)
{
    bool delete;
    file = find_image_file (file, &delete);
    if (file && rep_STRINGP(file))
    {
	Pixmap bitmap;
	int ret = XReadBitmapFile (dpy, root_window, rep_STR(file),
				   widthp, heightp, &bitmap, x_hotp, y_hotp);

	if (delete)
	    Fdelete_file (file);

	return (ret == BitmapSuccess) ? bitmap : 0;
    }
    else
	return 0;
}

DEFUN("make-image", Fmake_image, Smake_image,
      (repv file, repv plist), rep_Subr2) /*
::doc:sawfish.wm.images#make-image::
make-image FILE [PLIST]

Return a new image object representing the image stored in FILE (a
string). PLIST defines the property list of the image.
::end:: */
{
    bool delete;
    rep_GC_root gc_plist;
    rep_DECLARE1(file, rep_STRINGP);
    if (dpy == 0)
	return Qnil;
    rep_PUSHGC(gc_plist, plist);
    file = find_image_file (file, &delete);
    rep_POPGC;
    if (file && rep_STRINGP(file))
    {
	image_t im = load_image (rep_STR(file));
	if (delete)
	    Fdelete_file (file);
	if (im != 0)
	    return make_image (im, plist);
    }
    return Fsignal (Qerror, rep_list_2(rep_string_dup("no such image"), file));
}

DEFUN("make-image-from-x-drawable", Fmake_image_from_x_drawable,
      Smake_image_from_x_drawable, (repv id, repv mask_id), rep_Subr2) /*
::doc:sawfish.wm.images#make-image-from-x-drawable::
make-image-from-x-drawable ID [MASK-ID]
::end:: */
{
   Drawable d;
   Pixmap mask = 0;
   Window root;
   int x, y, w, h, bdr, dp;
   image_t im;

   rep_DECLARE1 (id, rep_INTP);
   d = rep_INT (id);
   if (rep_INTP (mask_id))
       mask = rep_INT (mask_id);

   if (!XGetGeometry (dpy, d, &root, &x, &y, &w, &h, &bdr, &dp))
       return Qnil;

#if defined HAVE_IMLIB
   /* Imlib ignores the MASK parameter of this function.. */
   im = Imlib_create_image_from_drawable (imlib_id, d, 0, 0, 0, w, h);
   if (mask != 0)
   {
       /* ..so do it by hand */
       XImage *xim = XGetImage (dpy, mask, 0, 0, w, h, AllPlanes, ZPixmap);
       if (xim != 0)
       {
	   u_char *pixels = im->rgb_data;
	   int x, y;
	   ImlibColor shape;

	   /* locate a suitable shape color (one that is unused by
	      the actual image data). */
	   shape.r = shape.g = shape.b = 1;	/* most images include black */
       try_again:
	   for (y = 0; y < h; y++)
	   {
	       for (x = 0; x < w; x++)
	       {
		   u_long pixel = XGetPixel (xim, x, y);
		   u_char *rgb = pixels + ((y * w) + x) * 3;
		   if (pixel != 0 && rgb[0] == shape.r
		       && rgb[1] == shape.g && rgb[2] == shape.b)
		   {
		       /* this color no good. */
		       if (++shape.r == 256)
		       {
			   shape.r = 0;
			   if (++shape.g == 256)
			   {
			       shape.g = 0;
			       if (++shape.b == 256)
				   goto out;	/* no possible shape color */
			   }
		       }
		       goto try_again;
		   }
	       }
	   }
	   /* SHAPE now has a color unused by the image data */
	   Imlib_set_image_shape (imlib_id, im, &shape);
	   for (y = 0; y < h; y++)
	   {
	       for (x = 0; x < w; x++)
	       {
		   if (XGetPixel (xim, x, y) == 0)
		   {
		       u_char *rgb = pixels + ((y * w) + x) * 3;
		       rgb[0] = shape.r;
		       rgb[1] = shape.g;
		       rgb[2] = shape.b;
		   }
	       }
	   }
	   Imlib_changed_image (imlib_id, im);
       out:
	   XDestroyImage (xim);
       }
   }
#elif defined HAVE_GDK_PIXBUF
   if (dp == 1)
   {
       /* gdk_pixbuf_xlib_get_from_drawable () doesn't seem to work
          with single-bitplane drawables. Doing it by hand.. */
       XImage *xim = XGetImage (dpy, d, 0, 0, w, h, 1, ZPixmap);
       im = gdk_pixbuf_new (GDK_COLORSPACE_RGB, FALSE, 8, w, h);
       if (xim != 0)
       {
	   int rowstride = gdk_pixbuf_get_rowstride (im);
	   u_char *pixels = gdk_pixbuf_get_pixels (im);
	   int x, y;
	   for (y = 0; y < h; y++)
	   {
	       for (x = 0; x < w; x++)
	       {
		   int idx = y * rowstride + x * 3;
		   u_char chan = (XGetPixel (xim, x, y) != 0) ? 0 : 255;
		   pixels[idx+0] = chan;
		   pixels[idx+1] = chan;
		   pixels[idx+2] = chan;
	       }
	   }
	   XDestroyImage (xim);
       }
   }
   else
   {
       im = gdk_pixbuf_xlib_get_from_drawable (0, d, image_cmap, image_visual,
					       0, 0, 0, 0, w, h);
   }

   if (mask != 0)
   {
       /* this code inspired by the GNOME tasklist_applet */
       GdkPixbuf *tem = gdk_pixbuf_add_alpha (im, FALSE, 0, 0, 0);
       if (tem != 0)
       {
	   XImage *xim = XGetImage (dpy, mask, 0, 0, w, h, AllPlanes, ZPixmap);
	   gdk_pixbuf_unref (im);
	   im = tem;
	   if (xim != 0)
	   {
	       int rowstride = gdk_pixbuf_get_rowstride (im);
	       int channels = gdk_pixbuf_get_n_channels (im);
	       unsigned char *pixels = gdk_pixbuf_get_pixels (im);
	       int x, y;

	       for (y = 0; y < h; y++)
	       {
		   for (x = 0; x < w; x++)
		   {
		       int idx = y * rowstride + x * channels + 3;
		       pixels[idx] = (XGetPixel (xim, x, y) == 0) ? 0 : 255;
		   }
	       }
	       XDestroyImage (xim);
	   }
       }
   }
#endif

   /* server grab may be lost in the above calls */
   regrab_server ();

   return im ? make_image (im, Qnil) : Qnil;
}

DEFUN("copy-image", Fcopy_image, Scopy_image, (repv source), rep_Subr1) /*
::doc:sawfish.wm.images#copy-image::
copy-image SOURCE-IMAGE

Return a new image object, a clone of SOURCE-IMAGE.
::end:: */
{
    image_t im;
    rep_DECLARE1(source, IMAGEP);
#if defined HAVE_IMLIB
    im = Imlib_clone_image (imlib_id, VIMAGE(source)->image);
#elif defined HAVE_GDK_PIXBUF
    im = gdk_pixbuf_copy (VIMAGE (source)->image);
#endif
    if (im != 0)
	return make_image (im, Fcopy_sequence (VIMAGE(source)->plist));
    return Fsignal (Qerror,
		    rep_list_2 (rep_string_dup("can't clone image"), source));
}

DEFUN("flip-image-horizontally", Fflip_image_horizontally,
      Sflip_image_horizontally, (repv image), rep_Subr1) /*
::doc:sawfish.wm.images#flip-image-horizontally::
flip-image-horizontally IMAGE 

Flip the contents of IMAGE around the vertical axis.
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
#if defined HAVE_IMLIB
    Imlib_flip_image_horizontal (imlib_id, VIMAGE(image)->image);
#else
    {
	u_char *pixels = image_pixels (VIMAGE (image));
	int channels = image_channels (VIMAGE (image));
	int stride = image_row_stride (VIMAGE (image));
	int width = image_width (VIMAGE (image));
	int height = image_height (VIMAGE (image));
	u_char *buf = alloca (channels);
	int y;
	for (y = 0; y < height; y++)
	{
	    u_char *left = pixels + y * stride;
	    u_char *right = left + channels * (width - 1);
	    while (left < right)
	    {
		memcpy (buf, left, channels);
		memcpy (left, right, channels);
		memcpy (right, buf, channels);
		left += channels;
		right -= channels;
	    }
	}
	image_changed (VIMAGE (image));
    }
#endif
    return image;
}

DEFUN("flip-image-vertically", Fflip_image_vertically,
      Sflip_image_vertically, (repv image), rep_Subr1) /*
::doc:sawfish.wm.images#flip-image-vertically::
flip-image-vertically IMAGE 

Flip the contents of IMAGE around the horizontal axis.
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
#if defined HAVE_IMLIB
    Imlib_flip_image_vertical (imlib_id, VIMAGE(image)->image);
#else
    {
	u_char *pixels = image_pixels (VIMAGE (image));
	int stride = image_row_stride (VIMAGE (image));
	int height = image_height (VIMAGE (image));
	u_char *buf = alloca (stride);
	u_char *top = pixels, *bottom = pixels + (height - 1) * stride;
	while (top < bottom)
	{
	    memcpy (buf, top, stride);
	    memcpy (top, bottom, stride);
	    memcpy (bottom, buf, stride);
	    top += stride;
	    bottom -= stride;
	}
	image_changed (VIMAGE (image));
    }
#endif
    return image;
}

DEFUN("flip-image-diagonally", Fflip_image_diagonally,
      Sflip_image_diagonally, (repv image), rep_Subr1) /*
::doc:sawfish.wm.images#flip-image-diagonally::
flip-image-diagonally IMAGE 

Flip the contents of IMAGE around a diagonal axis from the top-left to
the bottom right of the image.
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
#if defined HAVE_IMLIB
    Imlib_rotate_image (imlib_id, VIMAGE(image)->image, 1);
#elif defined HAVE_GDK_PIXBUF
    {
	/* adapted from Imlib */
	GdkPixbuf *in = VIMAGE (image)->image;
	int channels = gdk_pixbuf_get_n_channels (in);
	int in_stride = gdk_pixbuf_get_rowstride (in);
	int in_width = gdk_pixbuf_get_width (in);
	int in_height = gdk_pixbuf_get_height (in);
	u_char *in_pixels = gdk_pixbuf_get_pixels (in);
	GdkPixbuf *out = gdk_pixbuf_new (gdk_pixbuf_get_colorspace (in),
					 gdk_pixbuf_get_has_alpha (in),
					 gdk_pixbuf_get_bits_per_sample (in),
					 in_height, in_width);
	int out_stride = gdk_pixbuf_get_rowstride (out);
	u_char *out_pixels = gdk_pixbuf_get_pixels (out);
	int x, y;
	for (y = 0; y < in_height; y++)
	{
	    for (x = 0; x < in_width; x++)
	    {
		memcpy (out_pixels + x * out_stride + y * channels,
			in_pixels  + y * in_stride  + x * channels,
			channels);
	    }
	}
	image_changed (VIMAGE (image));
	gdk_pixbuf_unref (VIMAGE (image)->image);
	VIMAGE (image)->image = out;
    }
#endif
    return image;
}

DEFUN("image-get", Fimage_get, Simage_get, (repv win, repv prop), rep_Subr2) /*
::doc:sawfish.wm.images#image-get::
image-get IMAGE PROPERTY

Return the value of the property named PROPERTY (a symbol) of IMAGE.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, IMAGEP);
    plist = VIMAGE(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop
	    || (!rep_SYMBOLP(prop)
		&& rep_value_cmp (rep_CAR(plist), prop) == 0))
	{
	    return rep_CAR(rep_CDR(plist));
	}
	plist = rep_CDR(rep_CDR(plist));
    }
    return Qnil;
}

DEFUN("image-put", Fimage_put, Simage_put,
      (repv win, repv prop, repv val), rep_Subr3) /*
::doc:sawfish.wm.images#image-put::
image-put IMAGE PROPERTY VALUE

Set the value of the property named PROPERTY (a symbol) of IMAGE to VALUE.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, IMAGEP);
    plist = VIMAGE(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop
	    || (!rep_SYMBOLP(prop)
		&& rep_value_cmp (rep_CAR(plist), prop) == 0))
	{
	    rep_CAR(rep_CDR(plist)) = val;
	    return val;
	}
	plist = rep_CDR(rep_CDR(plist));
    }
    plist = Fcons(prop, Fcons(val, VIMAGE(win)->plist));
    if (plist != rep_NULL)
	VIMAGE(win)->plist = plist;
    return val;
}

DEFUN("imagep", Fimagep, Simagep, (repv arg), rep_Subr1) /*
::doc:sawfish.wm.images#imagep::
image ARG

Returns t if ARG is an image object.
::end:: */
{
    return IMAGEP(arg) ? Qt : Qnil;
}

DEFUN("image-dimensions", Fimage_dimensions, Simage_dimensions,
      (repv img), rep_Subr1) /*
::doc:sawfish.wm.images#image-dimensions::
image-dimensions IMAGE

Return (WIDTH . HEIGHT) representing the dimensions in pixels of IMAGE.
::end:: */
{
    rep_DECLARE1(img, IMAGEP);

    return Fcons (rep_MAKE_INT(image_width(VIMAGE(img))),
		  rep_MAKE_INT(image_height(VIMAGE(img))));
}

DEFUN("image-shape-color", Fimage_shape_color,
      Simage_shape_color, (repv img), rep_Subr1) /*
::doc:sawfish.wm.images#image-shape-color::
image-shape-color IMAGE
::end:: */
{
#if defined HAVE_IMLIB
    ImlibColor shape;
    rep_DECLARE1(img, IMAGEP);
    Imlib_get_image_shape (imlib_id, VIMAGE(img)->image, &shape);
    if (shape.r == -1 && shape.g == -1 && shape.b == -1)
	return Qnil;
    else
	return Fget_color_rgb (rep_MAKE_INT(shape.r * 256),
			       rep_MAKE_INT(shape.g * 256),
			       rep_MAKE_INT(shape.b * 256));
#elif defined HAVE_GDK_PIXBUF
    fprintf (stderr, "shape colors are unimplemented for gdk-pixbuf\n");
    return Qnil;
#endif
}

DEFUN("set-image-shape-color", Fset_image_shape_color, Sset_image_shape_color,
      (repv img, repv shape), rep_Subr2) /*
::doc:sawfish.wm.images#set-image-shape-color::
set-image-shape-color IMAGE TRANSPARENT-COLOR
::end:: */
{
#if defined HAVE_IMLIB
    ImlibColor color;
    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE2(shape, COLORP);
    color.r = VCOLOR(shape)->red / 256;
    color.g = VCOLOR(shape)->green / 256;
    color.b = VCOLOR(shape)->blue / 256;
    Imlib_set_image_shape (imlib_id, VIMAGE(img)->image, &color);
    return img;
#elif defined HAVE_GDK_PIXBUF
    fprintf (stderr, "shape colors are unimplemented for gdk-pixbuf\n");
    return img;
#endif
}

DEFUN("image-border", Fimage_border, Simage_border, (repv img), rep_Subr1) /*
::doc:sawfish.wm.images#image-border::
image-border IMAGE

Return (LEFT RIGHT TOP BOTTOM) representing the border (in pixels) of IMAGE.
::end:: */
{
    rep_DECLARE1(img, IMAGEP);
#if !defined (HAVE_IMLIB)
    return rep_list_4 (rep_MAKE_INT (VIMAGE(img)->border[0]),
		       rep_MAKE_INT (VIMAGE(img)->border[1]),
		       rep_MAKE_INT (VIMAGE(img)->border[2]),
		       rep_MAKE_INT (VIMAGE(img)->border[3]));
#else
    {
	ImlibBorder border;
	Imlib_get_image_border (imlib_id, VIMAGE(img)->image, &border);
	return rep_list_4 (rep_MAKE_INT(border.left),
			   rep_MAKE_INT(border.right),
			   rep_MAKE_INT(border.top),
			   rep_MAKE_INT(border.bottom));
    }
#endif
}

DEFUN("set-image-border", Fset_image_border, Sset_image_border,
      (repv img, repv left, repv right, repv top, repv bottom), rep_Subr5) /*
::doc:sawfish.wm.images#set-image-border::
set-image-border IMAGE LEFT RIGHT TOP BOTTOM

Set the border of IMAGE to (LEFT RIGHT TOP BOTTOM). The border of an
image defines how that image is scaled -- only pixels inside the border
are resized.
::end:: */
{
    rep_DECLARE1(img, IMAGEP);
#if !defined (HAVE_IMLIB)
    VIMAGE(img)->border[0] = rep_INTP (left) ? rep_INT (left) : 0;
    VIMAGE(img)->border[1] = rep_INTP (right) ? rep_INT (right) : 0;
    VIMAGE(img)->border[2] = rep_INTP (top) ? rep_INT (top) : 0;
    VIMAGE(img)->border[3] = rep_INTP (bottom) ? rep_INT (bottom) : 0;
    image_changed (VIMAGE(img));
#else
    {
	ImlibBorder border;
	Imlib_get_image_border (imlib_id, VIMAGE(img)->image, &border);
	if (rep_INTP(left))
	    border.left = rep_INT(left);
	if (rep_INTP(right))
	    border.right = rep_INT(right);
	if (rep_INTP(top))
	    border.top = rep_INT(top);
	if (rep_INTP(bottom))
	    border.bottom = rep_INT(bottom);
	Imlib_set_image_border (imlib_id, VIMAGE(img)->image, &border);
    }
#endif
    return img;
}

DEFUN("image-modifier", Fimage_modifier, Simage_modifier,
      (repv img, repv type), rep_Subr2) /*
::doc:sawfish.wm.images#image-modifier::
image-modifier IMAGE TYPE

TYPE may be one of nil, red, green, blue. returned modifier is
(GAMMA BRIGHTNESS CONTRAST). All values range from 0 to 255.
::end:: */
{
#if defined HAVE_IMLIB
    ImlibColorModifier modifier;
    void (*fun)(ImlibData *, ImlibImage *, ImlibColorModifier *);
    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE(2, type, type == Qnil || rep_SYMBOLP (type));
    fun = (type == Qred ? Imlib_get_image_red_modifier
	   : type == Qgreen ? Imlib_get_image_green_modifier
	   : type == Qblue ? Imlib_get_image_blue_modifier
	   : Imlib_get_image_modifier);
    (*fun) (imlib_id, VIMAGE(img)->image, &modifier);
    return rep_list_3 (rep_MAKE_INT(modifier.gamma),
		       rep_MAKE_INT(modifier.brightness),
		       rep_MAKE_INT(modifier.contrast));
#else
    fprintf (stderr, "image modifiers are unimplemented for gdk-pixbuf\n");
    return Qnil;
#endif
}

DEFUN("set-image-modifier", Fset_image_modifier, Sset_image_modifier,
      (repv img, repv type, repv mod), rep_Subr3) /*
::doc:sawfish.wm.images#set-image-modifier::
set-image-modifier IMAGE TYPE MODIFIER

TYPE may be one of nil, red, green, blue. MODIFIER is (GAMMA BRIGHTNESS
CONTRAST). These are integers ranging from 0 to 255.
::end:: */
{
#if defined HAVE_IMLIB
    ImlibColorModifier modifier;
    void (*fun)(ImlibData *, ImlibImage *, ImlibColorModifier *);
    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE(2, type, type == Qnil || rep_SYMBOLP (type));
    if (!rep_CONSP(mod) || !rep_CONSP(rep_CDR(mod))
	|| !rep_CONSP(rep_CDR(rep_CDR(mod))))
    {
	return rep_signal_arg_error (mod, 3);
    }
    modifier.gamma = rep_INT(rep_CAR(mod));
    modifier.brightness = rep_INT(rep_CAR(rep_CDR(mod)));
    modifier.contrast = rep_INT(rep_CAR(rep_CDR(rep_CDR(mod))));
    fun = (type == Qred ? Imlib_set_image_red_modifier
	   : type == Qgreen ? Imlib_set_image_green_modifier
	   : type == Qblue ? Imlib_set_image_blue_modifier
	   : Imlib_set_image_modifier);
    (*fun) (imlib_id, VIMAGE(img)->image, &modifier);
    return img;
#elif defined HAVE_GDK_PIXBUF
    fprintf (stderr, "image modifiers are unimplemented for gdk-pixbuf\n");
    return img;
#endif
}

static inline void
bevel_pixel (u_char *data, bool up, double bevel_fraction)
{
    double pix[3];
    pix[0] = ((double)data[0]) / 256.0;
    pix[1] = ((double)data[1]) / 256.0;
    pix[2] = ((double)data[2]) / 256.0;
    if (up)
    {
	pix[0] = pix[0] + (1.0 - pix[0]) * bevel_fraction;
	pix[1] = pix[1] + (1.0 - pix[1]) * bevel_fraction;
	pix[2] = pix[2] + (1.0 - pix[2]) * bevel_fraction;
	data[0] = pix[0] * 256.0;
	data[1] = pix[1] * 256.0;
	data[2] = pix[2] * 256.0;
    }
    else
    {
	pix[0] = pix[0] - pix[0] * bevel_fraction;
	pix[1] = pix[1] - pix[1] * bevel_fraction;
	pix[2] = pix[2] - pix[2] * bevel_fraction;
	data[0] = pix[0] * 256.0;
	data[1] = pix[1] * 256.0;
	data[2] = pix[2] * 256.0;
    }
}

static inline void
bevel_horizontally (u_char *data, int width, int height,
		    int row_stride, int channels, 
		    int border, bool top, bool up, double bevel_fraction)
{
    int rows;
    up = top ? up : !up;
    for (rows = 0; rows < border; rows++)
    {
	u_char *ptr = data;
	int x;
	if (top)
	    ptr += (rows * row_stride) + (rows + 1) * channels;
	else
	    ptr += row_stride * (height - (rows + 1)) + (rows) * channels;
	for (x = rows; x < width - (rows + 1); x++)
	{
	    bevel_pixel (ptr, up, bevel_fraction);
	    ptr += channels;
	}
    }
}

static inline void
bevel_vertically (u_char *data, int width, int height,
		  int row_stride, int channels,
		  int border, bool top, bool up, double bevel_fraction)
{
    int cols;
    up = top ? up : !up;
    for (cols = 0; cols < border; cols++)
    {
	u_char *ptr = data;
	int y;
	if (top)
	    ptr += cols * channels + (cols * row_stride);
	else
	    ptr += (width - (cols + 1)) * channels + ((cols) * row_stride);
	for (y = cols; y <= height - (cols + 1); y++)
	{
	    bevel_pixel (ptr, up, bevel_fraction);
	    ptr += row_stride;
	}
    }
}

DEFUN("bevel-image", Fbevel_image, Sbevel_image,
      (repv image, repv border, repv up, repv bevel_percent),
      rep_Subr4) /*
::doc:sawfish.wm.images#bevel-image::
bevel-image IMAGE BORDER UP [BEVEL-PERCENT]

Draw a bevelled edge outline onto IMAGE. BORDER is an integer defining
the width of the bevel. If UP is non-nil the bevel is raised.

If BEVEL-PERCENT is an integer between 0 and 100, then this is the
intensity of the bevel created.
::end:: */
{
    double bevel_fraction = 0.75;
    rep_DECLARE1(image, IMAGEP);
    rep_DECLARE2(border, rep_INTP);

    if (!rep_INTP(bevel_percent))
	bevel_percent = global_symbol_value (Qdefault_bevel_percent);
    if (rep_INTP(bevel_percent))
    {
	int bp = rep_INT(bevel_percent);
	if ((bp >= 0) && (bp <= 100))
	    bevel_fraction = (((double) bp) / 100.0);
    }

    bevel_horizontally (image_pixels (VIMAGE(image)),
			image_width (VIMAGE(image)),
			image_height (VIMAGE(image)),
			image_row_stride (VIMAGE(image)),
			image_channels (VIMAGE(image)),
			rep_INT(border), TRUE, up != Qnil, bevel_fraction);
    bevel_vertically (image_pixels (VIMAGE(image)),
		      image_width (VIMAGE(image)),
		      image_height (VIMAGE(image)),
		      image_row_stride (VIMAGE(image)),
		      image_channels (VIMAGE(image)),
		      rep_INT(border), TRUE, up != Qnil, bevel_fraction);
    bevel_horizontally (image_pixels (VIMAGE(image)),
			image_width (VIMAGE(image)),
			image_height (VIMAGE(image)),
			image_row_stride (VIMAGE(image)),
			image_channels (VIMAGE(image)),
			rep_INT(border), FALSE, up != Qnil, bevel_fraction);
    bevel_vertically (image_pixels (VIMAGE(image)),
		      image_width (VIMAGE(image)),
		      image_height (VIMAGE(image)),
		      image_row_stride (VIMAGE(image)),
		      image_channels (VIMAGE(image)),
		      rep_INT(border), FALSE, up != Qnil, bevel_fraction);

    image_changed (VIMAGE(image));
    return image;
}

DEFUN("clear-image", Fclear_image, Sclear_image,
      (repv image, repv color), rep_Subr2) /*
::doc:sawfish.wm.images#clear-image::
clear-image IMAGE [COLOR]

Set all pixels in IMAGE to COLOR (or black if COLOR is undefined).
::end:: */
{
    int width, height, stride, channels;
    int x, y, r, g, b;
    u_char *data;

    rep_DECLARE1(image, IMAGEP);
    if (COLORP(color))
    {
	r = VCOLOR(color)->red / 256;
	g = VCOLOR(color)->green / 256;
	b = VCOLOR(color)->blue / 256;
    }
    else
	r = g = b = 0;

    data = image_pixels (VIMAGE(image));
    width = image_width (VIMAGE(image));
    height = image_height (VIMAGE(image));
    stride = image_row_stride (VIMAGE(image));
    channels = image_channels (VIMAGE(image));
    for (y = 0; y < height; y++)
    {
	for (x = 0; x < width; x++)
	{
	    data[y * stride + x * channels + 0] = r;
	    data[y * stride + x * channels + 1] = g;
	    data[y * stride + x * channels + 2] = b;
	}
    }

    image_changed (VIMAGE(image));
    return image;
}

#if defined HAVE_GDK_PIXBUF
static void
free_pixbuf_data (guchar *pixels, gpointer data)
{
    rep_free (pixels);
}
#endif

DEFUN("make-sized-image", Fmake_sized_image, Smake_sized_image,
      (repv width, repv height, repv color), rep_Subr3) /*
::doc:sawfish.wm.images#make-sized-image::
make-sized-image WIDTH HEIGHT [COLOR]

Return a new image of dimensions (WIDTH, HEIGHT). The object COLOR
defines the color of its pixels.
::end:: */
{
    u_char *data;
    int r, g, b;
    rep_DECLARE1(width, rep_INTP);
    rep_DECLARE2(height, rep_INTP);
    if (COLORP(color))
    {
	r = VCOLOR(color)->red / 256;
	g = VCOLOR(color)->green / 256;
	b = VCOLOR(color)->blue / 256;
    }
    else
	r = g = b = 0;

#if defined HAVE_IMLIB
    data = rep_alloc (rep_INT(width) * rep_INT(height) * 3);
    if (data != 0)
    {
	image_t im;
	int i;
	for (i = 0; i < rep_INT(width) * rep_INT(height) * 3; i += 3)
	{
	    data[i] = r;
	    data[i+1] = g;
	    data[i+2] = b;
	}
	im = Imlib_create_image_from_data (imlib_id, data, 0,
					   rep_INT(width), rep_INT(height));
	rep_free (data);
#elif defined HAVE_GDK_PIXBUF
    data = rep_alloc (rep_INT(width) * rep_INT(height) * 4);
    if (data != 0)
    {
	image_t im;
	int i;
	for (i = 0; i < rep_INT(width) * rep_INT(height) * 4; i += 4)
	{
	    data[i] = r;
	    data[i+1] = g;
	    data[i+2] = b;
	    data[i+3] = 255;
	}
	im = gdk_pixbuf_new_from_data (data, GDK_COLORSPACE_RGB, TRUE,
				       8, rep_INT (width), rep_INT (height),
				       rep_INT (width) * 4,
				       free_pixbuf_data, data);
#endif
	if (im != 0)
	    return make_image (im, Qnil);
    }
    return Qnil;
}

DEFUN("tile-image", Ftile_image, Stile_image, (repv dst, repv src), rep_Subr2) /*
::doc:sawfish.wm.images#tile-image::
tile-image DEST-IMAGE SOURCE-IMAGE

Tile SOURCE-IMAGE into DEST-IMAGE.
::end:: */
{
    image_t src_im, dst_im;
    int src_width, src_height;
    int dst_width, dst_height;
    int x, src_y, dst_y;

    rep_DECLARE1(dst, IMAGEP);
    rep_DECLARE2(src, IMAGEP);
    src_im = VIMAGE(src)->image;
    dst_im = VIMAGE(dst)->image;
    src_width = image_width (VIMAGE (src));
    src_height = image_height (VIMAGE (src));
    dst_width = image_width (VIMAGE (dst));
    dst_height = image_height (VIMAGE (dst));

#if defined HAVE_IMLIB
    for (dst_y = src_y = 0; dst_y < dst_height; dst_y++, src_y++)
    {
	if (src_y >= src_height)
	    src_y = 0;
	for (x = 0; x < dst_width; x += src_width)
	{
	    memcpy (dst_im->rgb_data + dst_y*dst_width*3 + x*3,
		    src_im->rgb_data + src_y*src_width*3,
		    MIN (dst_width - x, src_width) * 3);
	}
    }
#elif defined HAVE_GDK_PIXBUF
    for (dst_y = 0; dst_y < dst_height; dst_y += src_height)
    {
	for (x = 0; x < dst_width; x += src_width)
	{
	    gdk_pixbuf_copy_area (VIMAGE(src)->image, 0, 0,
				  MIN (src_width, dst_width - x),
				  MIN (src_height, dst_height - dst_y),
				  VIMAGE(dst)->image, x, dst_y);
	}
    }
#endif

    image_changed (VIMAGE(dst));
    return dst;
}
    
DEFUN("scale-image", Fscale_image, Sscale_image,
      (repv img, repv w, repv h), rep_Subr3) /*
::doc:sawfish.wm.images#scale-image::
scale-image IMAGE WIDTH HEIGHT

Return a new image object, a copy of the contents of IMAGE, but scaled
to WIDTH by HEIGHT pixels.
::end:: */
{
    image_t copy;

    rep_DECLARE1 (img, IMAGEP);
    rep_DECLARE (2, w, rep_INTP (w) && rep_INT (w) > 0);
    rep_DECLARE (3, h, rep_INTP (h) && rep_INT (h) > 0);

    /* XXX don't these handle borders differently..? */
#if defined HAVE_IMLIB
    copy = Imlib_clone_scaled_image (imlib_id, VIMAGE (img)->image,
				     rep_INT (w), rep_INT (h));
#elif defined HAVE_GDK_PIXBUF
    copy = gdk_pixbuf_scale_simple (VIMAGE (img)->image,
				    rep_INT (w), rep_INT (h),
				    interp_type);
#endif

    if (copy != 0)
	return make_image (copy, Fcopy_sequence (VIMAGE (img)->plist));
    else
	return rep_mem_error ();
}

DEFUN ("composite-images", Fcomposite_images,
       Scomposite_images, (repv img1, repv img2, repv x, repv y), rep_Subr4) /*
::doc:sawfish.wm.images#composite-images::
composite-images DEST-IMAGE SRC-IMAGE [X Y]

Modify DEST-IMAGE by compositing SRC-IMAGE onto its current contents,
at position (X, Y), or (0, 0) if no position is given.
::end:: */
{
    int w1, h1, w2, h2, copy_w, copy_h;

    rep_DECLARE1 (img1, IMAGEP);
    rep_DECLARE2 (img2, IMAGEP);

    if (x == Qnil)
	x = rep_MAKE_INT (0);
    else
	rep_DECLARE (3, x, rep_INTP (x) && rep_INT (x) >= 0);

    if (y == Qnil)
	y = rep_MAKE_INT (0);
    else
	rep_DECLARE (4, y, rep_INTP (y) && rep_INT (y) >= 0);

    w1 = image_width (VIMAGE (img1));
    h1 = image_height (VIMAGE (img1));
    w2 = image_width (VIMAGE (img2));
    h2 = image_height (VIMAGE (img2));

    copy_w = MIN (w2, w1 - rep_INT (x));
    copy_h = MIN (h2, h1 - rep_INT (y));
    
#if defined HAVE_IMLIB
    {
	u_char *img1_rgb = image_pixels (VIMAGE (img1));
	u_char *img2_rgb = image_pixels (VIMAGE (img2));
	int row, col;
	ImlibColor shape;
	Imlib_get_image_shape (imlib_id, VIMAGE (img2)->image, &shape);
	for (row = 0; row < copy_h; row++)
	{
	    for (col = 0; col < copy_w; col++)
	    {
		u_char *img2_pixel = img2_rgb + (row * w2 + col) * 3;
		if (img2_pixel[0] != shape.r
		    || img2_pixel[1] != shape.g
		    || img2_pixel[2] != shape.b)
		{
		    u_char *img1_pixel;
		    img1_pixel = img1_rgb + (((rep_INT (y) + row) * w1)
					     + (rep_INT (x) + col)) * 3;
		    /* constant size, so should be inlined */
		    memcpy (img1_pixel, img2_pixel, 3);
		}
	    }
	}
    }
#elif defined HAVE_GDK_PIXBUF
    gdk_pixbuf_composite (VIMAGE (img2)->image, VIMAGE (img1)->image,
			  rep_INT (x), rep_INT (y),
			  copy_w, copy_h, rep_INT (x), rep_INT (y), 1.0, 1.0,
			  interp_type, 255);
#endif

    image_changed (VIMAGE (img1));
    return img1;
}

DEFUN ("crop-image", Fcrop_image, Scrop_image,
       (repv img, repv x, repv y, repv w, repv h), rep_Subr5) /*
::doc:sawfish.wm.images#crop-image::
crop-image IMAGE X Y WIDTH HEIGHT

Return a new image, with dimensions WIDTH by HEIGHT. A copy of the
contents of IMAGE starting at position (X, Y).
::end:: */
{
    int img_w, img_h;
    image_t out;

    rep_DECLARE1 (img, IMAGEP);
    img_w = image_width (VIMAGE (img));
    img_h = image_height (VIMAGE (img));

    rep_DECLARE (2, x, rep_INTP (x) && rep_INT (x) >= 0);
    rep_DECLARE (3, y, rep_INTP (y) && rep_INT (y) >= 0);
    rep_DECLARE (4, w, rep_INTP (w) && rep_INT (w) > 0
		 && rep_INT (w) <= (img_w - rep_INT (x)));
    rep_DECLARE (5, h, rep_INTP (h) && rep_INT (h) > 0
		 && rep_INT (h) <= (img_h - rep_INT (y)));

#if defined HAVE_IMLIB
    out = Imlib_crop_and_clone_image (imlib_id, VIMAGE (img)->image,
				      rep_INT (x), rep_INT (y),
				      rep_INT (w), rep_INT (h));
#elif defined HAVE_GDK_PIXBUF
    {
	GdkPixbuf *in = VIMAGE (img)->image;
	out = gdk_pixbuf_new (gdk_pixbuf_get_colorspace (in),
			      gdk_pixbuf_get_has_alpha (in),
			      gdk_pixbuf_get_bits_per_sample (in),
			      rep_INT (w), rep_INT (h));
	if (out != 0)
	{
	    gdk_pixbuf_copy_area (in, rep_INT (x), rep_INT (y),
				  rep_INT (w), rep_INT (h),
				  out, 0, 0);
	}
    }
#endif

    if (out != 0)
	return make_image (out, Fcopy_sequence (VIMAGE (img)->plist));
    else
	return rep_mem_error ();
}


/* image structure accessors */

int
image_width (Lisp_Image *im)
{
#if defined HAVE_IMLIB
    return im->image->rgb_width;
#elif defined HAVE_GDK_PIXBUF
    return gdk_pixbuf_get_width (im->image);
#endif
}

int
image_height (Lisp_Image *im)
{
#if defined HAVE_IMLIB
    return im->image->rgb_height;
#elif defined HAVE_GDK_PIXBUF
    return gdk_pixbuf_get_height (im->image);
#endif
}

u_char *
image_pixels (Lisp_Image *im)
{
#if defined HAVE_IMLIB
    return im->image->rgb_data;
#elif defined HAVE_GDK_PIXBUF
    return gdk_pixbuf_get_pixels (im->image);
#endif
}

int
image_row_stride (Lisp_Image *im)
{
#if defined HAVE_IMLIB
    return im->image->rgb_width * 3;
#elif defined HAVE_GDK_PIXBUF
    return gdk_pixbuf_get_rowstride (im->image);
#endif
}

int
image_channels (Lisp_Image *im)
{
#if defined HAVE_IMLIB
    return 3;
#elif defined HAVE_GDK_PIXBUF
    return gdk_pixbuf_get_n_channels (im->image);
#endif
}

void
image_changed (Lisp_Image *im)
{
#if defined NEED_PIXMAP_CACHE
    pixmap_cache_flush_image (im);
#endif
#if defined HAVE_IMLIB
    Imlib_changed_image (imlib_id, im->image);
#endif
}


/* render functions */

#if defined HAVE_GDK_PIXBUF
static void
do_scale (GdkPixbuf *src, int src_x, int src_y, int src_w, int src_h,
	  GdkPixbuf *dst, int dst_x, int dst_y, int dst_w, int dst_h)
{
    double scale_x, scale_y;
    double off_x, off_y;

    if (src_w <= 0 || src_h <= 0 || dst_w <= 0 || dst_h <= 0)
	return;

    scale_x = dst_w / (double) src_w;
    scale_y = dst_h / (double) src_h;

    off_x = dst_x - scale_x * src_x;
    off_y = dst_y - scale_y * src_y;

    gdk_pixbuf_scale (src, dst,
		      dst_x, dst_y,
		      dst_w, dst_h,
		      off_x, off_y,
		      scale_x, scale_y,
		      interp_type);
}
#endif

void
image_render (Lisp_Image *image, int width, int height,
	      Pixmap *pixmap, Pixmap *mask)
{
    image_t im = image->image;

    assert (width > 0);
    assert (height > 0);

#if defined HAVE_IMLIB
    Imlib_render (imlib_id, im, width, height);
    if (pixmap != 0)
	*pixmap = Imlib_move_image (imlib_id, im);
    if (mask != 0)
	*mask = Imlib_move_mask (imlib_id, im);

#elif defined HAVE_GDK_PIXBUF
    {
	GdkPixbuf *scaled = im;
	bool need_to_unref = FALSE;
	int im_width = image_width (image), im_height = image_height (image);

	if (pixmap_cache_ref (image, width, height, pixmap, mask))
	    return;

	/* XXX handle cases where combined image borders are larger
	   XXX than the destination image.. */

	if (im_width != width || im_height != height)
	{
	    /* need to scale to width by height */

	    int border[4];
	    border[0] = image->border[0];
	    border[1] = image->border[1];
	    border[2] = image->border[2];
	    border[3] = image->border[3];

	    /* truncate borders if dest image is too small */
	    if (border[0] + border[1] > width)
	    {
		border[0] = MIN (border[0], width / 2);
		border[1] = MIN (border[1], width / 2);
	    }
	    if (border[2] + border[3] > height
		|| image->border[2] + image->border[3] >= im_height)
	    {
		border[2] = MIN (border[2], height / 2);
		border[3] = MIN (border[3], height / 2);
	    }

	    assert (border[0] + border[1] <= width);
	    assert (border[2] + border[3] <= height);

	    /* create a new buffer */
	    scaled = gdk_pixbuf_new (gdk_pixbuf_get_colorspace (im),
				     gdk_pixbuf_get_has_alpha (im),
				     gdk_pixbuf_get_bits_per_sample (im),
				     width, height);
	    need_to_unref = TRUE;

	    /* stretch borders to fit scaled image */

	    if (border[0] > 0)
	    {
		do_scale (im, 0, image->border[2], image->border[0],
			  im_height - (image->border[2] + image->border[3]),
			  scaled, 0, border[2], border[0],
			  height - (border[2] + border[3]));
	    }
	    if (border[1] > 0)
	    {
		do_scale (im, im_width - image->border[1], image->border[2],
			  image->border[1],
			  im_height - (image->border[2] + image->border[3]),
			  scaled, width - border[1], border[2], border[1],
			  height - (border[2] + border[3]));
	    }

	    if (border[2] > 0)
	    {
		do_scale (im, image->border[0], 0,
			  im_width - (image->border[0] + image->border[1]),
			  image->border[2],
			  scaled, border[0], 0,
			  width - (border[0] + border[1]), border[2]);
	    }
	    if (border[3] > 0)
	    {
		do_scale (im, image->border[0], im_height - image->border[3],
			  im_width - (image->border[0] + image->border[1]),
			  image->border[3],
			  scaled, border[0], height - border[3],
			  width - (border[0] + border[1]), border[3]);
	    }

	    /* now do corner intersections between borders */

	    if (border[0] > 0 && border[2] > 0)
	    {
		do_scale (im, 0, 0, image->border[0], image->border[2],
			  scaled, 0, 0, border[0], border[2]);
	    }
	    if (border[1] > 0 && border[2] > 0)
	    {
		do_scale (im, im_width - image->border[1], 0,
			  image->border[1], image->border[2],
			  scaled, width - border[1], 0, border[1], border[2]);
	    }
	    if (border[0] > 0 && border[3] > 0)
	    {
		do_scale (im, 0, im_height - image->border[3],
			  image->border[0], image->border[3],
			  scaled, 0, height - border[3], border[0], border[3]);
	    }
	    if (border[1] > 0 && border[3] > 0)
	    {
		do_scale (im, im_width - image->border[1],
			  im_height - image->border[3],
			  image->border[1], image->border[3],
			  scaled, width - border[1], height - border[3],
			  border[1], border[3]);
	    }

	    /* scale the inner parts of the image */
	    if (border[0] + border[1] < width
		|| border[2] + border[3] < height)
	    {
		do_scale (im, image->border[0], image->border[2],
			  im_width - (image->border[0] + image->border[1]),
			  im_height - (image->border[2] + image->border[3]),
			  scaled, border[0], border[2],
			  width - (border[0] + border[1]),
			  height - (border[2] + border[3]));
	    }
	}

	gdk_pixbuf_xlib_render_pixmap_and_mask (scaled, pixmap, mask, 128);
	if (need_to_unref)
	    gdk_pixbuf_unref (scaled);

	pixmap_cache_set (image, width, height, *pixmap, *mask);
    }
#endif

    /* Imlib sometimes calls XSync (), which could hide events */
    rep_mark_input_pending (ConnectionNumber(dpy));
}

void
image_free_pixmaps (Lisp_Image *image, Pixmap pixmap, Pixmap mask)
{
#if defined NEED_PIXMAP_CACHE
    pixmap_cache_unref (image, pixmap, mask);
#endif
#if defined HAVE_IMLIB
    Imlib_free_pixmap (imlib_id, pixmap);
#endif
}

int
best_color_match (int red, int green, int blue)
{
#if defined HAVE_IMLIB
    return Imlib_best_color_match (imlib_id, &red, &green, &blue);
#elif defined HAVE_GDK_PIXBUF
    return xlib_rgb_xpixel_from_rgb ((red << 16) | (green << 8) | blue);
#endif
}

void
paste_image_to_drawable (Lisp_Image *img, Drawable d,
			 int x, int y, int w, int h)
{
#if defined HAVE_IMLIB
    Imlib_paste_image (imlib_id, img->image, d, x, y, w, h);

    /* Imlib sometimes calls XSync (), which could hide events */
    rep_mark_input_pending (ConnectionNumber(dpy));
#elif defined HAVE_GDK_PIXBUF
    GC gc;
    XGCValues gcv;
    int gcmask = 0;
    Pixmap pixmap, mask;
    image_render (img, w, h, &pixmap, &mask);
    if (mask != 0)
    {
	gcv.clip_mask = mask;
	gcv.clip_x_origin = x;
	gcv.clip_y_origin = y;
	gcmask |= GCClipMask | GCClipXOrigin | GCClipYOrigin;
    }
    gc = XCreateGC (dpy, d, gcmask, &gcv);
    XCopyArea (dpy, pixmap, d, gc, 0, 0, w, h, x, y);
    XFreeGC (dpy, gc);
    image_free_pixmaps (img, pixmap, mask);
#endif
}


/* lisp functions for accessing image contents */

static repv
get_pixel (Lisp_Image *im, int x, int y)
{
    int nchannels = image_channels (im);
    u_char *data = (image_pixels (im)
		    + (y * image_row_stride (im)) + (x * nchannels));
    repv alpha;
#ifdef HAVE_IMLIB
    ImlibColor shape;
    Imlib_get_image_shape (imlib_id, im->image, &shape);
    alpha = (data[0] == shape.r && data[1] == shape.g
	     && data[2] == shape.b) ? rep_MAKE_INT (0) : rep_MAKE_INT (255);
#else
    alpha = nchannels > 3 ? rep_MAKE_INT (data[3]) : rep_MAKE_INT (255);
#endif
    return rep_list_4 (rep_MAKE_INT (data[0]), rep_MAKE_INT (data[1]),
		       rep_MAKE_INT (data[2]), alpha);
}

static void
set_pixel (Lisp_Image *im, int x, int y, repv pixel)
{
    int length = rep_INT (Flength (pixel));
    if (length == 3 || length == 4)
    {
	int nchannels = image_channels (im);
	int alpha = (length > 3) ? rep_INT (rep_CADDDR (pixel)) : 255;
	u_char *data;

#ifdef HAVE_GDK_PIXBUF
	if (alpha != 255 && nchannels < 4)
	{
	    GdkPixbuf *new = gdk_pixbuf_add_alpha (im->image, FALSE, 0, 0, 0);
	    if (new != 0)
	    {
		gdk_pixbuf_unref (im->image);
		im->image = new;
		nchannels = image_channels (im);
	    }
	}
#endif
	data = (image_pixels (im)
		+ (y * image_row_stride (im)) + (x * nchannels));
#ifdef HAVE_IMLIB
	if (length > 3 && rep_INT (rep_CADDDR (pixel)) < 128)
	{
	    /* transparent */
	    ImlibColor shape;
	    Imlib_get_image_shape (imlib_id, im->image, &shape);
	    data[0] = shape.r;
	    data[1] = shape.g;
	    data[2] = shape.b;
	}
	else
#endif
	{
		
	    data[0] = rep_INT (rep_CAR (pixel));
	    data[1] = rep_INT (rep_CADR (pixel));
	    data[2] = rep_INT (rep_CADDR (pixel));
	    if (nchannels > 3)
		data[3] = alpha;
	}
    }
}

DEFUN("image-ref", Fimage_ref, Simage_ref,
      (repv im, repv x, repv y), rep_Subr3) /*
::doc:sawfish.wm.images#image-ref::
image-ref IMAGE X Y

Return a list `(R G B A)', the red, green, blue and alpha components of
the pixel at position (X, Y) of IMAGE.

All values are in the range 0 to 255 inclusive.
::end:: */
{
    int width, height;

    rep_DECLARE1 (im, IMAGEP);
    width = image_width (VIMAGE (im));
    height = image_height (VIMAGE (im));

    rep_DECLARE (2, x, rep_INTP (x) && rep_INT (x) >= 0
		 && rep_INT (x) < width);
    rep_DECLARE (3, y, rep_INTP (y) && rep_INT (y) >= 0
		 && rep_INT (y) < height);

    return get_pixel (VIMAGE (im), rep_INT (x), rep_INT (y));
}

DEFUN ("image-set", Fimage_set, Simage_set,
       (repv im, repv x, repv y, repv pixel), rep_Subr4) /*
::doc:sawfish.wm.images#image-set::
image-set IMAGE X Y PIXEL

Set the pixel at position (X, Y) in IMAGE to PIXEL. PIXEL is a list of
four numbers, `(R G B A)', the red, green, blue and alpha components of
the pixel. All values are in the range 0 to 255 inclusive.
::end:: */
{
    int width, height;

    rep_DECLARE1 (im, IMAGEP);
    width = image_width (VIMAGE (im));
    height = image_height (VIMAGE (im));

    rep_DECLARE (2, x, rep_INTP (x) && rep_INT (x) >= 0
		 && rep_INT (x) < width);
    rep_DECLARE (3, y, rep_INTP (y) && rep_INT (y) >= 0
		 && rep_INT (y) < height);
    rep_DECLARE (4, pixel, rep_LISTP (pixel));

    set_pixel (VIMAGE (im), rep_INT (x), rep_INT (y), pixel);
    image_changed (VIMAGE (im));
    return Qnil;
}

DEFUN ("image-map", Fimage_map, Simage_map, (repv fun, repv im), rep_Subr2) /*
::doc:sawfish.wm.images#image-map::
image-map XFORM IMAGE

Transform the values of all pixels in IMAGE according to XFORM.

XFORM is a function taking a single argument, the four element list
representing the current pixel value (as in `image-ref'). If XFORM
returns a non-nil value it should be the new pixel value (another four
element list).
::end:: */
{
    int width, height;
    int x, y;
    rep_GC_root gc_im, gc_fun;

    rep_DECLARE1 (im, IMAGEP);
    width = image_width (VIMAGE (im));
    height = image_height (VIMAGE (im));

    rep_DECLARE (2, fun, Ffunctionp (fun) != Qnil);

    rep_PUSHGC (gc_im, im);
    rep_PUSHGC (gc_fun, fun);
    for (y = 0; y < height; y++)
    {
	for (x = 0; x < width; x++)
	{
	    repv pix = rep_call_lisp1 (fun, get_pixel (VIMAGE (im), x, y));
	    if (pix == rep_NULL)
	    {
		rep_POPGC; rep_POPGC;
		return rep_NULL;
	    }
	    if (rep_CONSP (pix))
		set_pixel (VIMAGE (im), x, y, pix);
	}
    }
    image_changed (VIMAGE (im));
    rep_POPGC; rep_POPGC;
    return Qnil;
}

DEFUN ("image-fill", Fimage_fill, Simage_fill,
       (repv fun, repv im), rep_Subr2) /*
::doc:sawfish.wm.images#image-fill::
image-fill GENERATOR IMAGE

Set the values of all pixels in IMAGE according to GENERATOR.

GENERATOR is a function taking two arguments, the coordinates of a
pixel in the image. It should return the pixel value to set the value
of the specified pixel to (as accepted by `image-set').
::end:: */
{
    int width, height;
    int x, y;
    rep_GC_root gc_im, gc_fun;

    rep_DECLARE1 (im, IMAGEP);
    width = image_width (VIMAGE (im));
    height = image_height (VIMAGE (im));

    rep_DECLARE (2, fun, Ffunctionp (fun) != Qnil);

    rep_PUSHGC (gc_im, im);
    rep_PUSHGC (gc_fun, fun);
    for (y = 0; y < height; y++)
    {
	for (x = 0; x < width; x++)
	{
	    repv pix = rep_call_lisp2 (fun, rep_MAKE_INT (x),
				       rep_MAKE_INT (y));
	    if (pix == rep_NULL)
	    {
		rep_POPGC; rep_POPGC;
		return rep_NULL;
	    }
	    if (rep_CONSP (pix))
		set_pixel (VIMAGE (im), x, y, pix);
	}
    }
    image_changed (VIMAGE (im));
    rep_POPGC; rep_POPGC;
    return Qnil;
}


/* type hooks */

static int
image_cmp (repv w1, repv w2)
{
    return w1 != w2;
}

static void
image_prin (repv stream, repv obj)
{
    char buf[256];
    sprintf (buf, "#<image %dx%d>",
	     image_width (VIMAGE (obj)), image_height (VIMAGE (obj)));
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
image_mark (repv obj)
{
    rep_MARKVAL(VIMAGE(obj)->plist);
}

static void
image_sweep (void)
{
    Lisp_Image *w = image_list;
    image_list = 0;
    while (w != 0)
    {
	Lisp_Image *next = w->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
#if defined NEED_PIXMAP_CACHE
	    pixmap_cache_flush_image (w);
#endif
#if defined HAVE_IMLIB
	    Imlib_kill_image (imlib_id, w->image);
#elif defined HAVE_GDK_PIXBUF
	    gdk_pixbuf_unref (w->image);
#endif
	    rep_FREE_CELL(w);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = image_list;
	    image_list = w;
	}
	w = next;
    }
}


/* initialisation */

void
images_init (void)
{
    repv tem;
    image_type = rep_register_new_type ("image", image_cmp, image_prin,
					image_prin, image_sweep, image_mark,
					0, 0, 0, 0, 0, 0, 0);
    if (!batch_mode_p ())
    {
#if defined HAVE_IMLIB
	ImlibInitParams params;
	params.visualid = preferred_visual->visualid;
	params.flags = PARAMS_VISUALID;
	imlib_id = Imlib_init_with_params (dpy, &params);
	if (imlib_id == 0)
	{
	    fprintf (stderr, "sawfish: can't initialize Imlib\n");
	    exit (1);
	}
	image_cmap = Imlib_get_colormap (imlib_id);
	image_visual = Imlib_get_visual (imlib_id);
	image_depth = imlib_id->x.depth;
#elif defined HAVE_GDK_PIXBUF
	gdk_pixbuf_xlib_init (dpy, screen_num);
	image_cmap = xlib_rgb_get_cmap ();
	image_visual = xlib_rgb_get_visual ();
	image_depth = xlib_rgb_get_depth ();
#endif
    }

    tem = rep_push_structure ("sawfish.wm.images");
    rep_ADD_SUBR(Smake_image);
    rep_ADD_SUBR(Smake_image_from_x_drawable);
    rep_ADD_SUBR(Scopy_image);
    rep_ADD_SUBR(Sflip_image_horizontally);
    rep_ADD_SUBR(Sflip_image_vertically);
    rep_ADD_SUBR(Sflip_image_diagonally);
    rep_ADD_SUBR(Simage_get);
    rep_ADD_SUBR(Simage_put);
    rep_ADD_SUBR(Simagep);
    rep_ADD_SUBR(Simage_dimensions);
    rep_ADD_SUBR(Simage_border);
    rep_ADD_SUBR(Sset_image_shape_color);
    rep_ADD_SUBR(Simage_shape_color);
    rep_ADD_SUBR(Sset_image_border);
    rep_ADD_SUBR(Simage_modifier);
    rep_ADD_SUBR(Sset_image_modifier);
    rep_ADD_SUBR(Smake_sized_image);
    rep_ADD_SUBR(Sbevel_image);
    rep_ADD_SUBR(Sclear_image);
    rep_ADD_SUBR(Stile_image);
    rep_ADD_SUBR(Sscale_image);
    rep_ADD_SUBR(Scomposite_images);
    rep_ADD_SUBR(Scrop_image);
    rep_ADD_SUBR(Simage_ref);
    rep_ADD_SUBR(Simage_set);
    rep_ADD_SUBR(Simage_map);
    rep_ADD_SUBR(Simage_fill);
    rep_pop_structure (tem);

    rep_INTERN_SPECIAL(image_directory);
    Fset (Qimage_directory,
	  rep_concat2 (rep_STR (Fsymbol_value (Qsawfish_directory, Qt)),
		       "/images"));
    rep_INTERN_SPECIAL(image_load_path);
    Fset (Qimage_load_path,
	  rep_list_2 (rep_string_dup("."),
		      Fsymbol_value (Qimage_directory, Qt)));

    rep_INTERN_SPECIAL(default_bevel_percent);
    Fset (Qdefault_bevel_percent, rep_MAKE_INT(50));

    rep_INTERN(red);
    rep_INTERN(green);
    rep_INTERN(blue);
}

void
images_kill (void)
{
}
