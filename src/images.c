/* images.c -- Image handling
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

static Lisp_Image *image_list;
int image_type;

ImlibData *imlib_id;

DEFSYM(image_load_path, "image-load-path");
DEFSYM(image_directory, "image-directory");

DEFSYM(red, "red");
DEFSYM(green, "green");
DEFSYM(blue, "blue");

/* Make a Lisp image object from the imlib image IM. Its initial properties
   will be taken from the list PLIST. */
static repv
make_image (ImlibImage *im, repv plist)
{
    Lisp_Image *f;
    for (f = image_list; f != 0; f = f->next)
    {
	if (f->image == im)
	    /* XXX do what with the plist..? */
	    return rep_VAL(f);
    }
    f = rep_ALLOC_CELL(sizeof(Lisp_Image));
    f->car = image_type;
    f->next = image_list;
    image_list = f;
    f->image = im;
    f->plist = plist;
    return rep_VAL(f);
}

DEFUN("make-image", Fmake_image, Smake_image,
      (repv file, repv plist), rep_Subr2) /*
::doc:Smake-image::
make-image FILE [PLIST]

Return a new image object representing the image stored in FILE (a
string). PLIST defines the property list of the image.
::end:: */
{
    repv path;
    rep_GC_root gc_file, gc_plist, gc_path;
    rep_DECLARE1(file, rep_STRINGP);

    rep_PUSHGC(gc_file, file);
    rep_PUSHGC(gc_plist, plist);
    path = Fsymbol_value (Qimage_load_path, Qt);
    rep_PUSHGC(gc_path, path);
    while (rep_CONSP(path))
    {
	repv all = Fexpand_file_name (file, rep_CAR(path));
	if (all && rep_STRINGP(all))
	{
	    repv tem;
	    rep_GC_root gc_all;
	    rep_PUSHGC(gc_all, all);
	    tem = Ffile_regular_p (all);
	    rep_POPGC;
	    if (tem && tem != Qnil)
	    {
		file = Flocal_file_name (all);
		if (file && rep_STRINGP(file))
		{
		    ImlibImage *im = Imlib_load_image (imlib_id,
						       rep_STR(file));
		    if (im != 0)
		    {
			rep_POPGC; rep_POPGC; rep_POPGC;
			return make_image (im, plist);
		    }
		}
	    }
	}
	path = rep_CDR(path);
    }
    rep_POPGC; rep_POPGC; rep_POPGC;
    return Fsignal (Qerror, rep_list_2(rep_string_dup("no such image"), file));
}

DEFUN("copy-image", Fcopy_image, Scopy_image, (repv source), rep_Subr1) /*
::doc:Scopy-image::
copy-image SOURCE-IMAGE

Return a new image object, a clone of SOURCE-IMAGE.
::end:: */
{
    ImlibImage *im;
    rep_DECLARE1(source, IMAGEP);
    im = Imlib_clone_image (imlib_id, VIMAGE(source)->image);
    if (im != 0)
	return make_image (im, Fcopy_sequence (VIMAGE(source)->plist));
    return Fsignal (Qerror,
		    rep_list_2 (rep_string_dup("can't clone image"), source));
}

DEFUN("flip-image-horizontally", Fflip_image_horizontally,
      Sflip_image_horizontally, (repv image), rep_Subr1) /*
::doc:Sflip-image-horizontally::
flip-image-horizontally IMAGE 

Flip the contents of IMAGE around the vertical axis.
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
    Imlib_flip_image_horizontal (imlib_id, VIMAGE(image)->image);
    return image;
}

DEFUN("flip-image-vertically", Fflip_image_vertically,
      Sflip_image_vertically, (repv image), rep_Subr1) /*
::doc:Sflip-image-vertically::
flip-image-vertically IMAGE 

Flip the contents of IMAGE around the horizontal axis.
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
    Imlib_flip_image_vertical (imlib_id, VIMAGE(image)->image);
    return image;
}

DEFUN("flip-image-diagonally", Fflip_image_diagonally,
      Sflip_image_diagonally, (repv image), rep_Subr1) /*
::doc:Sflip-image-diagonally::
flip-image-diagonally IMAGE 

Flip the contents of IMAGE around a diagonal axis from the top-left to
the bottom right of the image.
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
    Imlib_rotate_image (imlib_id, VIMAGE(image)->image, 1);
    return image;
}

DEFUN("image-get", Fimage_get, Simage_get, (repv win, repv prop), rep_Subr2) /*
::doc::Simage-get::
image-get IMAGE PROPERTY

Return the value of the property named PROPERTY (a symbol) of IMAGE.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, IMAGEP);
    plist = VIMAGE(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop)
	    return rep_CAR(rep_CDR(plist));
	plist = rep_CDR(rep_CDR(plist));
    }
    return Qnil;
}

DEFUN("image-put", Fimage_put, Simage_put,
      (repv win, repv prop, repv val), rep_Subr3) /*
::doc:Simage-put::
image-put IMAGE PROPERTY VALUE

Set the value of the property named PROPERTY (a symbol) of IMAGE to VALUE.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, IMAGEP);
    plist = VIMAGE(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop)
	{
	    if (!rep_CONS_WRITABLE_P(rep_CDR(plist)))
	    {
		/* Can't write into a dumped cell; need to cons
		   onto the head. */
		break;
	    }
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
::doc:Simagep::
image ARG

Returns t if ARG is an image object.
::end:: */
{
    return IMAGEP(arg) ? Qt : Qnil;
}

DEFUN("image-dimensions", Fimage_dimensions, Simage_dimensions,
      (repv img), rep_Subr1) /*
::doc:Simage-dimensions::
image-dimensions IMAGE

Return (WIDTH . HEIGHT) representing the dimensions in pixels of IMAGE.
::end:: */
{
    rep_DECLARE1(img, IMAGEP);
    return Fcons (rep_MAKE_INT(VIMAGE(img)->image->rgb_width),
		  rep_MAKE_INT(VIMAGE(img)->image->rgb_height));
}

DEFUN("image-border", Fimage_border, Simage_border, (repv img), rep_Subr1) /*
::doc:Simage-border::
image-border IMAGE

Return (LEFT RIGHT TOP BOTTOM) representing the border (in pixels) of IMAGE.
::end:: */
{
    ImlibBorder border;
    rep_DECLARE1(img, IMAGEP);
    Imlib_get_image_border (imlib_id, VIMAGE(img)->image, &border);
    return rep_list_4 (rep_MAKE_INT(border.left), rep_MAKE_INT(border.right),
		       rep_MAKE_INT(border.top), rep_MAKE_INT(border.bottom));
}

DEFUN("set-image-border", Fset_image_border, Sset_image_border,
      (repv img, repv left, repv right, repv top, repv bottom), rep_Subr5) /*
::doc:Sset-image-border::
set-image-border IMAGE LEFT RIGHT TOP BOTTOM

Set the border of IMAGE to (LEFT RIGHT TOP BOTTOM). The border of an
image defines how that image is scaled -- only pixels inside the border
are resized.
::end:: */
{
    ImlibBorder border;
    rep_DECLARE1(img, IMAGEP);
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
    return img;
}

DEFUN("image-modifier", Fimage_modifier, Simage_modifier,
      (repv img, repv type), rep_Subr2) /*
::doc:Simage-modifier::
image-modifier IMAGE TYPE

TYPE may be one of nil, red, green, blue. returned modifier is
(GAMMA BRIGHTNESS CONTRAST). All values range from 0 to 255.
::end:: */
{
    ImlibColorModifier modifier;
    void (*fun)(ImlibData *, ImlibImage *, ImlibColorModifier *);
    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE2(type, rep_SYMBOLP);
    fun = (type == Qred ? Imlib_get_image_red_modifier
	   : type == Qgreen ? Imlib_get_image_green_modifier
	   : type == Qblue ? Imlib_get_image_blue_modifier
	   : Imlib_get_image_modifier);
    (*fun) (imlib_id, VIMAGE(img)->image, &modifier);
    return rep_list_3 (rep_MAKE_INT(modifier.gamma),
		       rep_MAKE_INT(modifier.brightness),
		       rep_MAKE_INT(modifier.contrast));
}

DEFUN("set-image-modifier", Fset_image_modifier, Sset_image_modifier,
      (repv img, repv type, repv mod), rep_Subr3) /*
::doc:Sset-image-modifier::
set-image-modifier IMAGE TYPE MODIFIER

TYPE may be one of nil, red, green, blue. MODIFIER is (GAMMA BRIGHTNESS
CONTRAST). These are integers ranging from 0 to 255.
::end:: */
{
    ImlibColorModifier modifier;
    void (*fun)(ImlibData *, ImlibImage *, ImlibColorModifier *);
    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE2(type, rep_SYMBOLP);
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
}

static inline void
bevel_pixel (u_char *data, bool up)
{
    double pix[3];
    pix[0] = ((double)data[0]) / 256.0;
    pix[1] = ((double)data[1]) / 256.0;
    pix[2] = ((double)data[2]) / 256.0;
    if (up)
    {
	pix[0] = pix[0] + (1.0 - pix[0]) * 0.75;
	pix[1] = pix[1] + (1.0 - pix[1]) * 0.75;
	pix[2] = pix[2] + (1.0 - pix[2]) * 0.75;
	data[0] = pix[0] * 256.0;
	data[1] = pix[1] * 256.0;
	data[2] = pix[2] * 256.0;
    }
    else
    {
	pix[0] = pix[0] - pix[0] * 0.75;
	pix[1] = pix[1] - pix[1] * 0.75;
	pix[2] = pix[2] - pix[2] * 0.75;
	data[0] = pix[0] * 256.0;
	data[1] = pix[1] * 256.0;
	data[2] = pix[2] * 256.0;
    }
}

static inline void
bevel_horizontally (u_char *data, int width, int height,
		    int border, bool top, bool up)
{
    int rows;
    up = top ? up : !up;
    for (rows = 0; rows < border; rows++)
    {
	u_char *ptr = data;
	int x;
	if (top)
	    ptr += rows * 3;
	else
	    ptr += width * (height - (rows + 1)) * 3;
	for (x = rows; x < width - (rows + 1); x++)
	{
	    bevel_pixel (ptr, up);
	    ptr += 3;
	}
    }
}

static inline void
bevel_vertically (u_char *data, int width, int height,
		  int border, bool top, bool up)
{
    int cols;
    up = top ? up : !up;
    for (cols = 0; cols < border; cols++)
    {
	u_char *ptr = data;
	int y;
	if (top)
	    ptr += cols * 3;
	else
	    ptr += (width - (cols + 1)) * 3;
	for (y = cols; y < height - (cols + 1); y++)
	{
	    bevel_pixel (ptr, up);
	    ptr += width * 3;
	}
    }
}

DEFUN("bevel-image", Fbevel_image, Sbevel_image,
      (repv image, repv border, repv up), rep_Subr3) /*
::doc:Sbevel-image::
bevel-image IMAGE BORDER UP

Draw a bevelled edge outline onto IMAGE. BORDER is an integer defining
the width of the bevel. If UP is non-nil the bevel is raised.
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
    rep_DECLARE2(border, rep_INTP);

    bevel_horizontally (VIMAGE(image)->image->rgb_data,
			VIMAGE(image)->image->rgb_width,
			VIMAGE(image)->image->rgb_height,
			rep_INT(border), TRUE, up != Qnil);
    bevel_horizontally (VIMAGE(image)->image->rgb_data,
			VIMAGE(image)->image->rgb_width,
			VIMAGE(image)->image->rgb_height,
			rep_INT(border), FALSE, up != Qnil);
    bevel_vertically (VIMAGE(image)->image->rgb_data,
		      VIMAGE(image)->image->rgb_width,
		      VIMAGE(image)->image->rgb_height,
		      rep_INT(border), TRUE, up != Qnil);
    bevel_vertically (VIMAGE(image)->image->rgb_data,
		      VIMAGE(image)->image->rgb_width,
		      VIMAGE(image)->image->rgb_height,
		      rep_INT(border), FALSE, up != Qnil);

    Imlib_changed_image (imlib_id, VIMAGE(image)->image);
    return image;
}

DEFUN("clear-image", Fclear_image, Sclear_image,
      (repv image, repv color), rep_Subr2) /*
::doc:Sclear-image::
clear-image IMAGE [COLOR]

Set all pixels in IMAGE to COLOR (or black if COLOR is undefined).
::end:: */
{
    int i, r, g, b;
    u_char *data;
    rep_DECLARE1(image, IMAGEP);
    if (COLORP(color))
    {
	r = VCOLOR(color)->color.red / 256;
	g = VCOLOR(color)->color.green / 256;
	b = VCOLOR(color)->color.blue / 256;
    }
    else
	r = g = b = 0;
    data = VIMAGE(image)->image->rgb_data;
    for (i = 0; i < VIMAGE(image)->image->rgb_height * VIMAGE(image)->image->rgb_width; i++)
    {
	*data++ = r;
	*data++ = g;
	*data++ = b;
    }

    Imlib_changed_image (imlib_id, VIMAGE(image)->image);
    return image;
}

/* XXX stubs for suitable Imlib functions... */

DEFUN("make-sized-image", Fmake_sized_image, Smake_sized_image,
      (repv width, repv height, repv color), rep_Subr3) /*
::doc:Smake-sized-image::
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
	r = VCOLOR(color)->color.red / 256;
	g = VCOLOR(color)->color.green / 256;
	b = VCOLOR(color)->color.blue / 256;
    }
    else
	r = g = b = 0;

    data = rep_alloc (rep_INT(width) * rep_INT(height) * 3);
    if (data != 0)
    {
	ImlibImage *im;
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
	if (im != 0)
	    return make_image (im, Qnil);
    }
    return Qnil;
}

DEFUN("tile-image", Ftile_image, Stile_image, (repv dst, repv src), rep_Subr2) /*
::doc:Stile-image::
tile-image DEST-IMAGE SOURCE-IMAGE

Tile SOURCE-IMAGE into DEST-IMAGE.
::end:: */
{
    ImlibImage *src_im, *dst_im;
    int x, y;
    rep_DECLARE1(dst, IMAGEP);
    rep_DECLARE2(src, IMAGEP);
    src_im = VIMAGE(src)->image;
    dst_im = VIMAGE(dst)->image;
    for (y = 0; y < dst_im->rgb_height; y++)
    {
	for (x = 0; x < dst_im->rgb_width; x += src_im->rgb_width)
	{
	    memcpy (dst_im->rgb_data + y*dst_im->rgb_width*3 + x*3,
		    src_im->rgb_data + y*src_im->rgb_width*3,
		    MIN (dst_im->rgb_width - x, src_im->rgb_width) * 3);
	}
    }
    Imlib_changed_image (imlib_id, dst_im);
    return dst;
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
    sprintf (buf, "#<image %p>", rep_STR(VIMAGE(obj)->image));
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
	    Imlib_destroy_image (imlib_id, w->image);
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
    ImlibInitParams params;
    image_type = rep_register_new_type ("image", image_cmp, image_prin,
					image_prin, image_sweep, image_mark,
					0, 0, 0, 0, 0, 0, 0);
    if (rep_SYM(Qbatch_mode)->value == Qnil)
    {
	params.visualid = screen_visual->visualid;
	params.flags = PARAMS_VISUALID;
	imlib_id = Imlib_init_with_params (dpy, &params);
    }
    rep_ADD_SUBR(Smake_image);
    rep_ADD_SUBR(Scopy_image);
    rep_ADD_SUBR(Sflip_image_horizontally);
    rep_ADD_SUBR(Sflip_image_vertically);
    rep_ADD_SUBR(Sflip_image_diagonally);
    rep_ADD_SUBR(Simage_get);
    rep_ADD_SUBR(Simage_put);
    rep_ADD_SUBR(Simagep);
    rep_ADD_SUBR(Simage_dimensions);
    rep_ADD_SUBR(Simage_border);
    rep_ADD_SUBR(Sset_image_border);
    rep_ADD_SUBR(Simage_modifier);
    rep_ADD_SUBR(Sset_image_modifier);
    rep_ADD_SUBR(Smake_sized_image);
    rep_ADD_SUBR(Sbevel_image);
    rep_ADD_SUBR(Sclear_image);
    rep_ADD_SUBR(Stile_image);
    rep_INTERN(image_directory);
    rep_SYM(Qimage_directory)->value
	= rep_concat2 (rep_STR(rep_SYM(Qsawmill_directory)->value), "/images");
    rep_INTERN(image_load_path);
    rep_SYM(Qimage_load_path)->value
	= rep_list_2 (rep_string_dup("."), rep_SYM(Qimage_directory)->value);

    rep_INTERN(red);
    rep_INTERN(green);
    rep_INTERN(blue);
}

void
images_kill (void)
{
}
