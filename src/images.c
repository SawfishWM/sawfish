/* images.c -- Image handling
   $Id$ */

#include "sawmill.h"

static Lisp_Image *image_list;
int image_type;

ImlibData *imlib_id;

DEFSYM(image_load_path, "image-load-path");
DEFSYM(image_directory, "image-directory");

static repv
make_image (ImlibImage *im, repv plist)
{
    Lisp_Image *f = rep_ALLOC_CELL(sizeof(Lisp_Image));
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
::end:: */
{
    ImlibImage *im;
    rep_DECLARE1(source, IMAGEP);
    im = Imlib_clone_image (imlib_id, VIMAGE(source)->image);
    if (im != 0)
	return make_image (im, VIMAGE(source)->plist);
    return Fsignal (Qerror,
		    rep_list_2 (rep_string_dup("can't clone image"), source));
}

DEFUN("flip-image-horizontally", Fflip_image_horizontally,
      Sflip_image_horizontally, (repv image), rep_Subr1) /*
::doc:Sflip-image-horizontally::
flip-image-horizontally IMAGE 
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
::end:: */
{
    rep_DECLARE1(image, IMAGEP);
    Imlib_rotate_image (imlib_id, VIMAGE(image)->image, 1);
    return image;
}

DEFUN("image-get", Fimage_get, Simage_get, (repv win, repv prop), rep_Subr2) /*
::doc::Simage-get::
image-get IMAGE PROPERTY
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

DEFUN("image-dimensions", Fimage_dimensions, Simage_dimensions,
      (repv img), rep_Subr1) /*
::doc:Simage-dimensions::
image-dimensions IMAGE
::end:: */
{
    rep_DECLARE1(img, IMAGEP);
    return Fcons (rep_MAKE_INT(VIMAGE(img)->image->rgb_width),
		  rep_MAKE_INT(VIMAGE(img)->image->rgb_height));
}

DEFUN("image-border", Fimage_border, Simage_border, (repv img), rep_Subr1) /*
::doc:Simage-border::
image-border IMAGE
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

/* XXX stubs for suitable Imlib functions... */


/* type hooks */

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
    image_type = rep_register_new_type ("image", 0, image_prin, image_prin,
					image_sweep, image_mark,
					0, 0, 0, 0, 0, 0, 0);
    params.visualid = screen_visual->visualid;
    params.flags = PARAMS_VISUALID;
    imlib_id = Imlib_init_with_params (dpy, &params);
    rep_ADD_SUBR(Smake_image);
    rep_ADD_SUBR(Scopy_image);
    rep_ADD_SUBR(Sflip_image_horizontally);
    rep_ADD_SUBR(Sflip_image_vertically);
    rep_ADD_SUBR(Sflip_image_diagonally);
    rep_ADD_SUBR(Simage_get);
    rep_ADD_SUBR(Simage_put);
    rep_ADD_SUBR(Simage_dimensions);
    rep_ADD_SUBR(Simage_border);
    rep_ADD_SUBR(Sset_image_border);
    rep_INTERN(image_directory);
    rep_SYM(Qimage_directory)->value
	= rep_concat2 (rep_STR(rep_SYM(Qsawmill_directory)->value), "/images");
    rep_INTERN(image_load_path);
    rep_SYM(Qimage_load_path)->value
	= rep_list_2 (rep_string_dup("."), rep_SYM(Qimage_directory)->value);
}

void
images_kill (void)
{
}
