/* fonts.c -- font manipulation
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
#include <string.h>
#include <X11/Xlocale.h>
#include <ctype.h>

#ifdef HAVE_X11_XFT_XFT_H
# include <X11/Xft/Xft.h>
#endif

static Lisp_Font *font_list;
int font_type;

DEFSYM(default_font, "default-font");

struct Lisp_Font_Class_struct {
    const char *type;
    bool (*load) (Lisp_Font *f);
    void (*finalize) (Lisp_Font *f);
    int (*measure) (Lisp_Font *f, u_char *string, size_t length);
    void (*draw) (Lisp_Font *f, u_char *string, size_t length,
		  Window id, GC gc, Lisp_Color *fg, int x, int y);
};


/* Xlib font structs */

static bool
fontstruct_load (Lisp_Font *f)
{
    XFontStruct *font_struct;

    font_struct = XLoadQueryFont (dpy, rep_STR (f->name));

    if (font_struct == 0)
	return FALSE;

    f->font = font_struct;
    f->ascent = font_struct->ascent;
    f->descent = font_struct->descent;

    return TRUE;
}

static void
fontstruct_finalize (Lisp_Font *f)
{
    XFreeFont (dpy, f->font);
}

static int
fontstruct_measure (Lisp_Font *f, u_char *string, size_t length)
{
    return XTextWidth (f->font, string, length);
}

static void
fontstruct_draw (Lisp_Font *f, u_char *string, size_t length,
		 Window id, GC gc, Lisp_Color *fg, int x, int y)
{
    XFontStruct *fs;
    XGCValues gcv;

    fs = f->font;

    gcv.foreground = fg->pixel;
    gcv.font = fs->fid;
    XChangeGC (dpy, gc, GCForeground | GCFont, &gcv);

    XDrawString (dpy, id, gc, x, y, string, length);
}

static const Lisp_Font_Class fontstruct_class = {
    "xlfd",
    fontstruct_load, fontstruct_finalize,
    fontstruct_measure, fontstruct_draw,
};


/* Xlib font sets */

static char *
xlfd_get_element (const char *xlfd, int idx)
{
    const char *p = xlfd;
    while (*p != 0)
    {
	if (*p == '-' && --idx == 0)
	{
	    const char *end = strchr (p + 1, '-');
	    char *buf;
	    size_t len;
	    if (end == 0)
		end = p + strlen (p);
	    len = end - (p + 1);
	    buf = malloc (len);
	    memcpy (buf, p + 1, len);
	    buf[len] = 0;
	    return buf;
	}
	p++;
    }
    return 0;
}

static char *
generalize_xlfd (const char *xlfd)
{
    char *weight = xlfd_get_element (xlfd, 3);
    char *slant = xlfd_get_element (xlfd, 4);
    char *pxlsz = xlfd_get_element (xlfd, 7);

    char *buf;
    int len;

    if (weight == 0)
	weight = strdup ("*");
    if (slant == 0)
	slant = strdup ("*");
    if (pxlsz == 0)
	pxlsz = strdup ("*");

#define XLFD_FORMAT "%s,-*-*-%s-%s-*-*-%s-*-*-*-*-*-*-*," \
		    "-*-*-*-*-*-*-%s-*-*-*-*-*-*-*,*"

    len = strlen (xlfd) + strlen (weight) + strlen (slant) +
          strlen (pxlsz) + strlen (pxlsz) + strlen (XLFD_FORMAT);
    buf = malloc (len + 1);
#ifdef HAVE_SNPRINTF
    snprintf (buf, len + 1, XLFD_FORMAT, xlfd, weight, slant, pxlsz, pxlsz);
#else
    sprintf (buf, XLFD_FORMAT, xlfd, weight, slant, pxlsz, pxlsz);
#endif

    free (pxlsz);
    free (slant);
    free (weight);

    return buf;
}

static XFontSet
x_create_fontset (char *xlfd, char ***missing,
		   int *nmissing, char **def_string)
{
    XFontSet fs = XCreateFontSet (dpy, xlfd, missing, nmissing, def_string);

    if (fs != 0 && *nmissing == 0)
	return fs;

    /* for non-iso8859-1 language and iso8859-1 specification
       (this fontset is only for pattern analysis) */

    if (fs == 0)
    {
	char *old_locale, *tem;

	if (*nmissing != 0)
	    XFreeStringList (*missing);

	/* Save the old LC_CTYPE locale.. */
	tem = setlocale (LC_CTYPE, 0);
	if (tem != 0)
	{
	    old_locale = alloca (strlen (tem) + 1);
	    strcpy (old_locale, tem);

	    /* ..then create the fontset in the default locale.. */
	    setlocale (LC_CTYPE, "C");
	}
	else
	    old_locale = 0;

	fs = XCreateFontSet (dpy, xlfd, missing, nmissing, def_string);

	/* ..then restore the original locale */
	if (old_locale != 0)
	    setlocale (LC_CTYPE, old_locale);
    }

    /* make XLFD font name for pattern analysis */
    if (fs != 0)
    {
	XFontStruct **fontstructs;
	char **fontnames;
	if (XFontsOfFontSet (fs, &fontstructs, &fontnames) > 0)
	    xlfd = fontnames[0];
    }

    xlfd = generalize_xlfd (xlfd);

    if (*nmissing != 0)
	XFreeStringList (*missing);
    if (fs != 0)
	XFreeFontSet (dpy, fs);

    fs = XCreateFontSet (dpy, xlfd, missing, nmissing, def_string);

    free (xlfd);
    return fs;
}

static bool
fontset_load (Lisp_Font *f)
{
    XFontSet font_set;
    int ascent, descent;

    char **missing_charset_list, *def_string;
    int num_missing_charset_list;

    font_set = x_create_fontset (rep_STR (f->name),
				 &missing_charset_list,
				 &num_missing_charset_list,
				 &def_string);

    if (font_set != 0)
    {
	XFontStruct **fstrs;
	char **font_names;
	int i, j, num_fonts;

	f->font = font_set;

	num_fonts = XFontsOfFontSet (font_set, &fstrs, &font_names);
	ascent = descent = 0;

	for (i = 0; i < num_fonts; i++)
	{
	    if (fstrs[i]->ascent > ascent)
		f->ascent = fstrs[i]->ascent;
	    if (fstrs[i]->descent > descent)
		f->descent = fstrs[i]->descent;
	}

	if (num_missing_charset_list > 0)
	{
	    fprintf (stderr, "Missing charsets in FontSet creation\n");
	    for (j = 0; j < num_missing_charset_list; j++)
		fprintf (stderr, "\t%s\n", missing_charset_list[j]);
	    XFreeStringList (missing_charset_list);
	}

	return TRUE;
    }

    return FALSE;
}

static void
fontset_finalize (Lisp_Font *f)
{
    XFreeFontSet (dpy, f->font);
}

static int
fontset_measure (Lisp_Font *f, u_char *string, size_t length)
{
    return XmbTextEscapement (f->font, string, length);
}

static void
fontset_draw (Lisp_Font *f, u_char *string, size_t length,
	      Window id, GC gc, Lisp_Color *fg, int x, int y)
{
    XGCValues gcv;

    gcv.foreground = fg->pixel;
    XChangeGC (dpy, gc, GCForeground, &gcv);

    XmbDrawString (dpy, id, f->font, gc, x, y, string, length);
}

static const Lisp_Font_Class fontset_class = {
    "xlfd",
    fontset_load, fontset_finalize,
    fontset_measure, fontset_draw,
};


/* Xft fonts */

#ifdef HAVE_X11_XFT_XFT_H

static bool
xft_load (Lisp_Font *f)
{
    XftFont *xft_font;

    xft_font = XftFontOpenName (dpy, screen_num, rep_STR (f->name));

    if (xft_font == 0)
	return FALSE;

    f->font = xft_font;
    f->ascent = xft_font->ascent;
    f->descent = xft_font->descent;

    return TRUE;
}

static void
xft_finalize (Lisp_Font *f)
{
    XftFontClose (dpy, f->font);
}

static int
xft_measure (Lisp_Font *f, u_char *string, size_t length)
{
    XGlyphInfo info;

    XftTextExtents8 (dpy, f->font, string, length, &info);

    return info.xOff; 
}

static void
xft_draw (Lisp_Font *f, u_char *string, size_t length,
	  Window id, GC gc, Lisp_Color *fg, int x, int y)
{
    static XftDraw *draw;

    XftColor xft_color;

    if (draw == 0)
	draw = XftDrawCreate (dpy, id, image_visual, image_cmap);
    else
	XftDrawChange (draw, id);

    xft_color.pixel = fg->pixel;
    xft_color.color.red = fg->red;
    xft_color.color.green = fg->green;
    xft_color.color.blue = fg->blue;
    xft_color.color.alpha = 65535;	/* FIXME: */

    XftDrawString8 (draw, &xft_color, f->font,
		    x, y, string, length);
}

static const Lisp_Font_Class xft_class = {
    "Xft",
    xft_load, xft_finalize,
    xft_measure, xft_draw,
};

#endif /* HAVE_X11_XFT_XFT_H */


/* All classes */

static const Lisp_Font_Class *classes[] = {
    &fontstruct_class,
    &fontset_class,
#ifdef HAVE_X11_XFT_XFT_H
    &xft_class,
#endif
    0,
};


/* Entry points */

DEFUN ("font-type-exists-p", Ffont_type_exists_p,
       Sfont_type_exists_p, (repv type), rep_Subr1) /*
::doc:sawfish.wm.fonts#font-type-exists-p::
font-type-exists-p TYPE

Returns true if fonts with the type described by the string TYPE can be
loaded.
::end:: */
{
    int i;

    rep_DECLARE1 (type, rep_STRINGP);

    for (i = 0; classes[i] != 0; i++)
    {
	if (strcasecmp (rep_STR (type), classes[i]->type) == 0)
	    return Qt;
    }

    return Qnil;
}

DEFUN("get-font-typed", Fget_font_typed, Sget_font_typed,
      (repv type, repv name), rep_Subr2) /*
::doc:sawfish.wm.fonts#get-font-typed::
get-font-typed TYPE NAME

Return the font object representing the font named NAME. NAME is
interpreted based on the value of the string TYPE.
::end:: */
{
    Lisp_Font *f;
    const Lisp_Font_Class *class;
    repv tem;
    int i;

    rep_DECLARE1(name, rep_STRINGP);
    rep_DECLARE2(type, rep_STRINGP);

    if (dpy == 0)
	return Qnil;

    for (f = font_list; f != NULL; f = f->next)
    {
	if (strcmp (rep_STR(name), rep_STR(f->name)) == 0
	    && strcmp (rep_STR (type), rep_STR (f->name)) == 0)
	{
	    return rep_VAL (f);
	}
    }

    class = 0;

    if (strcasecmp (rep_STR (type), "xlfd") == 0)
    {
	/* Boring old X core fonts */

	tem = global_symbol_value (Qfonts_are_fontsets);
	if (tem != Qnil)
	    class = &fontset_class;
	else
	    class = &fontstruct_class;
    }
    else
    {
	for (i = 0; classes[i] != 0; i++)
	{
	    if (strcasecmp (rep_STR (type), classes[i]->type) == 0)
	    {
		class = classes[i];
		break;
	    }
	}
    }

    if (class == 0)
    {
	DEFSTRING (err, "unknown font type");
	return Fsignal (Qerror, rep_list_2 (rep_VAL (&err), type));
    }
	
    f = rep_ALLOC_CELL(sizeof(Lisp_Font));

    f->car = font_type;
    f->class = class;
    f->type = type;
    f->name = name;
    f->plist = Qnil;

    if (!(*class->load) (f))
    {
	DEFSTRING (err, "unknown font");

	rep_FREE_CELL (f);
	return Fsignal (Qerror, rep_list_2 (rep_VAL (&err), name));
    }

    rep_data_after_gc += sizeof (Lisp_Font);

    f->next = font_list;
    font_list = f;

    return rep_VAL (f);
}

DEFUN("get-font", Fget_font, Sget_font, (repv name), rep_Subr1) /*
::doc:sawfish.wm.fonts#get-font::
get-font NAME

Return the font object representing the font named NAME (a standard X
font specifier string).
::end:: */
{
    DEFSTRING (type, "xlfd");

    return Fget_font_typed (rep_VAL (&type), name);
}

DEFUN("font-get", Ffont_get, Sfont_get, (repv win, repv prop), rep_Subr2) /*
::doc:sawfish.wm.fonts#font-get::
font-get FONT PROPERTY

Return the property PROPERTY (a symbol) associated with FONT.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, FONTP);
    plist = VFONT(win)->plist;
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

DEFUN("font-put", Ffont_put, Sfont_put, (repv win, repv prop, repv val), rep_Subr3) /*
::doc:sawfish.wm.fonts#font-put::
font-put FONT PROPERTY VALUE

Set the property PROPERTY (a symbol) associated with FONT to VALUE.
::end:: */
{
    repv plist;
    rep_DECLARE1(win, FONTP);
    plist = VFONT(win)->plist;
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
    plist = Fcons(prop, Fcons(val, VFONT(win)->plist));
    if (plist != rep_NULL)
	VFONT(win)->plist = plist;
    return val;
}

DEFUN("font-type", Ffont_type, Sfont_type, (repv font), rep_Subr1) /*
::doc:sawfish.wm.fonts#font-type::
font-type FONT

Return the type of the font represented by the font object FONT.
::end:: */
{
    rep_DECLARE1(font, FONTP);
    return VFONT(font)->type;
}

DEFUN("font-name", Ffont_name, Sfont_name, (repv font), rep_Subr1) /*
::doc:sawfish.wm.fonts#font-name::
font-name FONT

Return the name of the font represented by the font object FONT.
::end:: */
{
    rep_DECLARE1(font, FONTP);
    return VFONT(font)->name;
}

DEFUN("fontp", Ffontp, Sfontp, (repv win), rep_Subr1) /*
::doc:sawfish.wm.fonts#fontp::
fontp ARG

Return t if ARG is a font object.
::end:: */
{
    return FONTP(win) ? Qt : Qnil;
}

int
x_text_width (repv font, u_char *string, size_t len)
{
    return (*VFONT (font)->class->measure) (VFONT (font), string, len);
}

/* The foreground pixel of GC is undefined after this function returns. */
void
x_draw_string (Window id, repv font, GC gc, Lisp_Color *fg_color,
	       int x, int y, u_char *string, size_t len)
{
    return (*VFONT (font)->class->draw) (VFONT (font), string, len,
					 id, gc, fg_color, x, y);
}

DEFUN("text-width", Ftext_width, Stext_width, (repv string, repv font), rep_Subr2) /*
::doc:sawfish.wm.fonts#text-width::
text-width STRING [FONT]

Return the number of horizontal pixels that would be required to display
the text STRING using font object FONT (or the default-font).
::end:: */
{
    rep_DECLARE1(string, rep_STRINGP);
    if (font == Qnil)
	font = global_symbol_value (Qdefault_font);
    rep_DECLARE2(font, FONTP);
    return rep_MAKE_INT (x_text_width (font, rep_STR(string),
				       rep_STRING_LEN(string)));
}

DEFUN("font-height", Ffont_height, Sfont_height, (repv font), rep_Subr1) /*
::doc:sawfish.wm.fonts#font-height::
font-height [FONT]

Return the bounding height of glyphs rendered using FONT (or the
default-font).
::end:: */
{
    if (font == Qnil)
	font = global_symbol_value (Qdefault_font);
    rep_DECLARE1(font, FONTP);
    return rep_MAKE_INT(VFONT(font)->ascent + VFONT(font)->descent);
}

DEFUN("font-ascent", Ffont_ascent, Sfont_ascent, (repv font), rep_Subr1) /*
::doc:sawfish.wm.fonts#font-ascent::
font-ascent [FONT]

Return the ascent of glyphs rendered using FONT (or the
default-font).
::end:: */
{
    if (font == Qnil)
	font = global_symbol_value (Qdefault_font);
    rep_DECLARE1(font, FONTP);
    return rep_MAKE_INT(VFONT(font)->ascent);
}

DEFUN("font-descent", Ffont_descent, Sfont_descent, (repv font), rep_Subr1) /*
::doc:sawfish.wm.fonts#font-descent::
font-descent [FONT]

Return the descent of glyphs rendered using FONT (or the
default-font).
::end:: */
{
    if (font == Qnil)
	font = global_symbol_value (Qdefault_font);
    rep_DECLARE1(font, FONTP);
    return rep_MAKE_INT(VFONT(font)->descent);
}


/* type hooks */

static int
font_cmp (repv w1, repv w2)
{
    return w1 != w2;
}

static void
font_prin (repv stream, repv obj)
{
    char buf[256];
    sprintf (buf, "#<font %s:%s>",
	     rep_STR(VFONT(obj)->type), rep_STR(VFONT(obj)->name));
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
font_mark (repv obj)
{
    rep_MARKVAL(VFONT(obj)->type);
    rep_MARKVAL(VFONT(obj)->name);
    rep_MARKVAL(VFONT(obj)->plist);
}

static void
font_sweep (void)
{
    Lisp_Font *w = font_list;
    font_list = 0;
    while (w != 0)
    {
	Lisp_Font *next = w->next;

	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
	    (*w->class->finalize) (w);
	    rep_FREE_CELL(w);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = font_list;
	    font_list = w;
	}
	w = next;
    }
}


/* initialisation */

void
fonts_init (void)
{
    repv tem = rep_push_structure ("sawfish.wm.fonts");
    font_type = rep_register_new_type ("font", font_cmp, font_prin, font_prin,
				       font_sweep, font_mark,
				       0, 0, 0, 0, 0, 0, 0);

    rep_ADD_SUBR(Sfont_type_exists_p);
    rep_ADD_SUBR(Sget_font_typed);
    rep_ADD_SUBR(Sget_font);
    rep_ADD_SUBR(Sfont_get);
    rep_ADD_SUBR(Sfont_put);
    rep_ADD_SUBR(Sfont_type);
    rep_ADD_SUBR(Sfont_name);
    rep_ADD_SUBR(Sfontp);
    rep_ADD_SUBR(Stext_width);
    rep_ADD_SUBR(Sfont_height);
    rep_ADD_SUBR(Sfont_ascent);
    rep_ADD_SUBR(Sfont_descent);

    rep_INTERN_SPECIAL(default_font);
    if (!batch_mode_p ())
    {
	DEFSTRING (type, "xlfd");
	DEFSTRING (name, "fixed");

	repv font = Fget_font_typed (rep_VAL (&type), rep_VAL (&name));
	if (font == rep_NULL || !FONTP(font))
	{
	    fputs ("can't load 'fixed' font during initialisation", stderr);
	    rep_throw_value = rep_NULL;
	    font = Qnil;
	}
	Fset (Qdefault_font, font);
    }
    rep_pop_structure (tem);
}

void
fonts_kill (void)
{
}
