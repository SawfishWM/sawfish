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

#include "sawmill.h"

static Lisp_Font *font_list;
int font_type;

DEFSYM(default_font, "default-font");

DEFUN("get-font", Fget_font, Sget_font, (repv name), rep_Subr1) /*
::doc:get-font::
get-font NAME

Return the font object representing the font named NAME (a standard X
font specifier string).
::end:: */
{
    Lisp_Font *f;
    rep_DECLARE1(name, rep_STRINGP);

    if (dpy == 0)
	return Qnil;

    f = font_list;
    while (f != 0 && strcmp (rep_STR(name), rep_STR(f->name)) != 0)
	f = f->next;
    if (f == 0)
    {
	XFontSet font_set;
	XFontStruct *font_struct = 0;
	int ascent, descent;
	char **missing_charset_list, *def_string;
	int num_missing_charset_list;
	font_set = XCreateFontSet (dpy, rep_STR(name), &missing_charset_list,
				   &num_missing_charset_list, &def_string);
	if (font_set != 0)
	{
	    XFontStruct **fstrs;
	    char **font_names;
	    int i, num_fonts;
	    num_fonts = XFontsOfFontSet (font_set, &fstrs, &font_names);
	    ascent = descent = 0;
	    for (i = 0; i < num_fonts; i++)
	    {
		if (fstrs[i]->ascent > ascent)
		    ascent = fstrs[i]->ascent;
		if (fstrs[i]->descent > descent)
		    descent = fstrs[i]->descent;
	    }
	    if (num_missing_charset_list > 0)
	    {
		int i;
		fprintf (stderr, "Missing charsets in FontSet creation\n");
		for (i = 0; i < num_missing_charset_list; i++)
		    fprintf (stderr, "\t%s\n", missing_charset_list[i]);
		XFreeStringList (missing_charset_list);
	    }
	}
	else
	{
	    /* can't load a FontSet, try falling back to a FontStruct */

	    font_struct = XLoadQueryFont (dpy, rep_STR (name));
	    if (font_struct != 0)
	    {
		ascent = font_struct->ascent;
		descent = font_struct->descent;
	    }
	    else
	    {
		return Fsignal (Qerror,
				rep_list_2 (rep_string_dup("no such font"),
					    name));
	    }
	}

	f = rep_ALLOC_CELL(sizeof(Lisp_Font));
	rep_data_after_gc += sizeof (Lisp_Font);
	f->car = font_type;
	f->next = font_list;
	font_list = f;
	f->name = name;
	f->plist = Qnil;
	f->ascent = ascent;
	f->descent = 0;
	if (font_set != 0)
	    f->font.set = font_set;
	else
	{
	    f->font.str = font_struct;
	    f->car |= FF_FONT_STRUCT;
	}
    }
    return rep_VAL(f);
}

DEFUN("font-get", Ffont_get, Sfont_get, (repv win, repv prop), rep_Subr2) /*
::doc::Sfont-get::
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
::doc:font-put::
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

DEFUN("font-name", Ffont_name, Sfont_name, (repv font), rep_Subr1) /*
::doc:font-name::
font-name FONT

Return the name of the font represented by the font object FONT.
::end:: */
{
    rep_DECLARE1(font, FONTP);
    return VFONT(font)->name;
}

DEFUN("fontp", Ffontp, Sfontp, (repv win), rep_Subr1) /*
::doc:fontp::
fontp ARG

Return t if ARG is a font object.
::end:: */
{
    return FONTP(win) ? Qt : Qnil;
}

int
x_text_width (repv font, u_char *string, size_t len)
{
    if (FONT_STRUCT_P (font))
	return XTextWidth (VFONT(font)->font.str, string, len);
    else
	return XmbTextEscapement (VFONT(font)->font.set, string, len);
}

void
x_draw_string (Window id, repv font, GC gc,
	       int x, int y, u_char *string, size_t len)
{
    if (FONT_STRUCT_P (font))
    {
	XSetFont (dpy, gc, VFONT(font)->font.str->fid);
	XDrawString (dpy, id, gc, x, y, string, len);
    }
    else
	XmbDrawString (dpy, id, VFONT(font)->font.set, gc, x, y, string, len);
}

DEFUN("text-width", Ftext_width, Stext_width, (repv string, repv font), rep_Subr2) /*
::doc:text-width::
text-width STRING [FONT]

Return the number of horizontal pixels that would be required to display
the text STRING using font object FONT (or the default-font).
::end:: */
{
    rep_DECLARE1(string, rep_STRINGP);
    if (font == Qnil)
	font = Fsymbol_value (Qdefault_font, Qt);
    rep_DECLARE2(font, FONTP);
    return rep_MAKE_INT (x_text_width (font, rep_STR(string),
				       rep_STRING_LEN(string)));
}

DEFUN("font-height", Ffont_height, Sfont_height, (repv font), rep_Subr1) /*
::doc:font-height::
font-height [FONT]

Return the bounding height of glyphs rendered using FONT (or the
default-font).
::end:: */
{
    if (font == Qnil)
	font = Fsymbol_value (Qdefault_font, Qt);
    rep_DECLARE1(font, FONTP);
    return rep_MAKE_INT(VFONT(font)->ascent + VFONT(font)->descent);
}

DEFUN("font-ascent", Ffont_ascent, Sfont_ascent, (repv font), rep_Subr1) /*
::doc:font-ascent::
font-ascent [FONT]

Return the ascent of glyphs rendered using FONT (or the
default-font).
::end:: */
{
    if (font == Qnil)
	font = Fsymbol_value (Qdefault_font, Qt);
    rep_DECLARE1(font, FONTP);
    return rep_MAKE_INT(VFONT(font)->ascent);
}

DEFUN("font-descent", Ffont_descent, Sfont_descent, (repv font), rep_Subr1) /*
::doc:font-descent::
font-descent [FONT]

Return the descent of glyphs rendered using FONT (or the
default-font).
::end:: */
{
    if (font == Qnil)
	font = Fsymbol_value (Qdefault_font, Qt);
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
    sprintf (buf, "#<font %s>", rep_STR(VFONT(obj)->name));
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
font_mark (repv obj)
{
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
	    if (FONT_STRUCT_P (rep_VAL (w)))
		XFreeFont (dpy, w->font.str);
	    else
		XFreeFontSet (dpy, w->font.set);
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
    font_type = rep_register_new_type ("font", font_cmp, font_prin, font_prin,
				       font_sweep, font_mark,
				       0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sget_font);
    rep_ADD_SUBR(Sfont_get);
    rep_ADD_SUBR(Sfont_put);
    rep_ADD_SUBR(Sfont_name);
    rep_ADD_SUBR(Sfontp);
    rep_ADD_SUBR(Stext_width);
    rep_ADD_SUBR(Sfont_height);
    rep_ADD_SUBR(Sfont_ascent);
    rep_ADD_SUBR(Sfont_descent);

    rep_INTERN_SPECIAL(default_font);
    if (rep_SYM(Qbatch_mode)->value == Qnil)
    {
	repv font = Fget_font (rep_string_dup("fixed"));
	if (font == rep_NULL || !FONTP(font))
	{
	    fputs ("can't load fixed font during initialisation", stderr);
	    rep_throw_value = rep_NULL;
	    font = Qnil;
	}
	rep_SYM(Qdefault_font)->value = font;
    }
}

void
fonts_kill (void)
{
}
