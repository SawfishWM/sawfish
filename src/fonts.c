/* fonts.c -- font manipulation
   $Id$ */

#include "sawmill.h"

static Lisp_Font *font_list;
int font_type;

DEFSYM(default_font, "default-font");

DEFUN("get-font", Fget_font, Sget_font, (repv name), rep_Subr1) /*
::doc:Sget-font::
get-font NAME
::end:: */
{
    Lisp_Font *f;
    rep_DECLARE1(name, rep_STRINGP);

    f = font_list;
    while (f != 0 && strcmp (rep_STR(name), rep_STR(f->name)) != 0)
	f = f->next;
    if (f == 0)
    {
	XFontStruct *font = XLoadQueryFont (dpy, rep_STR(name));
	if (font != 0)
	{
	    f = rep_ALLOC_CELL(sizeof(Lisp_Font));
	    f->car = font_type;
	    f->next = font_list;
	    font_list = f;
	    f->name = name;
	    f->font = font;
	    f->plist = Qnil;
	}
	else
	{
	    return Fsignal (Qerror, rep_list_2 (rep_string_dup("no such font"),
						name));
	}
    }
    return rep_VAL(f);
}

DEFUN("font-get", Ffont_get, Sfont_get, (repv win, repv prop), rep_Subr2) /*
::doc::Sfont-get::
font-get FONT PROPERTY
::end:: */
{
    repv plist;
    rep_DECLARE1(win, FONTP);
    plist = VFONT(win)->plist;
    while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist)))
    {
	if (rep_CAR(plist) == prop)
	    return rep_CAR(rep_CDR(plist));
	plist = rep_CDR(rep_CDR(plist));
    }
    return Qnil;
}

DEFUN("font-put", Ffont_put, Sfont_put, (repv win, repv prop, repv val), rep_Subr3) /*
::doc:Sfont-put::
font-put FONT PROPERTY VALUE
::end:: */
{
    repv plist;
    rep_DECLARE1(win, FONTP);
    plist = VFONT(win)->plist;
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
    plist = Fcons(prop, Fcons(val, VFONT(win)->plist));
    if (plist != rep_NULL)
	VFONT(win)->plist = plist;
    return val;
}


/* type hooks */

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
	    XFreeFont (dpy, w->font);
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
    font_type = rep_register_new_type ("font", 0, font_prin, font_prin,
				       font_sweep, font_mark,
				       0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sget_font);
    rep_ADD_SUBR(Sfont_get);
    rep_ADD_SUBR(Sfont_put);
    rep_INTERN(default_font);
    rep_SYM(Qdefault_font)->value = Fget_font (rep_string_dup("fixed"));
}

void
fonts_kill (void)
{
}
