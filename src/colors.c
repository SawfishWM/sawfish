/* colors.c -- Colour handling
   $Id$ */

#include "sawmill.h"

static Lisp_Color *color_list;
int color_type;

DEFSYM(default_foreground, "default-foreground");

DEFUN("get-color", Fget_color, Sget_color, (repv name), rep_Subr1) /*
::doc:Sget-color::
get-color NAME
::end:: */
{
    Lisp_Color *f;
    rep_DECLARE1(name, rep_STRINGP);

    f = color_list;
    while (f != 0 && strcmp (rep_STR(name), rep_STR(f->name)) != 0)
	f = f->next;
    if (f == 0)
    {
	XColor screen_col, exact_col;
	if (XAllocNamedColor (dpy, screen_cmap, rep_STR(name),
			      &screen_col, &exact_col) != 0)
	{
	    f = rep_ALLOC_CELL(sizeof(Lisp_Color));
	    f->car = color_type;
	    f->next = color_list;
	    color_list = f;
	    f->name = name;
	    f->color = screen_col;
	}
	else
	{
	    return Fsignal (Qerror,
			    rep_list_2 (rep_string_dup("no such color"),
					name));
	}
    }
    return rep_VAL(f);
}


/* type hooks */

static void
color_prin (repv stream, repv obj)
{
    char buf[256];
    sprintf (buf, "#<color %s>", rep_STR(VCOLOR(obj)->name));
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
color_mark (repv obj)
{
    rep_MARKVAL(VCOLOR(obj)->name);
}

static void
color_sweep (void)
{
    Lisp_Color *w = color_list;
    color_list = 0;
    while (w != 0)
    {
	Lisp_Color *next = w->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
	    XFreeColors (dpy, screen_cmap, &w->color.pixel, 1, 0);
	    rep_FREE_CELL(w);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = color_list;
	    color_list = w;
	}
	w = next;
    }
}


/* initialisation */

void
colors_init (void)
{
    color_type = rep_register_new_type ("color", 0, color_prin, color_prin,
					color_sweep, color_mark,
					0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sget_color);
    rep_INTERN(default_foreground);
    rep_SYM(Qdefault_foreground)->value = Fget_color (rep_string_dup("black"));
}

void
colors_kill (void)
{
}
