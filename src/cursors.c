/* cursor.c -- Cursor handling
   $Id$ */

#include "sawmill.h"
#include <X11/cursorfont.h>

static Lisp_Cursor *cursor_list;
int cursor_type;

repv default_cursor;
DEFSYM(cursor_shape, "cursor-shape");

DEFUN("get-cursor", Fget_cursor, Sget_cursor, (repv data), rep_Subr1) /*
::doc:Sget-cursor::
get-cursor DATA
::end:: */
{
    Lisp_Cursor *f;

    f = cursor_list;
    while (f != 0 && f->data != data)
	f = f->next;
    if (f == 0)
    {
	Cursor cursor = 0;
	if (rep_SYMBOLP(data))
	    data = Fget (data, Qcursor_shape);
	if (rep_INTP(data))
	    cursor = XCreateFontCursor (dpy, rep_INT(data));
	else if (IMAGEP(data))
	{
	    /* XXX implement pixmap cursors.. */
	}
	if (cursor != 0)
	{
	    f = rep_ALLOC_CELL(sizeof(Lisp_Cursor));
	    f->car = cursor_type;
	    f->next = cursor_list;
	    cursor_list = f;
	    f->data = data;
	    f->cursor = cursor;
	}
	else
	{
	    return Fsignal (Qerror,
			    rep_list_2 (rep_string_dup("no such cursor"),
					data));
	}
    }
    return rep_VAL(f);
}

DEFUN("default-cursor", Vdefault_cursor, Sdefault_cursor, (repv arg), rep_Var)
{
    if (arg && CURSORP(arg))
    {
	default_cursor = arg;
	XDefineCursor (dpy, root_window, VCURSOR(default_cursor)->cursor);
    }
    return default_cursor;
}


/* type hooks */

static void
cursor_prin (repv stream, repv obj)
{
    char buf[256];
    sprintf (buf, "#<cursor %x>", (u_int) VCURSOR(obj)->cursor);
    rep_stream_puts (stream, buf, -1, FALSE);
}

static void
cursor_mark (repv obj)
{
    rep_MARKVAL(VCURSOR(obj)->data);
}

static void
cursor_sweep (void)
{
    Lisp_Cursor *w = cursor_list;
    cursor_list = 0;
    while (w != 0)
    {
	Lisp_Cursor *next = w->next;
	if (!rep_GC_CELL_MARKEDP(rep_VAL(w)))
	{
	    XFreeCursor (dpy, w->cursor);
	    rep_FREE_CELL(w);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(w));
	    w->next = cursor_list;
	    cursor_list = w;
	}
	w = next;
    }
}


/* initialisation */

void
cursors_init (void)
{
    cursor_type = rep_register_new_type ("cursor", 0, cursor_prin, cursor_prin,
					 cursor_sweep, cursor_mark,
					 0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sget_cursor);
    rep_ADD_SUBR(Sdefault_cursor);
    Vdefault_cursor (Fget_cursor (rep_MAKE_INT (XC_left_ptr)));
    rep_mark_static (&default_cursor);
    rep_INTERN(cursor_shape);
}

void
cursors_kill (void)
{
}
