/* functions.c -- useful window manager Lisp functions
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
#include <alloca.h>

/* Number of outstanding server grabs made; only when this is zero is
   the server ungrabbed. */
static int server_grabs;

DEFSYM(root, "root");

DEFUN_INT("raise-window", Fraise_window, Sraise_window, (repv win), rep_Subr1, "f") /*
::doc:Sraise-window::
raise-window WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->mapped)
	XRaiseWindow (dpy, VWIN(win)->frame);
    return win;
}

DEFUN_INT("lower-window", Flower_window, Slower_window, (repv win), rep_Subr1, "f") /*
::doc:Slower-window::
lower-window WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->mapped)
	XLowerWindow (dpy, VWIN(win)->frame);
    return win;
}

DEFUN_INT("circulate-up", Fcirculate_up, Scirculate_up, (void), rep_Subr0, "") /*
::doc:Scirculate-up::
circulate-up
::end:: */
{
    XCirculateSubwindowsUp (dpy, root_window);
    return Qt;
}

DEFUN_INT("circulate-down", Fcirculate_down, Scirculate_down, (void), rep_Subr0, "") /*
::doc:Scirculate-down::
circulate-down
::end:: */
{
    XCirculateSubwindowsDown (dpy, root_window);
    return Qt;
}

DEFUN_INT("raise-lower-window", Fraise_lower_window, Sraise_lower_window,
	  (repv win), rep_Subr1, "f") /*
::doc:Sraise-lower-window::
raise-lower-window WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->mapped)
    {
	if (VWIN(win)->frame_vis == VisibilityUnobscured)
	    XLowerWindow (dpy, VWIN(win)->frame);
	else
	    XRaiseWindow (dpy, VWIN(win)->frame);
    }
    return win;
}

DEFUN_INT("delete-window", Fdelete_window, Sdelete_window, (repv win), rep_Subr1, "f") /*
::doc:Sdelete-window::
delete-window WINDOW
::end:: */
{
    Window w = x_win_from_arg (win);
    if (w == 0)
	return rep_signal_arg_error (win, 1);
    send_client_message (w, xa_wm_delete_window, last_event_time);
    return win;
}

DEFUN_INT("destroy-window", Fdestroy_window, Sdestroy_window, (repv win), rep_Subr1, "f") /*
::doc:Sdestroy-window::
destroy-window WINDOW
::end:: */
{
    if (WINDOWP(win))
	XKillClient (dpy, VWIN(win)->id);
    else if (rep_INTP(win))
	XDestroyWindow (dpy, rep_INT(win));
    else
	return rep_signal_arg_error (win, 1);
    return win;
}

DEFUN("warp-cursor", Fwarp_cursor, Swarp_cursor, (repv x, repv y), rep_Subr2) /*
::doc:Swarp-cursor::
warp-cursor X Y
::end:: */
{
    rep_DECLARE1(x, rep_INTP);
    rep_DECLARE2(y, rep_INTP);
    if (rep_INT(x) >= 0 && rep_INT(x) < screen_width
	&& rep_INT(y) >= 0 && rep_INT(y) < screen_height)
    {
	XWarpPointer (dpy, None, root_window,
		      0, 0, 0, 0, rep_INT(x), rep_INT(y));
	return Qt;
    }
    else
	return Qnil;
}

DEFUN("warp-cursor-to-window", Fwarp_cursor_to_window, Swarp_cursor_to_window,
      (repv win, repv x, repv y), rep_Subr3) /*
::doc:Swarp-cursor-to-window::
warp-cursor-to-window WINDOW [X Y]
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->mapped)
    {
	int dest_x = 0, dest_y = 0;
	if (rep_INTP(x))
	    dest_x = rep_INT(x);
	if (rep_INTP(y))
	    dest_y = rep_INT(y);
	XWarpPointer (dpy, None, VWIN(win)->id, 0, 0, 0, 0, dest_x, dest_y);
	return win;
    }
    else
	return Qnil;
}

DEFUN("move-window-to", Fmove_window_to, Smove_window_to,
      (repv win, repv x, repv y), rep_Subr3) /*
::doc:Smove-window-to::
move-window-to WINDOW X Y
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    rep_DECLARE2(x, rep_INTP);
    rep_DECLARE3(y, rep_INTP);
    if (VWIN(win)->attr.x != rep_INT(x) || VWIN(win)->attr.y != rep_INT(y))
    {
	VWIN(win)->attr.x = rep_INT(x);
	VWIN(win)->attr.y = rep_INT(y);
	XMoveWindow (dpy,
		     VWIN(win)->reparented ? VWIN(win)->frame : VWIN(win)->id,
		     VWIN(win)->attr.x, VWIN(win)->attr.y);
	send_synthetic_configure (VWIN(win));
    }
    return win;
}

DEFUN("resize-window-to", Fresize_window_to, Sresize_window_to,
      (repv win, repv width, repv height), rep_Subr3) /*
::doc:Sresize-window-to::
resize-window-to WINDOW WIDTH HEIGHT
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    rep_DECLARE2(width, rep_INTP);
    rep_DECLARE3(height, rep_INTP);
    VWIN(win)->attr.width = rep_INT(width);
    VWIN(win)->attr.height = rep_INT(height);
    fix_window_size (VWIN(win));
    return win;
}

DEFUN("grab-server", Fgrab_server, Sgrab_server, (void), rep_Subr0) /*
::doc:Sgrab-server::
grab-server
::end:: */
{
    if (server_grabs++ == 0)
    {
	XGrabServer (dpy);
	XFlush (dpy);
    }
    return Qt;
}

DEFUN("ungrab-server", Fungrab_server, Sungrab_server, (void), rep_Subr0) /*
::doc:Sungrab-server::
ungrab-server
::end:: */
{
    if (--server_grabs == 0)
    {
	XUngrabServer (dpy);
	XFlush (dpy);
    }
    return Qt;
}

DEFUN("grab-pointer", Fgrab_pointer, Sgrab_pointer,
      (repv win, repv cursor), rep_Subr2) /*
::doc:Sgrab-pointer::
grab-pointer WINDOW [CURSOR]
::end:: */
{
    Window g_win;
    rep_DECLARE1(win, WINDOWP);
    g_win = VWIN(win)->frame;
    if (current_x_event)
    {
	/* XXX This is a hack. If we're being called from an event
	   XXX then assume that the originating window is where we
	   XXX want all following events to end up. This helps
	   XXX frame parts to be ``un-clicked'' */

	struct frame_part *fp
	    = find_frame_part_by_window (current_x_event->xany.window);
	if (fp != 0)
	    g_win = fp->id;
    }
    if (XGrabPointer (dpy, g_win, False,
		      ButtonPressMask | ButtonReleaseMask
		      | PointerMotionMask | PointerMotionHintMask,
		      GrabModeAsync, GrabModeAsync,
		      None,
		      CURSORP(cursor) ? VCURSOR(cursor)->cursor : None,
		      CurrentTime) == GrabSuccess)
    {
	return Qt;
    }
    else
	return Qnil;
}

DEFUN("ungrab-pointer", Fungrab_pointer, Sungrab_pointer, (void), rep_Subr0) /*
::doc:Sungrab-pointer::
ungrab-pointer
::end:: */
{
    XUngrabPointer (dpy, last_event_time);
    return Qt;
}


/* Drawing window outlines */

static void
draw_box_outline (int x, int y, int width, int height)
{
    static GC gc;
    XSegment lines[8];
    int i;

    if (gc == 0)
    {
	long black = BlackPixel (dpy, screen_num);
	long white = WhitePixel (dpy, screen_num);
	XGCValues gcv;
	gcv.line_width = 0;
	/* I don't understand this, but it works */
	gcv.function = GXxor;
	gcv.foreground = black ^ white;
	gcv.plane_mask = black ^ white;
	gcv.subwindow_mode = IncludeInferiors;
	gc = XCreateGC (dpy, root_window,
			GCFunction | GCForeground
			| GCSubwindowMode | GCLineWidth | GCPlaneMask, &gcv);
    }

    for (i = 0; i < 4; i++)
    {
	lines[i].x1 = x;
	lines[i].x2 = x + width;
	lines[i].y1 = lines[i].y2 = y + (i * height) / 3;
    }
    for (i = 4; i < 8; i++)
    {
	lines[i].y1 = y;
	lines[i].y2 = y + height;
	lines[i].x1 = lines[i].x2 = x + ((i-4) * width) / 3;
    }
    XDrawSegments(dpy, root_window, gc, lines, 8);
}

DEFUN("draw-window-outline", Fdraw_window_outline, Sdraw_window_outline,
      (repv mode, repv x, repv y, repv width, repv height), rep_Subr5) /*
::doc:Sdraw-window-outline::
draw-window-outline MODE X Y WIDTH HEIGHT
::end:: */
{
    rep_DECLARE2(x, rep_INTP);
    rep_DECLARE3(y, rep_INTP);
    rep_DECLARE4(width, rep_INTP);
    rep_DECLARE5(height, rep_INTP);
    draw_box_outline (rep_INT(x), rep_INT(y), rep_INT(width), rep_INT(height));
    return Qt;
}

DEFUN("erase-window-outline", Ferase_window_outline, Serase_window_outline,
      (repv mode, repv x, repv y, repv width, repv height), rep_Subr5) /*
::doc:Serase-window-outline::
erase-window-outline MODE X Y WIDTH HEIGHT
::end:: */
{
    rep_DECLARE2(x, rep_INTP);
    rep_DECLARE3(y, rep_INTP);
    rep_DECLARE4(width, rep_INTP);
    rep_DECLARE5(height, rep_INTP);
    draw_box_outline (rep_INT(x), rep_INT(y), rep_INT(width), rep_INT(height));
    return Qt;
}

DEFUN("screen-width", Fscreen_width, Sscreen_width, (void), rep_Subr0) /*
::doc:Sscreen-width::
screen-width
::end:: */
{
    return rep_MAKE_INT(screen_width);
}

DEFUN("screen-height", Fscreen_height, Sscreen_height, (void), rep_Subr0) /*
::doc:Sscreen-height::
screen-height
::end:: */
{
    return rep_MAKE_INT(screen_height);
}

DEFUN("sync-server", Fsync_server, Ssync_server, (void), rep_Subr0) /*
::doc:Ssync-server::
sync-server
::end:: */
{
    XFlush (dpy);
    return Qt;
}

DEFUN("delete-x-property", Fdelete_x_property, Sdelete_x_property,
      (repv win, repv atom), rep_Subr2) /*
::doc:Sdelete-x-property::
delete-x-property WINDOW ATOM
::end:: */
{
    Window w = x_win_from_arg (win);
    if (w == 0)
	return rep_signal_arg_error (win, 1);
    rep_DECLARE2(atom, rep_SYMBOLP);
    XDeleteProperty (dpy, w,
		     XInternAtom (dpy, rep_STR(rep_SYM(atom)->name), False));
    return atom;
}

DEFUN("list-x-properties", Flist_x_properties, Slist_x_properties,
      (repv win), rep_Subr1) /*
::doc:Slist-x-properties::
list-x-properties WINDOW
::end:: */
{
    Window w;
    Atom *atoms;
    int count;
    repv ret = Qnil;

    w = x_win_from_arg (win);
    if (w == 0)
	return rep_signal_arg_error (win, 1);
    atoms = XListProperties (dpy, w, &count);
    if (atoms != 0)
    {
	char **names = alloca (sizeof (char *) * count);
	if (XGetAtomNames (dpy, atoms, count, names) != 0)
	{
	    int i;
	    for (i = 0; i < count; i++)
	    {
		ret = Fcons (Fintern (rep_string_dup (names[i]),
				      rep_obarray), ret);
		XFree (names[i]);
	    }
	}
	XFree (atoms);
    }
    return Fnreverse (ret);
}

DEFUN("get-x-property", Fget_x_property, Sget_x_property,
      (repv win, repv prop), rep_Subr2) /*
::doc:Sget-x-property::
get-x-property WINDOW PROPERTY
::end:: */
{
    Window w;
    Atom a_prop;
    Atom type;
    int format;
    u_long nitems;
    u_char *data = 0;
    repv type_sym, ret_data = Qnil;

    w = x_win_from_arg (win);
    if (w == 0)
	return rep_signal_arg_error (win, 1);
    rep_DECLARE2(prop, rep_SYMBOLP);
    a_prop = XInternAtom (dpy, rep_STR(rep_SYM(prop)->name), False);

    /* First read the data.. */
    {
	long long_length = 32;
	u_long bytes_after;
	while (1)
	{
	    if (data != 0)
		XFree (data);
	    if (XGetWindowProperty (dpy, w, a_prop, 0, long_length, False,
				    AnyPropertyType, &type, &format,
				    &nitems, &bytes_after, &data) != Success)
		return Qnil;
	    if (bytes_after == 0)
		break;
	    long_length += (bytes_after / 4) + 1;
	}
    }

    /* Convert the type to a symbol */
    type_sym = x_atom_symbol (type);
    
    /* Then convert the contents to a vector or string */
    switch (format)
    {
	/* XXX assumes 32 bit ints, 16 bit shorts.. */
	u_short *s_data;
	u_long *l_data;
	int i;

    case 8:
	ret_data = rep_string_dupn (data, nitems);
	break;

    case 16:
	ret_data = Fmake_vector (rep_MAKE_INT(nitems), Qnil);
	s_data = (u_short *)data;
	for (i = 0; i < nitems; i++)
	    rep_VECTI(ret_data, i) = rep_MAKE_INT(s_data[i]);
	break;

    case 32:
	ret_data = Fmake_vector (rep_MAKE_INT(nitems), Qnil);
	l_data = (u_long *)data;
	for (i = 0; i < nitems; i++)
	{
	    repv name;
	    if (type == XA_ATOM && (name = x_atom_symbol (l_data[i])) != Qnil)
		rep_VECTI(ret_data, i) = name;
	    else
		rep_VECTI(ret_data, i) = rep_MAKE_INT(l_data[i]);
	}
	break;
    }
    XFree (data);

    return rep_list_3 (type_sym, rep_MAKE_INT(format), ret_data);
}

DEFUN("set-x-property", Fset_x_property, Sset_x_property,
      (repv win, repv prop, repv data, repv type, repv format), rep_Subr5) /*
::doc:Sset-x-property::
set-x-property WINDOW PROPERTY DATA TYPE FORMAT
::end:: */
{
    Window w;
    Atom a_prop, a_type;
    u_long nitems;
    u_char *c_data = 0;

    w = x_win_from_arg (win);
    if (w == 0)
	return rep_signal_arg_error (win, 1);
    rep_DECLARE2(prop, rep_SYMBOLP);
    a_prop = XInternAtom (dpy, rep_STR(rep_SYM(prop)->name), False);
    rep_DECLARE(3, data, rep_VECTORP(data) || rep_STRINGP(data));
    rep_DECLARE4(type, rep_SYMBOLP);
    a_type = XInternAtom (dpy, rep_STR(rep_SYM(type)->name), False);
    rep_DECLARE5(format, rep_INTP);

    /* Convert to data array */

    if (rep_STRINGP(data))
	nitems = rep_STRING_LEN(data);
    else
	nitems = rep_VECT_LEN(data);

    switch (rep_INT(format))
    {
	int i;
	u_short *s_data;
	u_long *l_data;

    case 8:
	if (rep_STRINGP(data))
	    c_data = rep_STR (data);
	else
	{
	    c_data = alloca (nitems);
	    for (i = 0; i < nitems; i++)
		c_data[i] = rep_STR(data)[i];
	}
	break;

    case 16:
	if (rep_STRINGP(data))
	    return rep_signal_arg_error (data, 3);
	c_data = alloca (nitems * 2);
	s_data = (u_short *)c_data;
	for (i = 0; i < nitems; i++)
	    s_data[i] = rep_INT(rep_VECTI(data, i));
	break;

    case 32:
	if (rep_STRINGP(data))
	    return rep_signal_arg_error (data, 3);
	c_data = alloca (nitems * 4);
	l_data = (u_long *)c_data;
	for (i = 0; i < nitems; i++)
	{
	    if (a_type == XA_ATOM && rep_SYMBOLP(rep_VECTI(data, i)))
		l_data[i] = XInternAtom (dpy, rep_STR(rep_SYM(rep_VECTI(data, i))->name), False);
	    else
		l_data[i] = rep_INT(rep_VECTI(data, i));
	}
	break;
    }

    /* Overwrite property */
    XChangeProperty (dpy, w, a_prop, a_type, rep_INT(format),
		     PropModeReplace, c_data, nitems);
    return prop;
}

DEFUN("send-client-message", Fsend_client_message, Ssend_client_message,
      (repv win, repv atom), rep_Subr2) /*
::doc:Ssend-client-message::
send-client-message WINDOW ATOM

Send an X ClientMessage event to WINDOW (a window object or the symbol
`root'). It will contain the atom ATOM.
::end:: */
{
    /* XXX add option to control timestamp? */
    Window w = x_win_from_arg (win);
    if (w == 0)
	return rep_signal_arg_error (win, 1);
    rep_DECLARE2(atom, rep_SYMBOLP);
    send_client_message (w, XInternAtom (dpy, rep_STR(rep_SYM(atom)->name),
					 False), CurrentTime);
    return atom;
}

DEFUN("create-window", Fcreate_window, Screate_window,
      (repv parent, repv x, repv y, repv width, repv height), rep_Subr5) /*
::doc:Screate-window::
create-window PARENT-WINDOW X Y WIDTH HEIGHT

Create an unmapped window that is a child of PARENT-WINDOW (a window object,
an integer window id, or the symbol `root'), with the specified dimensions.

Returns the window id of the new window.
::end:: */
{
    Window parent_w = x_win_from_arg (parent);
    Window id;
    if (parent_w == 0)
	return rep_signal_arg_error (parent, 1);
    rep_DECLARE2(x, rep_INTP);
    rep_DECLARE3(y, rep_INTP);
    rep_DECLARE4(width, rep_INTP);
    rep_DECLARE5(height, rep_INTP);
    id = XCreateSimpleWindow (dpy, parent_w, rep_INT(x), rep_INT(y),
			      rep_INT(width), rep_INT(height),
			      0, BlackPixel (dpy, screen_num),
			      WhitePixel (dpy, screen_num));
    return id ? rep_MAKE_INT(id) : Qnil;
}

DEFUN("x-atom", Fx_atom, Sx_atom, (repv symbol), rep_Subr1) /*
::doc:Sx-atom::
x-atom SYMBOL

Return the integer identifying the X atom with the same name as SYMBOL.
::end:: */
{
    rep_DECLARE1(symbol, rep_SYMBOLP);
    return rep_MAKE_INT (XInternAtom (dpy, rep_STR(rep_SYM(symbol)->name),
				      False));
}

DEFUN("x-atom-name", Fx_atom_name, Sx_atom_name, (repv atom), rep_Subr1) /*
::doc:Sx-atom-name::
x-atom-name ATOM

Return the symbol with the same name as the X atom identified by the
integer ATOM.
::end:: */
{
    rep_DECLARE1(atom, rep_INTP);
    return x_atom_symbol (rep_INT(atom));
}


/* initialisation */

void
functions_init (void)
{
    rep_ADD_SUBR_INT(Sraise_window);
    rep_ADD_SUBR_INT(Slower_window);
    rep_ADD_SUBR_INT(Scirculate_up);
    rep_ADD_SUBR_INT(Scirculate_down);
    rep_ADD_SUBR_INT(Sraise_lower_window);
    rep_ADD_SUBR_INT(Sdelete_window);
    rep_ADD_SUBR_INT(Sdestroy_window);
    rep_ADD_SUBR(Swarp_cursor);
    rep_ADD_SUBR(Swarp_cursor_to_window);
    rep_ADD_SUBR(Smove_window_to);
    rep_ADD_SUBR(Sresize_window_to);
    rep_ADD_SUBR(Sgrab_server);
    rep_ADD_SUBR(Sungrab_server);
    rep_ADD_SUBR(Sgrab_pointer);
    rep_ADD_SUBR(Sungrab_pointer);
    rep_ADD_SUBR(Sdraw_window_outline);
    rep_ADD_SUBR(Serase_window_outline);
    rep_ADD_SUBR(Sscreen_width);
    rep_ADD_SUBR(Sscreen_height);
    rep_ADD_SUBR(Ssync_server);
    rep_ADD_SUBR(Sdelete_x_property);
    rep_ADD_SUBR(Slist_x_properties);
    rep_ADD_SUBR(Sget_x_property);
    rep_ADD_SUBR(Sset_x_property);
    rep_ADD_SUBR(Ssend_client_message);
    rep_ADD_SUBR(Screate_window);
    rep_ADD_SUBR(Sx_atom);
    rep_ADD_SUBR(Sx_atom_name);
    rep_INTERN(root);
}
