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
#include <X11/Xproto.h>

/* Number of outstanding server grabs made; only when this is zero is
   the server ungrabbed. */
static int server_grabs;

DEFSYM(root, "root");
DEFSYM(after_restacking_hook, "after-restacking-hook");

DEFUN("restack-windows", Frestack_windows, Srestack_windows,
      (repv list), rep_Subr1) /*
::doc:Srestack-windows::
restack-windows LIST

Restack all windows in the list of windows LIST in the order they occur
in the list (from top to bottom). The stacking order of any unspecified
windows isn't affected.
::end:: */
{
    int len, i, j;
    Window *frames;
    rep_DECLARE1(list, rep_LISTP);
    len = rep_INT(Flength (list));
    frames = alloca (len * sizeof (Window));
    for (i = j = 0; i < len; i++)
    {
	repv w = rep_CAR(list);
	if (WINDOWP(w) && (VWIN(w)->frame != 0 || VWIN(w)->id != 0))
	{
	    frames[j++] = (VWIN(w)->reparented
			   ? VWIN(w)->frame : VWIN(w)->id);
	}
	list = rep_CDR(list);
    }
    XRestackWindows (dpy, frames, j);
    Fcall_hook (Qafter_restacking_hook, Qnil, Qnil);
    return Qt;
}

DEFUN("x-raise-window", Fx_raise_window, Sx_raise_window,
      (repv win), rep_Subr1) /*
::doc:Sx-raise-window::
x-raise-window WINDOW

Bring WINDOW to the top of the display.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->reparented)
    {
       XRaiseWindow (dpy, VWIN(win)->frame);
       Fcall_hook (Qafter_restacking_hook, Qnil, Qnil);
    }
    return win;
}

DEFUN_INT("delete-window", Fdelete_window, Sdelete_window, (repv win), rep_Subr1, "%W") /*
::doc:Sdelete-window::
delete-window WINDOW

Delete WINDOW, i.e. send a WM_DELETE_WINDOW client-message if possible, or
just kill the owning client if not.

WINDOW may be a window object or a numeric window id.
::end:: */
{
    Window w;

    if (WINDOWP(win))
    {
	/* In case it's late in setting WM_DELETE_WINDOW */
	get_window_protocols (VWIN(win));
    }

    w = x_win_from_arg (win);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);
    if (WINDOWP(win) && VWIN(win)->does_wm_delete_window)
	send_client_message (w, xa_wm_delete_window, last_event_time);
    else
	XKillClient (dpy, w);
    return win;
}

DEFUN_INT("destroy-window", Fdestroy_window, Sdestroy_window, (repv win), rep_Subr1, "%W") /*
::doc:Sdestroy-window::
destroy-window WINDOW

Destroy WINDOW with out giving the owning application any warning.

WINDOW may be a window object or a numeric window id.
::end:: */
{
    if (WINDOWP(win))
	XDestroyWindow (dpy, VWIN(win)->id);
    else if (rep_INTP(win))
	XDestroyWindow (dpy, rep_INT(win));
    else
	return rep_signal_arg_error (win, 1);
    return win;
}

DEFUN("warp-cursor", Fwarp_cursor, Swarp_cursor, (repv x, repv y), rep_Subr2) /*
::doc:Swarp-cursor::
warp-cursor X Y

Move the mouse pointer to position (X, Y) relative to the origin of the
root window.
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

Move the mouse pointer to position (X, Y) relative to the client window
associated with object WINDOW.

If X and Y are nil, then they are taken as the top-left corner of the
window frame.
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    if (VWIN(win)->visible)
    {
	Window w = VWIN(win)->id;
	int dest_x = 1, dest_y = 1;
	if (!rep_INTP(x) && !rep_INTP(y) && VWIN(win)->reparented)
	    w = VWIN(win)->frame;
	else
	{
	    if (rep_INTP(x))
		dest_x = rep_INT(x);
	    if (rep_INTP(y))
		dest_y = rep_INT(y);
	}
	XWarpPointer (dpy, None, w, 0, 0, 0, 0, dest_x, dest_y);
	return win;
    }
    else
	return Qnil;
}

DEFUN("move-window-to", Fmove_window_to, Smove_window_to,
      (repv win, repv x, repv y), rep_Subr3) /*
::doc:Smove-window-to::
move-window-to WINDOW X Y

Move the top-left corner of window object WINDOW to (X, Y).
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
	Fcall_window_hook (Qwindow_moved_hook, win, Qnil, Qnil);
    }
    return win;
}

DEFUN("resize-window-to", Fresize_window_to, Sresize_window_to,
      (repv win, repv width, repv height), rep_Subr3) /*
::doc:Sresize-window-to::
resize-window-to WINDOW WIDTH HEIGHT

Set the dimensions of window object WINDOW to (WIDTH, HEIGHT).
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    rep_DECLARE2(width, rep_INTP);
    rep_DECLARE3(height, rep_INTP);
    VWIN(win)->attr.width = rep_INT(width);
    VWIN(win)->attr.height = rep_INT(height);
    fix_window_size (VWIN(win));
    Fcall_window_hook (Qwindow_resized_hook, win, Qnil, Qnil);
    return win;
}

DEFUN("grab-server", Fgrab_server, Sgrab_server, (void), rep_Subr0) /*
::doc:Sgrab-server::
grab-server

Prevent any other clients from accessing the X server. See `ungrab-server'.
::end:: */
{
    if (server_grabs++ == 0)
    {
	XGrabServer (dpy);
	XSync (dpy, False);
	rep_mark_input_pending (ConnectionNumber (dpy));
    }
    return Qt;
}

DEFUN("ungrab-server", Fungrab_server, Sungrab_server, (void), rep_Subr0) /*
::doc:Sungrab-server::
ungrab-server

After a call to `grab-server' this will allow other clients to access
the X server again.

Note that calls to `grab-server' and `ungrab-server' _nest_.
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
grab-pointer [WINDOW] [CURSOR]

Grab the mouse pointer and direct all pointer events to window object
WINDOW. If CURSOR is defined and a cursor object, display this whilst
the pointer is grabbed.

If WINDOW is nil, or unviewable, the grab will be made on the root
window.

Returns non-nil if the grab succeeded.
::end:: */
{
    Window g_win;
    int ret;

    if (WINDOWP(win) && VWIN(win)->visible)
	g_win = VWIN(win)->frame;
    else
	g_win = root_window;

again:
    ret = XGrabPointer (dpy, g_win, False, POINTER_GRAB_EVENTS,
			GrabModeAsync, GrabModeAsync, None,
			CURSORP(cursor) ? VCURSOR(cursor)->cursor : None,
			last_event_time);
    if (ret == GrabNotViewable && g_win != root_window)
    {
	/* fall back to the root window. */
	g_win = root_window;
	goto again;
    }

    DB(("grab-pointer: time=%lu ret=%d\n", last_event_time, ret));
    return (ret == GrabSuccess) ? Qt : Qnil;
}

DEFUN("ungrab-pointer", Fungrab_pointer, Sungrab_pointer, (void), rep_Subr0) /*
::doc:Sungrab-pointer::
ungrab-pointer

Release the grab on the mouse pointer.
::end:: */
{
    XUngrabPointer (dpy, last_event_time);
    synthesize_button_release ();
    DB(("ungrab-pointer: time=%lu\n", last_event_time));
    return Qt;
}

DEFUN("grab-keyboard", Fgrab_keyboard, Sgrab_keyboard, (repv win), rep_Subr1) /*
::doc:Sgrab-keyboard::
grab-keyboard [WINDOW]

Grab the keyboard and direct all keyboard events to window object
WINDOW.

If WINDOW is nil, or unviewable, the grab will be made on the root
window.

Returns non-nil if the grab succeeded.
::end:: */
{
    Window g_win;
    int ret;

    if (WINDOWP(win) && VWIN(win)->visible)
	g_win = VWIN(win)->frame;
    else
	g_win = root_window;

again:
    ret = XGrabKeyboard (dpy, g_win, False, GrabModeAsync,
			 GrabModeAsync, last_event_time);
    if (ret == GrabNotViewable && g_win != root_window)
    {
	/* fall back to the root window. */
	g_win = root_window;
	goto again;
    }

    DB(("grab-keyboard: time=%lu ret=%d\n", last_event_time, ret));
    return (ret == GrabSuccess) ? Qt : Qnil;
}
    
DEFUN("ungrab-keyboard", Fungrab_keyboard,
      Sungrab_keyboard, (void), rep_Subr0) /*
::doc:Sungrab-keyboard::
ungrab-keyboard

Release the grab on the keyboard.
::end:: */
{
    DB(("ungrab-keyboard: time=%lu\n", last_event_time));
    XUngrabKeyboard (dpy, last_event_time);
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

Draw an outline of a window of dimensions (WIDTH, HEIGHT) at position
(X, Y) relative to the root window.

MODE is a symbol defining the type of outline drawn, currently it may
only be `box' for a 3x3 grid.

Use the `erase-window-outline' to erase the grid. Also note that since
these functions draw directly on the root window the server should be
grabbed until the outline is erased.
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
Erase a previously drawn outline of a window of dimensions (WIDTH, HEIGHT)
at position (X, Y) relative to the root window. See `draw-window-outline'.

MODE is a symbol defining the type of outline drawn, currently it may
only be `box' for a 3x3 grid.
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

Return the width of the root window (in pixels).
::end:: */
{
    return rep_MAKE_INT(screen_width);
}

DEFUN("screen-height", Fscreen_height, Sscreen_height, (void), rep_Subr0) /*
::doc:Sscreen-height::
screen-height

Return the height of the root window (in pixels).
::end:: */
{
    return rep_MAKE_INT(screen_height);
}

DEFUN("sync-server", Fsync_server, Ssync_server, (void), rep_Subr0) /*
::doc:Ssync-server::
sync-server

Flush all pending X requests, don't wait for them to finish.
::end:: */
{
    XFlush (dpy);
    return Qt;
}

DEFUN("delete-x-property", Fdelete_x_property, Sdelete_x_property,
      (repv win, repv atom), rep_Subr2) /*
::doc:Sdelete-x-property::
delete-x-property WINDOW ATOM

Delete the X property ATOM (a symbol) of WINDOW.

WINDOW may be the symbol `root', a window object or a numeric window id.
::end:: */
{
    Window w = x_win_from_arg (win);
    rep_DECLARE2(atom, rep_SYMBOLP);
    if (w == 0)
	return WINDOWP(win) ? atom : rep_signal_arg_error (win, 1);
    XDeleteProperty (dpy, w,
		     XInternAtom (dpy, rep_STR(rep_SYM(atom)->name), False));
    return atom;
}

DEFUN("list-x-properties", Flist_x_properties, Slist_x_properties,
      (repv win), rep_Subr1) /*
::doc:Slist-x-properties::
list-x-properties WINDOW

List all X properties (symbols) of WINDOW.

WINDOW may be the symbol `root', a window object or a numeric window id.
::end:: */
{
    Window w;
    Atom *atoms;
    int count;
    repv ret = Qnil;
    rep_GC_root gc_ret;

    w = x_win_from_arg (win);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);
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
    ret = Fnreverse (ret);

    rep_PUSHGC(gc_ret, ret);
    emit_pending_destroys ();
    rep_POPGC;

    return ret;
}

DEFUN("get-x-property", Fget_x_property, Sget_x_property,
      (repv win, repv prop), rep_Subr2) /*
::doc:Sget-x-property::
get-x-property WINDOW PROPERTY

Return (TYPE FORMAT DATA) representing the X property PROPERTY (a
symbol) of WINDOW. If no such property exists, return nil.

WINDOW may be the symbol `root', a window object or a numeric window
id.

TYPE is a symbol representing the atom defining the type of the
property. FORMAT is an integer, either 8, 16 or 32, defining the width
of the data items read. DATA is an array, either a string for an 8-bit
format, or a vector of integers.

If TYPE is `ATOM' and FORMAT is 32, then DATA will be a vector of
symbols, representing the atoms read.
::end:: */
{
    Window w;
    Atom a_prop;
    Atom type;
    int format;
    u_long nitems;
    u_char *data = 0;
    repv type_sym, ret_data = Qnil;
    rep_GC_root gc_ret_data;

    w = x_win_from_arg (win);
    rep_DECLARE2(prop, rep_SYMBOLP);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);
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
	    if (type == None)
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
	CARD16 *s_data;
	CARD32 *l_data;
	int i;

    case 8:
	ret_data = rep_string_dupn (data, nitems);
	break;

    case 16:
	ret_data = Fmake_vector (rep_MAKE_INT(nitems), Qnil);
	s_data = (CARD16 *)data;
	for (i = 0; i < nitems; i++)
	    rep_VECTI(ret_data, i) = rep_MAKE_INT(s_data[i]);
	break;

    case 32:
	ret_data = Fmake_vector (rep_MAKE_INT(nitems), Qnil);
	l_data = (CARD32 *)data;
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

    ret_data = rep_list_3 (type_sym, rep_MAKE_INT(format), ret_data);

    rep_PUSHGC(gc_ret_data, ret_data);
    emit_pending_destroys ();
    rep_POPGC;

    return ret_data;
}

DEFUN("set-x-property", Fset_x_property, Sset_x_property,
      (repv win, repv prop, repv data, repv type, repv format), rep_Subr5) /*
::doc:Sset-x-property::
set-x-property WINDOW PROPERTY DATA TYPE FORMAT

Set the X property PROPERTY (a symbol) of WINDOW to the array DATA.

WINDOW may be the symbol `root', a window object or a numeric window
id.

TYPE is a symbol representing the atom defining the type of the
property, FORMAT is either 8, 16 or 32 defining the width of the data
values. DATA is either a string or a vector of integers.

If TYPE is `ATOM' and FORMAT is 32, then any symbols in DATA will be
converted to their numeric X atoms.
::end:: */
{
    Window w;
    Atom a_prop, a_type;
    u_long nitems;
    u_char *c_data = 0;

    w = x_win_from_arg (win);
    rep_DECLARE2(prop, rep_SYMBOLP);
    a_prop = XInternAtom (dpy, rep_STR(rep_SYM(prop)->name), False);
    rep_DECLARE(3, data, rep_VECTORP(data) || rep_STRINGP(data));
    rep_DECLARE4(type, rep_SYMBOLP);
    a_type = XInternAtom (dpy, rep_STR(rep_SYM(type)->name), False);
    rep_DECLARE5(format, rep_INTP);
    if (w == 0)
	return WINDOWP(win) ? prop : rep_signal_arg_error (win, 1);

    /* Convert to data array */

    if (rep_STRINGP(data))
	nitems = rep_STRING_LEN(data);
    else
	nitems = rep_VECT_LEN(data);

    switch (rep_INT(format))
    {
	int i;
	CARD16 *s_data;
	CARD32 *l_data;

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
	s_data = (CARD16 *)c_data;
	for (i = 0; i < nitems; i++)
	    s_data[i] = rep_INT(rep_VECTI(data, i));
	break;

    case 32:
	if (rep_STRINGP(data))
	    return rep_signal_arg_error (data, 3);
	c_data = alloca (nitems * 4);
	l_data = (CARD32 *)c_data;
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

DEFUN("get-x-text-property", Fget_x_text_property, Sget_x_text_property,
      (repv win, repv prop), rep_Subr2) /*
::doc:Sget-x-text-property::
get-x-text-property WINDOW PROPERTY
::end:: */
{
    Window w;
    Atom a_prop;
    XTextProperty t_prop;
    repv ret = Qnil;

    w = x_win_from_arg (win);
    rep_DECLARE2(prop, rep_SYMBOLP);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);
    a_prop = XInternAtom (dpy, rep_STR(rep_SYM(prop)->name), False);

    if (XGetTextProperty (dpy, w, &t_prop, a_prop) != 0)
    {
	char **list;
	int count;
	if (XTextPropertyToStringList (&t_prop, &list, &count) != 0)
	{
	    int i;
	    ret = Fmake_vector (rep_MAKE_INT(count), Qnil);
	    for (i = 0; i < count; i++)
		rep_VECTI(ret, i) = rep_string_dup (list[i]);
	    XFreeStringList (list);
	}
	XFree (t_prop.value);
    }

    return ret;
}

DEFUN("set-x-text-property", Fset_x_text_property, Sset_x_text_property, 
      (repv win, repv prop, repv vect), rep_Subr3) /*
::doc:Sset-x-text-prooperty::
set-x-text-property WINDOW PROPERTY STRING-VECTOR
::end:: */
{
    Window w;
    Atom a_prop;
    XTextProperty t_prop;
    char **strings;
    int count, i;

    w = x_win_from_arg (win);
    rep_DECLARE2(prop, rep_SYMBOLP);
    rep_DECLARE3(vect, rep_VECTORP);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);
    a_prop = XInternAtom (dpy, rep_STR(rep_SYM(prop)->name), False);

    count = rep_VECT_LEN(vect);
    strings = alloca (sizeof (char *) * (count + 1));
    for (i = 0; i < count; i++)
    {
	if (!rep_STRINGP(rep_VECTI(vect, i)))
	    return rep_signal_arg_error (vect, 3);
	strings[i] = rep_STR(rep_VECTI(vect, i));
    }
    if (XStringListToTextProperty (strings, count, &t_prop) != 0)
    {
	XSetTextProperty (dpy, w, &t_prop, a_prop);
	XFree (t_prop.value);
    }

    return Qt;
}

DEFUN("send-client-message", Fsend_client_message, Ssend_client_message,
      (repv win, repv type, repv data, repv format), rep_Subr4) /*
::doc:Ssend-client-message::
send-client-message WINDOW TYPE DATA FORMAT

Send an X ClientMessage event to WINDOW (a window object or the symbol
`root'). It will be of the type TYPE (a symbol), contain the array of
integers DATA (i.e. a vector or a string), and it will be transferred as
FORMAT sized quantities (8, 16 or 32).
::end:: */
{
    XClientMessageEvent ev;
    Window w = x_win_from_arg (win);

    rep_DECLARE2(type, rep_SYMBOLP);
    rep_DECLARE(3, data, rep_STRINGP(data) || rep_VECTORP(data));
    rep_DECLARE4(format, rep_INTP);

    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);

    ev.type = ClientMessage;
    ev.window = w;
    ev.message_type = XInternAtom (dpy, rep_STR(rep_SYM(type)->name), False);
    ev.format = rep_INT(format);

    switch (rep_INT(format))
    {
	int i;

    case 8:
	if (rep_STRINGP(data))
	    memcpy (rep_STR(data), ev.data.b, MAX(rep_STRING_LEN(data), 20));
	else
	{
	    for (i = 0; i < rep_VECT_LEN(data) && i < 20; i++)
		ev.data.b[i] = rep_INT(rep_VECTI(data, i));
	}
	break;

    case 16:
	if (rep_STRINGP(data))
	    return rep_signal_arg_error (data, 3);
	for (i = 0; i < rep_VECT_LEN(data) && i < 10; i++)
	    ev.data.s[i] = rep_INT(rep_VECTI(data, i));
	break;

    case 32:
	if (rep_STRINGP(data))
	    return rep_signal_arg_error (data, 3);
	for (i = 0; i < rep_VECT_LEN(data) && i < 5; i++)
	    ev.data.l[i] = rep_INT(rep_VECTI(data, i));
	break;
    }

    XSendEvent (dpy, w, False, 0L, (XEvent *) &ev);
    return win;
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
    rep_DECLARE2(x, rep_INTP);
    rep_DECLARE3(y, rep_INTP);
    rep_DECLARE4(width, rep_INTP);
    rep_DECLARE5(height, rep_INTP);
    if (parent_w == 0)
	return WINDOWP(parent) ? Qnil : rep_signal_arg_error (parent, 1);
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

DEFUN("getpid", Fgetpid, Sgetpid, (void), rep_Subr0) /*
::doc:Sgetpid::
getpid

Return the process-id of the running Lisp interpreter.
::end:: */
{
    return rep_MAKE_INT(getpid ());
}


/* Displaying a `message' window */

static Window message_win;

static struct {
    GC gc;
    repv text, fg, bg, font;
} message;

#define MSG_PAD_X 20
#define MSG_PAD_Y 10

static void
refresh_message_window ()
{
    if (message_win != 0)
    {
	XGCValues values;
	u_long mask;
	values.foreground = VCOLOR(message.fg)->pixel;
	values.background = VCOLOR(message.bg)->pixel;
	values.font = VFONT(message.font)->font->fid;
	mask = GCForeground | GCBackground | GCFont;

	if (message.gc == 0)
	    message.gc = XCreateGC (dpy, message_win, mask, &values);
	else
	    XChangeGC (dpy, message.gc, mask, &values);

	XClearWindow (dpy, message_win);
	XDrawString (dpy, message_win, message.gc, MSG_PAD_X,
		     MSG_PAD_Y + VFONT(message.font)->font->ascent,
		     rep_STR(message.text), rep_STRING_LEN(message.text));
    }
}

static void
message_event_handler (XEvent *ev)
{
    if (ev->type == Expose)
	refresh_message_window ();
}

DEFSTRING(white, "white");
DEFSTRING(black, "black");

DEFUN("show-message", Fshow_message, Sshow_message,
      (repv text, repv font, repv fg, repv bg, repv pos), rep_Subr5) /*
::doc:Sshow-message::
show-message [TEXT] [FONT] [FG] [BG] [POSITION]

Display the string TEXT in the center of the screen. If TEXT is not
specified then any string previously displayed is removed. Returns the
numeric id of the window displaying the message, or nil if no message
is displayed.

FONT defines the font to use, if undefined the `default-font' variable
provides this value. FG and BG define the color of the text and its
background respectively. If undefined they are black and white.

POSITION is a cons cell `(X . Y)'. X and Y are integers or nil (for
centered display). If negative they count in from the left and bottom
edges respectively.

Note that newlines in TEXT are ignored. This may change in the future.
::end:: */
{
    if (text == Qnil)
    {
	if (message_win != 0)
	{
	    deregister_event_handler (message_win);
	    XDestroyWindow (dpy, message_win);
	    message_win = 0;
	}
	if (message.gc != 0)
	{
	    XFreeGC (dpy, message.gc);
	    message.gc = 0;
	}
	message.font = message.fg = message.bg = message.text = Qnil;
	return Qnil;
    }
    else
    {
	int width, height, x, y;

	rep_DECLARE1(text, rep_STRINGP);

	if (!FONTP(font))
	    font = Fsymbol_value (Qdefault_font, Qt);
	if (!FONTP(font))
	    return rep_signal_arg_error (font, 1);

	if (!COLORP(fg))
	    fg = Fget_color (rep_VAL(&black));
	if (!COLORP(fg))
	    return rep_signal_arg_error (fg, 1);

	if (!COLORP(bg))
	    bg = Fget_color (rep_VAL(&white));
	if (!COLORP(bg))
	    return rep_signal_arg_error (bg, 1);

	message.text = text;
	message.font = font;
	message.fg = fg;
	message.bg = bg;

	width = rep_INT(Ftext_width (text, font)) + MSG_PAD_X * 2;
	height = rep_INT(Ffont_height (font)) + MSG_PAD_Y * 2;
	x = (screen_width - width) / 2;
	y = (screen_height - height) / 2;

	if (rep_CONSP(pos))
	{
	    if (rep_INTP(rep_CAR(pos)))
	    {
		x = rep_INT(rep_CAR(pos));
		if (x < 0)
		    x += screen_width - width;
	    }
	    if (rep_INTP(rep_CDR(pos)))
	    {
		y = rep_INT(rep_CDR(pos));
		if (y < 0)
		    y += screen_height - height;
	    }
	}

	if (message_win == 0)
	{
	    /* I tried setting save_under in here, but it just slows
	       down opaque window moves.. */
	    XSetWindowAttributes attr;
	    attr.override_redirect = True;
	    attr.background_pixel = VCOLOR(bg)->pixel;
	    attr.border_pixel = BlackPixel(dpy, screen_num);
	    attr.event_mask = ExposureMask;
	    attr.colormap = screen_cmap;
	    message_win = XCreateWindow (dpy, root_window, x, y,
					 width, height, 1, screen_depth,
					 InputOutput, screen_visual,
					 CWBackPixel | CWBorderPixel
					 | CWOverrideRedirect | CWEventMask
					 | CWColormap, &attr);
	    if (message_win == 0)
		return Qnil;
	    register_event_handler (message_win, message_event_handler);
	    XMapRaised (dpy, message_win);
	}
	else
	{
	    XWindowChanges attr;
	    attr.x = x;
	    attr.y = y;
	    attr.width = width;
	    attr.height = height;
	    attr.stack_mode = TopIf;
	    XConfigureWindow (dpy, message_win,
			      CWX | CWY | CWWidth | CWHeight | CWStackMode,
			      &attr);
	    refresh_message_window ();
	}
	return rep_MAKE_INT(message_win);
    }
}


/* initialisation */

void
functions_init (void)
{
    rep_ADD_SUBR(Srestack_windows);
    rep_ADD_SUBR(Sx_raise_window);
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
    rep_ADD_SUBR(Sgrab_keyboard);
    rep_ADD_SUBR(Sungrab_keyboard);
    rep_ADD_SUBR(Sdraw_window_outline);
    rep_ADD_SUBR(Serase_window_outline);
    rep_ADD_SUBR(Sscreen_width);
    rep_ADD_SUBR(Sscreen_height);
    rep_ADD_SUBR(Ssync_server);
    rep_ADD_SUBR(Sdelete_x_property);
    rep_ADD_SUBR(Slist_x_properties);
    rep_ADD_SUBR(Sget_x_property);
    rep_ADD_SUBR(Sset_x_property);
    rep_ADD_SUBR(Sget_x_text_property);
    rep_ADD_SUBR(Sset_x_text_property);
    rep_ADD_SUBR(Ssend_client_message);
    rep_ADD_SUBR(Screate_window);
    rep_ADD_SUBR(Sx_atom);
    rep_ADD_SUBR(Sx_atom_name);
    rep_ADD_SUBR(Sgetpid);
    rep_ADD_SUBR(Sshow_message);
    rep_INTERN(root);
    rep_INTERN_SPECIAL(after_restacking_hook);

    rep_mark_static (&message.text);
    rep_mark_static (&message.fg);
    rep_mark_static (&message.bg);
    rep_mark_static (&message.font);
}

void
functions_kill (void)
{
    if (message_win != 0)
	XDestroyWindow (dpy, message_win);
}
