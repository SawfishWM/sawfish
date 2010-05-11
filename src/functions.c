/* functions.c -- useful window manager Lisp functions
   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/* Define this to fake two side-by-side Xinerama style heads */
#undef TEST_XINERAMA

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
   
#include "sawfish.h"
#include <string.h>
#include <X11/Xatom.h>

/* Number of outstanding server grabs made; only when this is zero is
   the server ungrabbed. */
static int server_grabs;

static int xinerama_heads;

#ifdef HAVE_X11_EXTENSIONS_XINERAMA_H
# include <X11/extensions/Xinerama.h>
  static XineramaScreenInfo *xinerama_head_info = NULL;
# ifdef TEST_XINERAMA
   static XineramaScreenInfo debug_heads[2] = {
     { 0, 0, 0, 512, 768 },
     { 0, 512, 0, 512, 768 }
   };
   static int debug_nheads = 2;
# else
   static int xinerama_event_base, xinerama_error_base;
# endif
#endif

DEFSYM(root, "root");
DEFSYM(after_restacking_hook, "after-restacking-hook");
DEFSYM(position, "position");
DEFSYM(spacing, "spacing");
DEFSYM(window, "window");
DEFSYM(head, "head");

DEFUN("restack-windows", Frestack_windows, Srestack_windows,
      (repv list), rep_Subr1) /*
::doc:sawfish.wm.windows.subrs#restack-windows::
restack-windows LIST

Restack all windows in the list of windows LIST in the order they occur
in the list (from top to bottom). The stacking order of any unspecified
windows isn't affected.
::end:: */
{
    repv ptr;
    Lisp_Window *pred;

    rep_DECLARE1(list, rep_LISTP);
    for (ptr = list; rep_CONSP (ptr); ptr = rep_CDR (ptr))
    {
	if (!WINDOWP (rep_CAR (ptr)))
	    return rep_signal_arg_error (list, 1);
    }

    if (list == Qnil)
	return Qt;

    ptr = list;
    pred = 0;

    while (rep_CONSP (ptr))
    {
	Lisp_Window *this = VWIN (rep_CAR (ptr));

	if (!WINDOW_IS_GONE_P (this))
	{
	    if (pred != 0 && !WINDOW_IS_GONE_P (pred))
	    {
		remove_from_stacking_list (this);
		insert_in_stacking_list_below (this, pred);

		/* This works because it tries to stack relative to
		   the window above THIS first; which we just set */
		restack_window (this);
	    }
	    pred = this;
	}

	ptr = rep_CDR (ptr);

	rep_TEST_INT;
	if (rep_INTERRUPTP)
	    return rep_NULL;
    }

    Fcall_hook (Qafter_restacking_hook, Qnil, Qnil);
    return Qt;
}

DEFUN("x-raise-window", Fx_raise_window, Sx_raise_window,
      (repv win, repv above), rep_Subr2) /*
::doc:sawfish.wm.windows.subrs#x-raise-window::
x-raise-window WINDOW [ABOVE]

Raise WINDOW so that it is above window ABOVE. If ABOVE is undefined,
raise WINDOW to the top of the stacking order.
::end:: */
{
    rep_DECLARE1 (win, WINDOWP);

    if (!WINDOW_IS_GONE_P (VWIN (win)))
    {
	if (WINDOWP (above))
	{
	    if (!WINDOW_IS_GONE_P (VWIN (above)))
	    {
		remove_from_stacking_list (VWIN (win));
		insert_in_stacking_list_above (VWIN (win), VWIN (above));
	    }
	}
	else
	{
	    remove_from_stacking_list (VWIN (win));
	    insert_in_stacking_list_above_all (VWIN (win));
	}
	restack_window (VWIN (win));
	Fcall_hook (Qafter_restacking_hook, Qnil, Qnil);
    }
    return win;
}

DEFUN("x-lower-window", Fx_lower_window, Sx_lower_window,
      (repv win, repv below), rep_Subr2) /*
::doc:sawfish.wm.windows.subrs#x-lower-window::
x-lower-window WINDOW [BELOW]

Lower WINDOW so that it is below window BELOW. If BELOW is undefined,
lower WINDOW to the bottom of the stacking order.
::end:: */
{
    rep_DECLARE1 (win, WINDOWP);

    if (!WINDOW_IS_GONE_P (VWIN (win)))
    {
	if (WINDOWP (below))
	{
	    if (!WINDOW_IS_GONE_P (VWIN (below)))
	    {
		remove_from_stacking_list (VWIN (win));
		insert_in_stacking_list_below (VWIN (win), VWIN (below));
	    }
	}
	else
	{
	    remove_from_stacking_list (VWIN (win));
	    insert_in_stacking_list_below_all (VWIN (win));
	}
	restack_window (VWIN (win));
	Fcall_hook (Qafter_restacking_hook, Qnil, Qnil);
    }
    return win;
}

DEFUN("x-kill-client", Fx_kill_client, Sx_kill_client,
      (repv win), rep_Subr1)
{
    Window w = x_win_from_arg (win);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);
    XKillClient (dpy, w);
    return win;
}

DEFUN_INT("destroy-window", Fdestroy_window, Sdestroy_window, (repv win), rep_Subr1, "%W") /*
::doc:sawfish.wm.windows.subrs#destroy-window::
destroy-window WINDOW

Destroy WINDOW with out giving the owning application any warning.

WINDOW may be a window object or a numeric window id.
::end:: */
{
    if (WINDOWP(win))
	XDestroyWindow (dpy, VWIN(win)->id);
    else if (rep_INTEGERP(win))
	XDestroyWindow (dpy, rep_get_long_uint (win));
    else
	return rep_signal_arg_error (win, 1);
    return win;
}

DEFUN("warp-cursor", Fwarp_cursor, Swarp_cursor, (repv x, repv y), rep_Subr2) /*
::doc:sawfish.wm.misc#warp-cursor::
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
	invalidate_cached_mouse_position ();
	return Qt;
    }
    else
	return Qnil;
}

DEFUN("move-window-to", Fmove_window_to, Smove_window_to,
      (repv win, repv x, repv y), rep_Subr3) /*
::doc:sawfish.wm.windows.subrs#move-window-to::
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
::doc:sawfish.wm.windows.subrs#resize-window-to::
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
    VWIN (win)->pending_configure = 0;
    Fcall_window_hook (Qwindow_resized_hook, win, Qnil, Qnil);
    return win;
}

DEFUN("move-resize-window-to", Fmove_resize_window_to, Smove_resize_window_to,
      (repv win, repv x, repv y, repv width, repv height), rep_Subr5) /*
::doc:sawfish.wm.windows.subrs#move-resize-window-to::
move-resize-window-to WINDOW X Y WIDTH HEIGHT

Reconfigure the geometry of window object WINDOW as specified.
::end:: */
{
    bool resized, moved;
    rep_DECLARE1(win, WINDOWP);
    rep_DECLARE2(x, rep_INTP);
    rep_DECLARE3(y, rep_INTP);
    rep_DECLARE4(width, rep_INTP);
    rep_DECLARE5(height, rep_INTP);
    moved = (VWIN(win)->attr.x != rep_INT(x)
	     || VWIN(win)->attr.y != rep_INT(y));
    resized = (VWIN(win)->attr.width != rep_INT(width)
	       || VWIN(win)->attr.height != rep_INT(height));
    VWIN(win)->attr.x = rep_INT(x);
    VWIN(win)->attr.y = rep_INT(y);
    VWIN(win)->attr.width = rep_INT(width);
    VWIN(win)->attr.height = rep_INT(height);
    if (resized)
    {
	fix_window_size (VWIN(win));
	VWIN (win)->pending_configure = 0;
    }
    if (moved && !resized)
    {
	XMoveWindow (dpy, VWIN(win)->reparented ? VWIN(win)->frame
		     : VWIN(win)->id, VWIN(win)->attr.x, VWIN(win)->attr.y);
	send_synthetic_configure (VWIN(win));
    }
    if (moved)
	Fcall_window_hook (Qwindow_moved_hook, win, Qnil, Qnil);
    if (resized)
	Fcall_window_hook (Qwindow_resized_hook, win, Qnil, Qnil);
    return win;
}

DEFUN("grab-server", Fgrab_server, Sgrab_server, (void), rep_Subr0) /*
::doc:sawfish.wm.misc#grab-server::
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
::doc:sawfish.wm.misc#ungrab-server::
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

DEFUN("server-grabbed-p", Fserver_grabbed_p,
      Sserver_grabbed_p, (void), rep_Subr0) /*
::doc:sawfish.wm.misc#server-grabbed-p::
server-grabbed-p

Return t if the server is currently grabbed.
::end:: */
{
    return (server_grabs > 0) ? Qt : Qnil;
}

/* Call this when the server may have been ungrabbed prematurely (imlib
   lossage) */
void
regrab_server (void)
{
    if (server_grabs > 0)
    {
	server_grabs--;
	Fgrab_server ();
    }
}

DEFUN("grab-pointer", Fgrab_pointer, Sgrab_pointer,
      (repv win, repv cursor, repv ptr_sync, repv kbd_sync, repv confine_to),
      rep_Subr5) /*
::doc:sawfish.wm.events#grab-pointer::
grab-pointer [WINDOW] [CURSOR] [PTR-SYNC] [KBD-SYNC] [CONFINE-TO]

Grab the pointer and direct all pointer events to window object
WINDOW. If CURSOR is defined and a cursor object, display this whilst
the pointer is grabbed.

If PTR-SYNC or KBD-SYNC is non-nil the pointer or the keyboard will be
frozen, i.e., the device will not produce events until either the grab
is released or events are re-enabled using allow-events.

CONFINE-TO, if non-nil, is a visible window to confine the pointer to.

If WINDOW is a window object corresponding to a visible window the
grab will be made on its frame.  If WINDOW is an integer, it specifies the
window id of the grab window.  Otherwise the grab will be made on the root
window.  CONFINE-TO is interpreted similarly except that the default
is not to confine the pointer.  If the window id of a non-viewable window
was specified for either WINDOW of CONFINE-TO the grab will be made on the
root window without confining the pointer.

Returns non-nil if the grab succeeded.
::end:: */
{
    Window g_win, c_win;
    int ret;

    if (WINDOWP(win) && VWIN(win)->visible)
	g_win = VWIN(win)->frame;
    else if (rep_INTP(win))
	g_win = rep_INT(win);
    else
	g_win = root_window;

    if (WINDOWP(confine_to) && VWIN(confine_to)->visible)
        c_win = VWIN(confine_to)->frame;
    else if (rep_INTP(confine_to))
	c_win = rep_INT(confine_to);
    else
        c_win = None;

    if (cursor != Qnil && !CURSORP(cursor))
    {
	cursor = Fget_cursor (cursor);
	if (!cursor)
	    cursor = Qnil;
    }

again:
    ret = XGrabPointer (dpy, g_win, False, POINTER_GRAB_EVENTS,
			rep_NILP( ptr_sync) ? GrabModeAsync : GrabModeSync,
			rep_NILP( kbd_sync) ? GrabModeAsync : GrabModeSync,
			c_win,
			CURSORP(cursor) ? VCURSOR(cursor)->cursor : None,
			last_event_time);
    if (ret == GrabNotViewable && (g_win != root_window || c_win != None))
    {
	/* fall back to the root window. */
	g_win = root_window;
	c_win = None;
	goto again;
    }

    if (ret == GrabSuccess)
	mark_pointer_grabbed ();

    DB(("grab-pointer: time=%lu ret=%d\n", last_event_time, ret));
    return (ret == GrabSuccess) ? Qt : Qnil;
}

DEFUN("ungrab-pointer", Fungrab_pointer, Sungrab_pointer, (void), rep_Subr0) /*
::doc:sawfish.wm.events#ungrab-pointer::
ungrab-pointer

Release the grab on the mouse pointer.
::end:: */
{
    ungrab_pointer ();
    synthesize_button_release ();
    DB(("ungrab-pointer: time=%lu\n", last_event_time));
    return Qt;
}

DEFUN("grab-keyboard", Fgrab_keyboard, Sgrab_keyboard,
      (repv win, repv ptr_sync, repv kbd_sync), rep_Subr3) /*
::doc:sawfish.wm.events#grab-keyboard::
grab-keyboard [WINDOW] [PTR-SYNC] [KBD-SYNC]

Grab the keyboard and direct all keyboard events to window object
WINDOW.  If WINDOW is a window object corresponding to a visible
window the grab will be made on its frame.  If WINDOW is an integer it
specifies the window id of the grab window.  Otherwise the grab will
be made on the root window.  If the window id of a non-viewable window
was specified the grab is made on the root window instead.

If PTR-SYNC or KBD-SYNC is non-nil the pointer or the keyboard will be
frozen, i.e., the device will not produce events until either the grab
is released or events are re-enabled using allow-events.

Returns non-nil if the grab succeeded.
::end:: */
{
    Window g_win;
    int ret;

    if (WINDOWP(win) && VWIN(win)->visible)
	g_win = VWIN(win)->frame;
    else if (rep_INTP(win))
	g_win = rep_INT(win);
    else
	g_win = root_window;

again:
    ret = XGrabKeyboard (dpy, g_win, False,
			 rep_NILP( ptr_sync) ? GrabModeAsync : GrabModeSync,
			 rep_NILP( kbd_sync) ? GrabModeAsync : GrabModeSync,
			 last_event_time);
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
::doc:sawfish.wm.events#ungrab-keyboard::
ungrab-keyboard

Release the grab on the keyboard.
::end:: */
{
    DB(("ungrab-keyboard: time=%lu\n", last_event_time));
    XUngrabKeyboard (dpy, last_event_time);
    return Qt;
}

DEFUN("screen-width", Fscreen_width, Sscreen_width, (void), rep_Subr0) /*
::doc:sawfish.wm.misc#screen-width::
screen-width

Return the width of the root window (in pixels).
::end:: */
{
    return rep_MAKE_INT(screen_width);
}

DEFUN("screen-height", Fscreen_height, Sscreen_height, (void), rep_Subr0) /*
::doc:sawfish.wm.misc#screen-height::
screen-height

Return the height of the root window (in pixels).
::end:: */
{
    return rep_MAKE_INT(screen_height);
}

DEFUN("sync-server", Fsync_server, Ssync_server, (repv wait), rep_Subr0) /*
::doc:sawfish.wm.misc#sync-server::
sync-server [WAIT]

Flush all pending X requests, don't wait for them to finish, unless WAIT
is true.
::end:: */
{
    if (wait == Qnil)
	XFlush (dpy);
    else
	XSync (dpy, False);
    return Qt;
}

DEFUN("delete-x-property", Fdelete_x_property, Sdelete_x_property,
      (repv win, repv atom), rep_Subr2) /*
::doc:sawfish.wm.misc#delete-x-property::
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
    if (WINDOWP (win))
	property_cache_invalidate (win, atom);
    return atom;
}

DEFUN("list-x-properties", Flist_x_properties, Slist_x_properties,
      (repv win), rep_Subr1) /*
::doc:sawfish.wm.misc#list-x-properties::
list-x-properties WINDOW

List all X properties (symbols) of WINDOW.

WINDOW may be the symbol `root', a window object or a numeric window id.
::end:: */
{
    Window w;
    Atom *atoms;
    int count;
    repv ret = Qnil;

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
    return Fnreverse (ret);
}

DEFUN("get-x-property", Fget_x_property, Sget_x_property,
      (repv win, repv prop), rep_Subr2) /*
::doc:sawfish.wm.misc#get-x-property::
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
    unsigned long nitems;
    unsigned char *data = 0;
    repv type_sym, ret_data = Qnil;

    w = x_win_from_arg (win);
    rep_DECLARE2(prop, rep_SYMBOLP);
    if (w == 0)
	return WINDOWP(win) ? Qnil : rep_signal_arg_error (win, 1);

    if (WINDOWP (win))
    {
	ret_data = property_cache_ref (win, prop);
	if (ret_data != rep_NULL)
	    return ret_data;
    }

    a_prop = XInternAtom (dpy, rep_STR(rep_SYM(prop)->name), False);

    /* First read the data.. */
    {
	long long_length = 32;
	unsigned long bytes_after;
	while (1)
	{
	    ret_data = Qnil;
	    if (data != 0)
		XFree (data);
	    if (XGetWindowProperty (dpy, w, a_prop, 0, long_length, False,
				    AnyPropertyType, &type, &format,
				    &nitems, &bytes_after, &data) != Success)
		goto out;
	    if (type == None)
		goto out;
	    if (bytes_after == 0)
		break;
	    long_length += (bytes_after / sizeof(unsigned long)) + 1;
	}
    }

    /* Convert the type to a symbol */
    type_sym = x_atom_symbol (type);
    
    /* Then convert the contents to a vector or string */
    switch (format)
    {
	unsigned short *s_data;
	unsigned long *l_data;
	int i;

    case 8:
	ret_data = rep_string_dupn ((char *) data, nitems);
	break;

    case 16:
	ret_data = Fmake_vector (rep_MAKE_INT(nitems), Qnil);
	s_data = (unsigned short *)data;
	for (i = 0; i < nitems; i++)
	    rep_VECTI(ret_data, i) = rep_MAKE_INT(s_data[i]);
	break;

    case 32:
	ret_data = Fmake_vector (rep_MAKE_INT(nitems), Qnil);
	l_data = (unsigned long *)data;
	for (i = 0; i < nitems; i++)
	{
	    repv name;
	    if (type == XA_ATOM && (name = x_atom_symbol (l_data[i])) != Qnil)
		rep_VECTI(ret_data, i) = name;
	    else if (type == XA_INTEGER)
		rep_VECTI(ret_data, i) = rep_make_long_int((long) l_data[i]);
	    else
		rep_VECTI(ret_data, i) = rep_make_long_uint(l_data[i] & 0xffffffffUL);
	}
	break;
    }

    XFree (data);
    ret_data = rep_list_3 (type_sym, rep_MAKE_INT(format), ret_data);

out:
    if (WINDOWP (win))
	property_cache_set (win, prop, ret_data, 0);

    return ret_data;
}

DEFUN("set-x-property", Fset_x_property, Sset_x_property,
      (repv win, repv prop, repv data, repv type, repv format), rep_Subr5) /*
::doc:sawfish.wm.misc#set-x-property::
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
    unsigned long nitems;
    unsigned char *c_data = 0;

    w = x_win_from_arg (win);
    rep_DECLARE2(prop, rep_SYMBOLP);
    a_prop = XInternAtom (dpy, rep_STR(rep_SYM(prop)->name), False);
    rep_DECLARE(3, data, rep_VECTORP(data) || rep_STRINGP(data));
    rep_DECLARE4(type, rep_SYMBOLP);
    a_type = XInternAtom (dpy, rep_STR(rep_SYM(type)->name), False);
    rep_DECLARE5(format, rep_INTP);
    if (w == 0)
	return WINDOWP(win) ? prop : rep_signal_arg_error (win, 1);

    if (WINDOWP (win))
	property_cache_set (win, prop, rep_list_3 (type, format, data), 1);

    /* Convert to data array */

    if (rep_STRINGP(data))
	nitems = rep_STRING_LEN(data);
    else
	nitems = rep_VECT_LEN(data);

    switch (rep_INT(format))
    {
	int i;
	unsigned short *s_data;
	unsigned long *l_data;

    case 8:
	if (rep_STRINGP(data))
	    c_data = (unsigned char *) rep_STR (data);
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
	c_data = alloca (nitems * sizeof (unsigned short));
	s_data = (unsigned short *)c_data;
	for (i = 0; i < nitems; i++)
	    s_data[i] = rep_INT(rep_VECTI(data, i));
	break;

    case 32:
	if (rep_STRINGP(data))
	    return rep_signal_arg_error (data, 3);
	c_data = alloca (nitems * sizeof (unsigned long));
	l_data = (unsigned long *)c_data;
	for (i = 0; i < nitems; i++)
	{
	    if (a_type == XA_ATOM && rep_SYMBOLP(rep_VECTI(data, i)))
		l_data[i] = XInternAtom (dpy, rep_STR(rep_SYM(rep_VECTI(data, i))->name), False);
	    else
		l_data[i] = rep_get_long_uint (rep_VECTI(data, i));
	}
	break;
    }

    /* Overwrite property */
    XChangeProperty (dpy, w, a_prop, a_type, rep_INT(format),
		     PropModeReplace, c_data, nitems);
    return prop;
}

DEFUN("send-client-message", Fsend_client_message, Ssend_client_message,
      (repv win, repv type, repv data, repv format), rep_Subr4) /*
::doc:sawfish.wm.events#send-client-message::
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
	    ev.data.l[i] = rep_get_long_uint (rep_VECTI(data, i));
	break;
    }

    XSendEvent (dpy, w, False, 0L, (XEvent *) &ev);
    return win;
}

DEFUN("create-window", Fcreate_window, Screate_window,
      (repv parent, repv x, repv y, repv width, repv height), rep_Subr5) /*
::doc:sawfish.wm.misc#create-window::
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
::doc:sawfish.wm.misc#x-atom::
x-atom SYMBOL

Return the integer identifying the X atom with the same name as SYMBOL.
::end:: */
{
    rep_DECLARE1(symbol, rep_SYMBOLP);
    return rep_MAKE_INT (XInternAtom (dpy, rep_STR(rep_SYM(symbol)->name),
				      False));
}

DEFUN("x-atom-name", Fx_atom_name, Sx_atom_name, (repv atom), rep_Subr1) /*
::doc:sawfish.wm.misc#x-atom-name::
x-atom-name ATOM

Return the symbol with the same name as the X atom identified by the
integer ATOM.
::end:: */
{
    rep_DECLARE1(atom, rep_INTP);
    return x_atom_symbol (rep_INT(atom));
}

DEFUN("root-window-id", Froot_window_id, Sroot_window_id, (void), rep_Subr0) /*
::doc:sawfish.wm.misc#root-window-id::
root-window-id

Returns the numeric id of the root window of the managed screen.
::end:: */
{
    return rep_MAKE_INT (root_window);
}

/* xinerama support */

void
update_xinerama_info (void)
{
#ifdef HAVE_X11_EXTENSIONS_XINERAMA_H
# ifndef TEST_XINERAMA
    if (dpy != 0)
    {
	if (XineramaQueryExtension (dpy, &xinerama_event_base,
				    &xinerama_error_base))
	{
	  if(xinerama_head_info != NULL){
	    XFree(xinerama_head_info);
	  }
	  xinerama_head_info = XineramaQueryScreens (dpy, &xinerama_heads);
	}
    }
# else
    xinerama_head_info = debug_heads;
    xinerama_heads = debug_nheads;
# endif
#endif
}

DEFUN ("head-count", Fhead_count, Shead_count, (void), rep_Subr0)
{
    return rep_MAKE_INT (MAX (1, xinerama_heads));
}

DEFUN ("find-head", Ffind_head, Sfind_head, (repv x, repv y), rep_Subr2)
{
    if (rep_CONSP (x) && y == Qnil)
    {
	y = rep_CDR (x);
	x = rep_CAR (x);
    }
    rep_DECLARE (1, x, rep_INTP (x));
    rep_DECLARE (2, y, rep_INTP (y));

#ifdef HAVE_X11_EXTENSIONS_XINERAMA_H
    {
	int i;
	for (i = 0; i < xinerama_heads; i++)
	{
	    if ((xinerama_head_info[i].x_org <= rep_INT (x))
		&& (xinerama_head_info[i].y_org <= rep_INT (y))
		&& (xinerama_head_info[i].x_org
		    + xinerama_head_info[i].width > rep_INT (x))
		&& (xinerama_head_info[i].y_org
		    + xinerama_head_info[i].height > rep_INT (y)))
	    {
		return rep_MAKE_INT (i);
	    }
	}
    }
#endif
    if (xinerama_heads == 0
	&& rep_INT (x) >= 0 && rep_INT (x) < screen_width
	&& rep_INT (y) >= 0 && rep_INT (y) < screen_height)
    {
	return rep_MAKE_INT (0);
    }
    return Qnil;
}

DEFUN ("head-dimensions", Fhead_dimensions,
       Shead_dimensions, (repv id), rep_Subr1)
{
    rep_DECLARE (1, id, rep_INTP (id));

    if (xinerama_heads == 0 && rep_INT (id) == 0)
    {
	return Fcons (rep_MAKE_INT (screen_width),
		      rep_MAKE_INT (screen_height));
    }
    else
    {
	rep_DECLARE (1, id, rep_INT (id) >= 0
		     && rep_INT (id) < xinerama_heads);
#ifdef HAVE_X11_EXTENSIONS_XINERAMA_H
	return Fcons (rep_MAKE_INT (xinerama_head_info[rep_INT(id)].width),
		      rep_MAKE_INT (xinerama_head_info[rep_INT(id)].height));
#else
	abort ();
#endif
    }
}

DEFUN ("head-offset", Fhead_offset, Shead_offset, (repv id), rep_Subr1)
{
    rep_DECLARE (1, id, rep_INTP (id));

    if (xinerama_heads == 0 && rep_INT (id) == 0)
    {
	return Fcons (rep_MAKE_INT (0), rep_MAKE_INT (0));
    }
    else
    {
	rep_DECLARE (1, id, rep_INT (id) >= 0
		     && rep_INT (id) < xinerama_heads);
#ifdef HAVE_X11_EXTENSIONS_XINERAMA_H
	return Fcons (rep_MAKE_INT (xinerama_head_info[rep_INT(id)].x_org),
		      rep_MAKE_INT (xinerama_head_info[rep_INT(id)].y_org));
#else
	abort ();
#endif
    }
}

/* Displaying a `message' window */

static Window message_win;

static struct {
    GC gc;
    repv text, fg, bg, font, justify;
    int width, spacing;
} message;

#define MSG_PAD_X 20
#define MSG_PAD_Y 10

static void
refresh_message_window ()
{
    if (message_win != 0)
    {
	XGCValues values;
	unsigned long mask;
	char *ptr;
	int row = 0;

	values.background = VCOLOR(message.bg)->pixel;
	values.graphics_exposures = False;
	mask = GCBackground | GCGraphicsExposures;

	if (message.gc == 0)
	    message.gc = XCreateGC (dpy, message_win, mask, &values);
	else
	    XChangeGC (dpy, message.gc, mask, &values);

	XClearWindow (dpy, message_win);

	ptr = rep_STR(message.text);
	while (*ptr != 0)
	{
	    char *end = strchr (ptr, '\n');
	    int offset;
	    if (end == 0)
		end = ptr + strlen (ptr);
	    if (message.justify == Qleft)
		offset = MSG_PAD_X;
	    else
	    {
		int width = x_text_width (message.font, ptr, end - ptr);
		if (message.justify == Qright)
		    offset = message.width - (width + MSG_PAD_X);
		else
		    offset = (message.width - width) / 2;
	    }
	    x_draw_string (message_win, message.font,
			   message.gc, VCOLOR(message.fg), offset,
			   MSG_PAD_Y
			   + row * (VFONT(message.font)->ascent
				    + VFONT(message.font)->descent
				    + message.spacing)
			   + VFONT(message.font)->ascent, ptr, end - ptr);
	    row++;
	    ptr = end;
	    if (*ptr == '\n')
		ptr++;
	}
    }
}

static void
message_event_handler (XEvent *ev)
{
    if (ev->type == Expose && ev->xexpose.count == 0)
	refresh_message_window ();
    else if (ev->type == ButtonPress)
	Fdisplay_message (Qnil, Qnil);
}

DEFSTRING(white, "white");
DEFSTRING(black, "black");

DEFUN("display-message", Fdisplay_message, Sdisplay_message,
      (repv text, repv attrs), rep_Subr2)
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
	int height, x, y;
	repv tem;

	rep_DECLARE1(text, rep_STRINGP);
	rep_DECLARE2(attrs, rep_LISTP);

	message.text = text;
	message.font = message.fg = message.bg = Qnil;
	message.justify = Qleft;
	message.spacing = 0;

	tem = Fassq (Qfont, attrs);
	if (tem && rep_CONSP(tem))
	{
	    message.font = rep_CDR(tem);
	    if (!FONTP(message.font))
	    {
		message.font = Fget_font (message.font);
		if (!message.font)
		    return rep_NULL;
	    }
	}
	if (!FONTP(message.font))
	    message.font = global_symbol_value (Qdefault_font);
	if (!FONTP(message.font))
	    return rep_signal_arg_error (Qfont, 1);

	tem = Fassq (Qforeground, attrs);
	if (tem && rep_CONSP(tem))
	{
	    message.fg = rep_CDR(tem);
	    if (!COLORP(message.fg))
	    {
		message.fg = Fget_color (message.fg, Qnil);
		if (!message.fg)
		    return rep_NULL;
	    }
	}
	if (!COLORP(message.fg))
	    message.fg = Fget_color (rep_VAL(&black), Qnil);
	if (!COLORP(message.fg))
	    return rep_signal_arg_error (Qforeground, 1);

	tem = Fassq (Qbackground, attrs);
	if (tem && rep_CONSP(tem))
	{
	    message.bg = rep_CDR(tem);
	    if (!COLORP(message.bg))
	    {
		message.bg = Fget_color (message.bg, Qnil);
		if (!message.bg)
		    return rep_NULL;
	    }
	}
	if (!COLORP(message.bg))
	    message.bg = Fget_color (rep_VAL(&white), Qnil);
	if (!COLORP(message.bg))
	    return rep_signal_arg_error (Qbackground, 1);

	tem = Fassq (Qx_justify, attrs);
	if (tem && rep_CONSP(tem))
	    message.justify = rep_CDR(tem);

	tem = Fassq (Qspacing, attrs);
	if (tem && rep_CONSP(tem) && rep_INTP(rep_CDR(tem)))
	    message.spacing = rep_INT(rep_CDR(tem));

	{
	    char *ptr = rep_STR(text);
	    int max_width = 0, rows = 0;
	    repv head = Qnil;
	    int head_width, head_height;
	    int head_xoff, head_yoff;
	    while (*ptr != 0)
	    {
		int text_width;
		char *end = strchr (ptr, '\n');
		if (end == 0)
		  end = ptr + strlen (ptr);
		text_width = x_text_width (message.font, ptr, end - ptr);
		max_width = MAX(max_width, text_width);
		rows++;
		ptr = end;
		if (*ptr == '\n')
		  ptr++;
	    }
	    message.width = max_width + MSG_PAD_X * 2;
	    height = ((rep_INT(Ffont_height (message.font)) + message.spacing)
		      * rows + MSG_PAD_Y * 2);

	    /* Find the head to put the message on. */
	    tem = Fassq (Qhead, attrs);
	    if (tem && rep_CONSP (tem) && rep_INTP (rep_CDR (tem)))
		head = rep_CDR (tem);
	    if (!head || head == Qnil)
		head = Ffind_head (Fquery_pointer (Qnil), Qnil);

	    if (head && head != Qnil)
	    {
		/* We have a head to centre on. */

		tem = Fhead_dimensions (head);
		if (!tem)
		    goto no_head;
		head_width = rep_INT (rep_CAR (tem));
		head_height = rep_INT (rep_CDR (tem));

		tem = Fhead_offset (head);
		if (!tem)
		    goto no_head;
		head_xoff = rep_INT (rep_CAR (tem));
		head_yoff = rep_INT (rep_CDR (tem));
	    }
	    else
	    {
		/* No head, just centre on the screen. */
	    no_head:
		head_xoff = head_yoff = 0;
		head_width = screen_width;
		head_height = screen_height;
	    }

	    x = head_xoff + ((head_width - message.width) / 2);
	    y = head_yoff + ((head_height - height) / 2);
	}

	tem = Fassq (Qposition, attrs);
	if (tem && rep_CONSP(tem))
	{
	    tem = rep_CDR(tem);
	    if (rep_CONSP(tem))
	    {
		if (rep_INTP(rep_CAR(tem)))
		{
		    x = rep_INT(rep_CAR(tem));
		    if (x < 0)
			x += screen_width - message.width;
		}
		if (rep_INTP(rep_CDR(tem)))
		{
		    y = rep_INT(rep_CDR(tem));
		    if (y < 0)
			y += screen_height - height;
		}
	    }
	}

	if (x + message.width > screen_width)
	    x = MAX (0, screen_width - message.width - 4);
	else if (x < 4)
	    x = 4;
	if (y + height > screen_height)
	    y = MAX (0, screen_height - height - 4);
	else if (y < 4)
	    y = 4;

	if (message_win == 0)
	{
	    /* I tried setting save_under in here, but it just slows
	       down opaque window moves.. */
	    XSetWindowAttributes attr;
	    attr.override_redirect = True;
	    attr.background_pixel = VCOLOR(message.bg)->pixel;
	    attr.border_pixel = BlackPixel(dpy, screen_num);
	    attr.event_mask = ExposureMask | ButtonPressMask;
	    attr.colormap = image_cmap;
	    message_win = XCreateWindow (dpy, root_window, x, y,
					 message.width, height, 1,
					 image_depth, InputOutput,
					 image_visual,
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
	    attr.width = message.width;
	    attr.height = height;
	    attr.stack_mode = TopIf;
	    XConfigureWindow (dpy, message_win,
			      CWX | CWY | CWWidth | CWHeight | CWStackMode,
			      &attr);
	    XSetWindowBackground (dpy, message_win, VCOLOR(message.bg)->pixel);
	    refresh_message_window ();
	}
	return rep_MAKE_INT(message_win);
    }
}

/* initialisation */

void
functions_init (void)
{
    repv tem;

    tem = rep_push_structure ("sawfish.wm.windows.subrs");
    rep_ADD_SUBR(Srestack_windows);
    rep_ADD_SUBR(Sx_raise_window);
    rep_ADD_SUBR(Sx_lower_window);
    rep_ADD_SUBR_INT(Sdestroy_window);
    rep_ADD_SUBR(Smove_window_to);
    rep_ADD_SUBR(Sresize_window_to);
    rep_ADD_SUBR(Smove_resize_window_to);
    rep_pop_structure (tem);

    tem = rep_push_structure ("sawfish.wm.misc");
    rep_ADD_SUBR(Sx_kill_client);
    rep_ADD_SUBR(Swarp_cursor);
    rep_ADD_SUBR(Sgrab_server);
    rep_ADD_SUBR(Sungrab_server);
    rep_ADD_SUBR(Sserver_grabbed_p);
    rep_ADD_SUBR(Sscreen_width);
    rep_ADD_SUBR(Sscreen_height);
    rep_ADD_SUBR(Ssync_server);
    rep_ADD_SUBR(Sdelete_x_property);
    rep_ADD_SUBR(Slist_x_properties);
    rep_ADD_SUBR(Sget_x_property);
    rep_ADD_SUBR(Sset_x_property);
    rep_ADD_SUBR(Screate_window);
    rep_ADD_SUBR(Sx_atom);
    rep_ADD_SUBR(Sx_atom_name);
    rep_ADD_SUBR(Sroot_window_id);
    rep_ADD_SUBR(Shead_count);
    rep_ADD_SUBR(Sfind_head);
    rep_ADD_SUBR(Shead_dimensions);
    rep_ADD_SUBR(Shead_offset);
    rep_ADD_SUBR(Sdisplay_message);
    rep_pop_structure (tem);

    tem = rep_push_structure ("sawfish.wm.events");
    rep_ADD_SUBR(Sgrab_pointer);
    rep_ADD_SUBR(Sungrab_pointer);
    rep_ADD_SUBR(Sgrab_keyboard);
    rep_ADD_SUBR(Sungrab_keyboard);
    rep_ADD_SUBR(Ssend_client_message);
    rep_pop_structure (tem);

    rep_INTERN(root);
    rep_INTERN_SPECIAL(after_restacking_hook);
    rep_INTERN(position);
    rep_INTERN(spacing);
    rep_INTERN(head);

    rep_mark_static (&message.text);
    rep_mark_static (&message.fg);
    rep_mark_static (&message.bg);
    rep_mark_static (&message.font);
    rep_mark_static (&message.justify);

    update_xinerama_info ();
}

void
functions_kill (void)
{
    if (message_win != 0)
	XDestroyWindow (dpy, message_win);
}
