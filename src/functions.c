/* functions.c -- useful window manager Lisp functions
   $Id$ */

#include "sawmill.h"

static int server_grabs;

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
    rep_DECLARE1(win, WINDOWP);
    send_client_message (VWIN(win)->id, xa_wm_delete_window, last_event_time);
    return win;
}

DEFUN_INT("destroy-window", Fdestroy_window, Sdestroy_window, (repv win), rep_Subr1, "f") /*
::doc:Sdestroy-window::
destroy-window WINDOW
::end:: */
{
    rep_DECLARE1(win, WINDOWP);
    XKillClient (dpy, VWIN(win)->id);
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
	XSync (dpy, False);
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
	XSync (dpy, False);
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
    XSync (dpy, False);
    return Qt;
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
}
