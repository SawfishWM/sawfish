/* gnome.c -- minimal GNOME compliance
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

/* todo:

   * this assumes knowledge about the workspace implementation; not
     a good idea.. */

#include "sawmill.h"
#include <X11/Xutil.h>
#include <X11/Xutil.h>
#include <alloca.h>

#define WIN_STATE_STICKY (1 << 0)

DEFSYM(gnome, "gnome");
DEFSYM(enter_workspace_hook, "enter-workspace-hook");
DEFSYM(add_workspace_hook, "add-workspace-hook");
DEFSYM(delete_workspace_hook, "delete-workspace-hook");
DEFSYM(add_to_workspace_hook, "add-to-workspace-hook");
DEFSYM(remove_from_workspace_hook, "remove-from-workspace-hook");
DEFSYM(ws_workspaces, "ws-workspaces");
DEFSYM(ws_current_workspace, "ws-current-workspace");
DEFSYM(select_workspace, "select-workspace");
DEFSYM(ignored_window_names, "ignored-window-names");
DEFSYM(sticky_window_names, "sticky-window-names");
DEFSYM(ignored, "ignored");
DEFSYM(sticky, "sticky");
DEFSYM(toggle_window_sticky, "toggle-window-sticky");

DEFSTRING(gnome_windows, "^(gmc|panel)$");

static Window gnome_id_win;

static Atom xa_win_supporting_wm_check, xa_win_client_list, xa_win_protocols,
    xa_win_workspace, xa_win_workspace_count, xa_win_desktop_button_proxy,
    xa_win_state;


/* client list support */

DEFUN("gnome-set-client-list", Fgnome_set_client_list,
      Sgnome_set_client_list, (void), rep_Subr0)
{
    Lisp_Window *w;
    int count, i;
    Window *clients;

    count = 0;
    for (w = window_list; w != 0; w = w->next)
    {
	repv tem;
	if (w->id == 0 || !w->mapped)
	    continue;
	tem = Fwindow_get (rep_VAL(w), Qignored);
	if (tem && tem != Qnil)
	    continue;
	count++;
    }

    clients = alloca (count * sizeof (Window));
    for (w = window_list, i = 0; w != 0; w = w->next)
    {
	repv tem;
	if (w->id == 0 || !w->mapped)
	    continue;
	tem = Fwindow_get (rep_VAL(w), Qignored);
	if (tem && tem != Qnil)
	    continue;
	clients[i++] = w->id;
    }

    XChangeProperty (dpy, root_window, xa_win_client_list, XA_CARDINAL,
		     32, PropModeReplace, (u_char *)clients, count);
    return Qt;
}


/* workspace support */

DEFUN("gnome-set-workspace", Fgnome_set_workspace,
      Sgnome_set_workspace, (void), rep_Subr0)
{
    repv spaces = Fsymbol_value (Qws_workspaces, Qt);
    repv current = Fsymbol_value (Qws_current_workspace, Qt);
    int total = 0, active = 0;

    while (rep_CONSP(spaces) && !rep_INTERRUPTP)
    {
	repv windows;
	if (rep_CAR(spaces) == current)
	    active = total;
	windows = rep_CDR(rep_CAR(spaces));
	while (rep_CONSP(windows) && !rep_INTERRUPTP)
	{
	    repv win = rep_CAR(windows);
	    if (WINDOWP(win) && VWIN(win)->id != 0)
	    {
		XChangeProperty (dpy, VWIN(win)->id, xa_win_workspace,
				 XA_CARDINAL, 32, PropModeReplace,
				 (u_char *)&total, 1);
	    }
	    windows = rep_CDR(windows);
	}
	total++;
	spaces = rep_CDR(spaces);
	rep_TEST_INT;
    }

    XChangeProperty (dpy, root_window, xa_win_workspace, XA_CARDINAL,
		     32, PropModeReplace, (u_char *)&active, 1);
    XChangeProperty (dpy, root_window, xa_win_workspace_count, XA_CARDINAL,
		     32, PropModeReplace, (u_char *)&total, 1);
    return Qt;
}


/* client state support */

DEFUN("gnome-set-client-state", Fgnome_set_client_state,
      Sgnome_set_client_state, (repv win), rep_Subr1)
{
    u_int state = 0;
    repv tem;
    rep_DECLARE1(win, WINDOWP);

    tem = Fwindow_get (win, Qsticky);
    if (tem != Qnil)
	state |= WIN_STATE_STICKY;

    XChangeProperty (dpy, VWIN(win)->id, xa_win_state, XA_CARDINAL,
		     32, PropModeReplace, (u_char *)&state, 1);
    return win;
}

DEFUN("gnome-honour-client-state", Fgnome_honour_client_state,
      Sgnome_honour_client_state, (repv win), rep_Subr1)
{
    Atom actual_type;
    int format;
    u_long nitems, bytes_after;
    u_char *prop = 0;
    rep_DECLARE1(win, WINDOWP);
    if (XGetWindowProperty (dpy, VWIN(win)->id, xa_win_state, 0, 1,
			    False, XA_CARDINAL, &actual_type,
			    &format, &nitems, &bytes_after, &prop) != None
        && format == 32 && actual_type == XA_CARDINAL)
    {
	u_int state = *(u_int *)prop;
	repv tem;
	if (state & WIN_STATE_STICKY)
	{
	    tem = Fwindow_get (win, Qsticky);
	    if (tem == Qnil)
		rep_call_lisp1 (Qtoggle_window_sticky, win);
	}
    }
    if (prop != 0)
	XFree (prop);
    return win;
}
	

/* GNOME client message handling */

DEFUN("gnome-client-message-handler", Fgnome_client_message_handler,
      Sgnome_client_message_handler, (repv win), rep_Subr1)
{
    /* current_x_event is the event that was actually received */
    Lisp_Window *w;
    if (current_x_event == 0)
	return Qnil;
    w = find_window_by_id (current_x_event->xclient.window);
    if (current_x_event->xclient.message_type == xa_win_workspace)
    {
	int desk = current_x_event->xclient.data.l[0];
	rep_call_lisp1 (Qselect_workspace, rep_MAKE_INT(desk));
	return Qt;
    }
    else if (w != 0 && current_x_event->xclient.message_type == xa_win_state)
    {
	u_int mask = current_x_event->xclient.data.l[0];
	u_int values = current_x_event->xclient.data.l[1];
	if (mask & WIN_STATE_STICKY)
	{
	    repv tem = Fwindow_get (rep_VAL(w), Qsticky);
	    if ((tem == Qnil && (values & WIN_STATE_STICKY))
		|| (tem != Qnil && !(values & WIN_STATE_STICKY)))
	    {
		rep_call_lisp1 (Qtoggle_window_sticky, rep_VAL(w));
	    }
	}
	Fgnome_set_client_state (rep_VAL(w));
    }
    return Qnil;
}


/* event proxying */

static bool
event_proxy (XEvent *ev, long code, long mods)
{
    if (ev->xany.window == root_window
	&& (ev->type == ButtonPress || ev->type == ButtonRelease))
    {
	if (ev->type == ButtonPress)
	    XUngrabPointer (dpy, CurrentTime);
	XSendEvent (dpy, gnome_id_win, False, SubstructureNotifyMask, ev);
	return TRUE;
    }
    else
	return FALSE;
}


/* initialisation */

repv rep_dl_feature;
rep_xsubr *rep_dl_subrs[] = {
    &Sgnome_set_client_list,
    &Sgnome_set_workspace,
    &Sgnome_client_message_handler,
    0
};

repv
rep_dl_init(repv file_name)
{
    u_int val;
    Atom prot[4];

    rep_INTERN(gnome);
    rep_dl_feature = Qgnome;
    rep_INTERN(enter_workspace_hook);
    rep_INTERN(add_workspace_hook);
    rep_INTERN(delete_workspace_hook);
    rep_INTERN(add_to_workspace_hook);
    rep_INTERN(remove_from_workspace_hook);
    rep_INTERN(ws_workspaces);
    rep_INTERN(ws_current_workspace);
    rep_INTERN(select_workspace);
    rep_INTERN(ignored_window_names);
    rep_INTERN(sticky_window_names);
    rep_INTERN(ignored);
    rep_INTERN(sticky);
    rep_INTERN(toggle_window_sticky);

    if (rep_VOIDP(rep_SYM(Qignored_window_names)->value))
	rep_SYM(Qignored_window_names)->value = Qnil;
    if (rep_VOIDP(rep_SYM(Qsticky_window_names)->value))
	rep_SYM(Qsticky_window_names)->value = Qnil;

    rep_SYM(Qignored_window_names)->value
	 = Fcons (rep_VAL(&gnome_windows),
		  rep_SYM(Qignored_window_names)->value);
    rep_SYM(Qsticky_window_names)->value
	 = Fcons (rep_VAL(&gnome_windows),
		  rep_SYM(Qsticky_window_names)->value);

    xa_win_supporting_wm_check = XInternAtom (dpy, "_WIN_SUPPORTING_WM_CHECK", False);
    xa_win_protocols = XInternAtom (dpy, "_WIN_PROTOCOLS", False);
    xa_win_client_list = XInternAtom (dpy, "_WIN_CLIENT_LIST", False);
    xa_win_workspace = XInternAtom (dpy, "_WIN_WORKSPACE", False);
    xa_win_workspace_count = XInternAtom (dpy, "_WIN_WORKSPACE_COUNT", False);
    xa_win_desktop_button_proxy = XInternAtom (dpy, "_WIN_DESKTOP_BUTTON_PROXY", False);
    xa_win_state = XInternAtom (dpy, "_WIN_STATE", False);

    gnome_id_win = XCreateSimpleWindow (dpy, root_window,
					-200, -200, 5, 5, 0, 0, 0);
    val = gnome_id_win;
    XChangeProperty (dpy, root_window, xa_win_supporting_wm_check,
		     XA_CARDINAL, 32, PropModeReplace, (u_char *)&val, 1);
    XChangeProperty (dpy, gnome_id_win, xa_win_supporting_wm_check,
		     XA_CARDINAL, 32, PropModeReplace, (u_char *)&val, 1);

    XChangeProperty (dpy, root_window, xa_win_desktop_button_proxy,
		     XA_CARDINAL, 32, PropModeReplace, (u_char *)&val, 1);
    XChangeProperty (dpy, gnome_id_win, xa_win_desktop_button_proxy,
		     XA_CARDINAL, 32, PropModeReplace, (u_char *)&val, 1);

    prot[0] = xa_win_client_list;
    prot[1] = xa_win_workspace;
    prot[2] = xa_win_workspace_count;
    prot[3] = xa_win_state;
    XChangeProperty (dpy, root_window, xa_win_protocols,
		     XA_ATOM, 32, PropModeReplace, (u_char *)prot, 4);

    /* enlightenment doesn't clean up after itself.. */
    XDeleteProperty (dpy, root_window,
		     XInternAtom (dpy, "_WIN_AREA", False));
    XDeleteProperty (dpy, root_window,
		     XInternAtom (dpy, "_WIN_AREA_COUNT", False));
    XDeleteProperty (dpy, root_window,
		     XInternAtom (dpy, "_WIN_WORKSPACE_NAMES", False));

    rep_ADD_SUBR(Sgnome_set_client_list);
    rep_ADD_SUBR(Sgnome_set_workspace);
    rep_ADD_SUBR(Sgnome_client_message_handler);

    add_hook (Qenter_workspace_hook, rep_VAL(&Sgnome_set_workspace));
    add_hook (Qadd_workspace_hook, rep_VAL(&Sgnome_set_workspace));
    add_hook (Qdelete_workspace_hook, rep_VAL(&Sgnome_set_workspace));
    add_hook (Qadd_to_workspace_hook, rep_VAL(&Sgnome_set_workspace));
    add_hook (Qremove_from_workspace_hook, rep_VAL(&Sgnome_set_workspace));
    add_hook (Qadd_window_hook, rep_VAL(&Sgnome_set_client_list));
    add_hook (Qdestroy_notify_hook, rep_VAL(&Sgnome_set_client_list));
    add_hook (Qmap_notify_hook, rep_VAL(&Sgnome_set_client_list));
    add_hook (Qunmap_notify_hook, rep_VAL(&Sgnome_set_client_list));
    add_hook (Qclient_message_hook, rep_VAL(&Sgnome_client_message_handler));
    add_hook (Qwindow_state_change_hook, rep_VAL(&Sgnome_set_client_state));
    add_hook (Qadd_window_hook, rep_VAL(&Sgnome_honour_client_state));
    event_proxy_fun = event_proxy;

    return Qt;
}

void
rep_dl_kill(void)
{
    event_proxy_fun = 0;

    XDestroyWindow (dpy, gnome_id_win);
    XDeleteProperty (dpy, root_window, xa_win_supporting_wm_check);
    XDeleteProperty (dpy, root_window, xa_win_protocols);
    XDeleteProperty (dpy, root_window, xa_win_client_list);
    XDeleteProperty (dpy, root_window, xa_win_workspace);
    XDeleteProperty (dpy, root_window, xa_win_workspace_count);
}
