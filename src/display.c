/* display.c -- display handling
   $Id$ */

#include "sawmill.h"
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <string.h>
#include <assert.h>

#ifdef HAVE_UNIX
# ifdef HAVE_FCNTL_H
#  include <fcntl.h>
# endif
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif
#endif

char *prog_name;
Display *dpy;
int screen_num, screen_width, screen_height, screen_depth;
Visual *screen_visual;
Colormap screen_cmap;
Window root_window;

/* some atoms that may be useful.. */
Atom xa_wm_state, xa_wm_change_state, xa_wm_protocols, xa_wm_delete_window,
    xa_wm_colormap_windows, xa_wm_take_focus;


/* X error handlers */

static void
print_error (XErrorEvent *ev)
{
    char buf[256];
    XGetErrorText(dpy, ev->error_code, buf, sizeof (buf));
    fprintf(stderr, "X Error: %s\n", buf);
    fprintf(stderr, "  Request Major code: %d\n", ev->request_code);
    fprintf(stderr, "  Request Minor code: %d\n", ev->minor_code);
    fprintf(stderr, "  ResourceId 0x%x\n", (u_int)ev->resourceid);
}

/* General error handler. Probably due to lag between windows being
   killed and us receiving DestroyNotify events */
static int
error_handler (Display *dpy, XErrorEvent *ev)
{
    Lisp_Window *w;
#ifdef DEBUG
    print_error (ev);
#endif
    w = find_window_by_id (ev->resourceid);
    if (w != 0)
    {
	DB(("error_handler (%s)\n", w->name));
	if (w == focus_window)
	    focus_on_window (0);
	/* call the hook before removing it so that it's still windowp */
	Fcall_hook (Qdestroy_notify_hook, Fcons (rep_VAL(w), Qnil), Qnil);
	if (w->id)
	    remove_window (w, Qt);
	return 0;			/* ?? */
    }
    else
    {
#ifndef DEBUG
	print_error (ev);
#endif
#if 0
	longjmp (clean_exit_jmp_buf, ec_exit);
#endif
	return 0;
    }
}

/* Installed whilst trying to set the root window event mask */
static int
error_other_wm (Display *dpy, XErrorEvent *ev)
{
    fputs ("You may only run one window manager\n", stderr);
    exit (1);
}



/* Called from main(). */
bool
sys_init(char *program_name)
{
    char *display_name = 0;
    repv opt;

#ifdef HAVE_UNIX
    if (rep_SYM(Qbatch_mode)->value == Qnil)
	setpgid (0, 0);
#endif

    prog_name = program_name;

    if (rep_get_option ("--display", &opt))
	display_name = strdup (rep_STR(opt));
    if (rep_get_option ("--name", &opt))
	prog_name = strdup (rep_STR(opt));

    if (display_name == 0)
	display_name = getenv("DISPLAY");

    dpy = XOpenDisplay(display_name);
    if(dpy != 0)
    {
	rep_register_input_fd (ConnectionNumber(dpy), handle_sync_input);
	screen_num = DefaultScreen(dpy);
	root_window = RootWindow(dpy, screen_num);
	screen_depth = DefaultDepth(dpy, screen_num);
	screen_visual = DefaultVisual(dpy, screen_num);
	screen_width = DisplayWidth(dpy, screen_num);
	screen_height = DisplayHeight(dpy, screen_num);
	screen_cmap = DefaultColormap(dpy, screen_num);

	xa_wm_state = XInternAtom (dpy, "WM_STATE", False);
	xa_wm_change_state = XInternAtom (dpy, "WM_CHANGE_STATE", False);
	xa_wm_protocols = XInternAtom (dpy, "WM_PROTOCOLS", False);
	xa_wm_delete_window = XInternAtom (dpy, "WM_DELETE_WINDOW", False);
	xa_wm_colormap_windows = XInternAtom (dpy, "WM_COLORMAP_WINDOWS", False);
	xa_wm_take_focus = XInternAtom (dpy, "WM_TAKE_FOCUS", False);

	if(rep_SYM(Qbatch_mode)->value == Qnil)
	{
	    XSetErrorHandler (error_other_wm);
	    XSelectInput (dpy, root_window,
			  SubstructureRedirectMask | ButtonPressMask
			  | ButtonReleaseMask | KeyPressMask
			  | ButtonMotionMask | PointerMotionHintMask
			  | EnterWindowMask | LeaveWindowMask);
	    XSync (dpy, False);
	    XSetErrorHandler (error_handler);
	}

	if (rep_get_option ("--sync", 0))
	    XSynchronize (dpy, True);

	/* XXX If I don't do this all the events that are created by
	   XXX the window initialiation are ignored until the next
	   XXX new event arrives!? */
        rep_mark_input_pending (ConnectionNumber(dpy));

	return TRUE;
    }
    else
    {
	fprintf(stderr, "sawmill: Can't open display: %s\n",
		display_name ? display_name : "");
	return FALSE;
    }
}

void
sys_kill (void)
{
    XSetInputFocus (dpy, PointerRoot, 0, last_event_time);
    XSync (dpy, False);
    XCloseDisplay (dpy);
}


/* utilities */

/***************************************************************************
 *
 * ICCCM Client Messages - Section 4.2.8 of the ICCCM dictates that all
 * client messages will have the following form:
 *
 *     event type       ClientMessage
 *     message type     _XA_WM_PROTOCOLS
 *     window           tmp->w
 *     format           32
 *     data[0]          message atom
 *     data[1]          time stamp
 *
 ****************************************************************************/
void
send_client_message (Window w, Atom a, Time time)
{
  XClientMessageEvent ev;
  
  ev.type = ClientMessage;
  ev.window = w;
  ev.message_type = xa_wm_protocols;
  ev.format = 32;
  ev.data.l[0] = a;
  ev.data.l[1] = time;
  XSendEvent (dpy, w, False, 0L, (XEvent *) &ev);
}
