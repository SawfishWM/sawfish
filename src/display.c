/* display.c -- display handling
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

#include "sawfish.h"
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/extensions/shape.h>
#include <X11/Xproto.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <ctype.h>

#ifdef HAVE_UNIX
# ifdef HAVE_FCNTL_H
#  include <fcntl.h>
# endif
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif
# ifdef HAVE_SYS_UTSNAME_H
#  include <sys/utsname.h>
# endif
# include <netdb.h>
#endif

char *visual_name;
Display *dpy;
int screen_num, screen_width, screen_height;
Window root_window, no_focus_window;
int shape_event_base, shape_error_base;

Visual *preferred_visual;
int preferred_depth;

/* some atoms that may be useful.. */
Atom xa_wm_state, xa_wm_change_state, xa_wm_protocols, xa_wm_delete_window,
  xa_wm_colormap_windows, xa_wm_take_focus, xa_compound_text,
  xa_wm_net_name, xa_wm_net_icon_name, xa_utf8_string;

DEFSYM(display_name, "display-name");
DEFSYM(canonical_display_name, "canonical-display-name");

/* X error handlers */

/* General error handler. Probably due to lag between windows being
   killed and us receiving DestroyNotify events */
static int
error_handler (Display *dpy, XErrorEvent *ev)
{
    Lisp_Window *w;

    if (ev->resourceid != 0
	&& (ev->error_code == BadWindow || ev->error_code == BadDrawable))
    {
	w = x_find_window_by_id (ev->resourceid);

	if (w != NULL)
	{
	    DB(("error_handler (%s)\n", rep_STR(w->name)));

	    if (!WINDOW_IS_GONE_P (w))
           {
               /* don't unmap a window that had send an X_ConfigureWindow request */
               if(
                   /*     ev->type == 0 what is the "type" ? but I've seen that type is always 0 */
                   /*&&*/ ev->error_code==BadWindow /* the window is bad, because it is not configured yet */
                     &&   ev->request_code==X_ConfigureWindow
                     &&   ev->minor_code==0 /* X_ConfigureWindow is not in an Xlib extension, so it must be 0 */
               )
               {
                   return 0;
               } else
               {
                   remove_window (w, TRUE, TRUE);
               }
           }

	    /* so we call emit_pending_destroys () at some point */
	    rep_mark_input_pending (ConnectionNumber (dpy)); 
	}
    }

    return 0;
}

/* Installed whilst trying to set the root window event mask */
static int
error_other_wm (Display *dpy, XErrorEvent *ev)
{
    fputs ("You may only run one window manager\n", stderr);
    exit (1);
}


static char *
canonical_host (char *host)
{
    static char buf[256];
    char *ptr;

    /* check that the name is fully qualified */
    if (!strchr (host, '.'))
    {
	struct hostent *h = gethostbyname (host);
	if (h != 0)
	{
	    if (!strchr (h->h_name, '.'))
	    {
		char **aliases = h->h_aliases;
		while (*aliases && !strchr (*aliases, '.'))
		    aliases++;
		host = *aliases ? *aliases : h->h_name;
	    }
	    else
		host = h->h_name;
	}
    }

    ptr = buf;
    while (*host != 0)
    {
	*ptr++ = tolower (*host);
	host++;
    }
    return buf;
}

static char *
canonical_display (char *name)
{
    static char buf[256];
    char *ptr = buf;
    if (strncmp ("unix:", name, 5) == 0)
	name += 4;
    if (*name == ':')
    {
	repv host = Fsystem_name ();
	if (host && rep_STRINGP(host))
	    strcpy (ptr, rep_STR(host));
	else
	    *ptr = 0;
	ptr += strlen (ptr);
    }
    else
    {
	char *fq;
	while (*name && *name != ':')
	    *ptr++ = *name++;
	*ptr = 0;
	fq = canonical_host (buf);
	if (fq != buf)
	{
	    strcpy (buf, fq);
	    ptr = buf + strlen (buf);
	}
    }
    *ptr++ = *name++;
    while (*name && *name != '.')
	*ptr++ = *name++;
    if (*name == 0)
	strcpy (ptr, ".0");
    else
	strcpy (ptr, name);
    return buf;
}


static void
redisplay (void)
{
    commit_queued_reshapes ();

    /* round-trip requests swallow any pending events.. */
    if (XPending (dpy) > 0)
	rep_mark_input_pending (ConnectionNumber (dpy));
}

static void
beep(void)
{
    XBell(dpy, 0);
}

static void
choose_visual (void)
{
    int id = 0;
    if (visual_name != 0)
    {
	if (!strcasecmp ("StaticGray", visual_name))
	    id = StaticGray;
	else if (!strcasecmp ("StaticColor", visual_name))
	    id = StaticColor;
	else if (!strcasecmp ("TrueColor", visual_name))
	    id = TrueColor;
	else if (!strcasecmp ("GrayScale", visual_name)
		 || !strcasecmp ("GreyScale", visual_name))
	    id = GrayScale;
	else if (!strcasecmp ("PseudoColor", visual_name))
	    id = PseudoColor;
	else if (!strcasecmp ("DirectColor", visual_name))
	    id = DirectColor;
    }
    if (id != 0 || preferred_depth != 0)
    {
	XVisualInfo in, *out;
	int mask = VisualScreenMask, n_out;
	in.screen = screen_num;
	if (id != 0)
	{
	    in.class = id;
	    mask |= VisualClassMask;
	}
	out = XGetVisualInfo (dpy, mask, &in, &n_out);
	if (out != 0)
	{
	    int i, best = -1;
	    if (preferred_depth > 0)
	    {
		/* Look for a visual with the preferred depth. */
		for (i = 0; i < n_out; i++)
		{
		    if (out[i].depth == preferred_depth
			&& (best < 0
			    || out[i].colormap_size > out[best].colormap_size))
		    {
			best = i;
		    }
		}
	    }
	    if (best < 0)
	    {
		/* Else find the deepest visual of this type. */
		for (i = 0, best = 0; i < n_out; i++)
		{
		    if (out[i].depth > out[best].depth
			|| (out[i].depth == out[best].depth
			    && out[i].colormap_size > out[best].colormap_size))
		    {
			best = i;
		    }
		}
	    }
	    if (best >= 0 && best < n_out)
	    {
		preferred_visual = out[best].visual;
		preferred_depth = out[best].depth;
	    }
	    XFree (out);
	}
    }
    if (preferred_visual == 0)
    {
	if (visual_name != 0)
	    fprintf (stderr, "warning: using default visual\n");
	preferred_visual = DefaultVisual (dpy, screen_num);
	preferred_depth = DefaultDepth (dpy, screen_num);
    }
}

/* Called from main(). */
bool
sys_init(char *program_name)
{
    char *display_name = 0, *prog_name = 0;
    repv opt;

#ifdef HAVE_UNIX
    if (!batch_mode_p ())
	setpgid (0, 0);
#endif

    prog_name = program_name;
    if (rep_get_option ("--name", &opt))
	prog_name = strdup (rep_STR(opt));

    rep_INTERN_SPECIAL(display_name);
    rep_INTERN_SPECIAL(canonical_display_name);
    Fset (Qdisplay_name, rep_null_string ());
    Fset (Qcanonical_display_name, rep_null_string ());

    if(!batch_mode_p ())
    {
	if (rep_get_option ("--display", &opt))
	    display_name = strdup (rep_STR(opt));
	if (rep_get_option ("--visual", &opt))
	    visual_name = strdup (rep_STR(opt));
	if (rep_get_option ("--depth", &opt))
	    preferred_depth = atoi (rep_STR (opt));

	if (display_name == 0)
	    display_name = getenv("DISPLAY");

	dpy = XOpenDisplay(display_name);
	if(dpy != 0)
	{
	    Fset (Qdisplay_name, rep_string_dup (display_name));
	    Fset (Qcanonical_display_name,
		  rep_string_dup (canonical_display (display_name)));
	    rep_register_input_fd (ConnectionNumber(dpy), handle_sync_input);
	    screen_num = DefaultScreen(dpy);
	    root_window = RootWindow(dpy, screen_num);
	    screen_width = DisplayWidth(dpy, screen_num);
	    screen_height = DisplayHeight(dpy, screen_num);
	    choose_visual ();

	    xa_wm_state = XInternAtom (dpy, "WM_STATE", False);
	    xa_wm_change_state = XInternAtom (dpy, "WM_CHANGE_STATE", False);
	    xa_wm_protocols = XInternAtom (dpy, "WM_PROTOCOLS", False);
	    xa_wm_delete_window = XInternAtom (dpy, "WM_DELETE_WINDOW", False);
	    xa_wm_colormap_windows = XInternAtom (dpy, "WM_COLORMAP_WINDOWS", False);
	    xa_wm_take_focus = XInternAtom (dpy, "WM_TAKE_FOCUS", False);
	    xa_compound_text = XInternAtom (dpy, "COMPOUND_TEXT", False);
	    xa_wm_net_name = XInternAtom (dpy, "_NET_WM_NAME", False);
	    xa_wm_net_icon_name = XInternAtom (dpy, "_NET_WM_ICON_NAME", False);
	    xa_utf8_string = XInternAtom (dpy, "UTF8_STRING", False);

	    if (!XShapeQueryExtension (dpy, &shape_event_base,
				       &shape_error_base))
	    {
		fprintf (stderr, "sawfish: your X server doesn't suppot the SHAPE extension; aborting\n");
		return FALSE;
	    }

	    // This is used to prevent two Sawfish running.
	    // (Not sure for other WMs.)
	    XSetErrorHandler (error_other_wm);
	    XSelectInput (dpy, root_window, ROOT_EVENTS);
	    XSync (dpy, False);
	    XSetErrorHandler (error_handler);

	    {
		/* Create the mapped-but-invisible window that is given
		   the focus when no other window has it. */
		XSetWindowAttributes attr;
		/* this value is assumed in events.c:get_server_timestamp */
		attr.event_mask = KeyPressMask;
		attr.override_redirect = True;
		no_focus_window = XCreateWindow (dpy, root_window,
						 -10, -10, 10, 10, 0, 0,
						 InputOnly, CopyFromParent,
						 CWEventMask
						 | CWOverrideRedirect,
						 &attr);
		XMapWindow (dpy, no_focus_window);
	    }

	    /* This should _never_ be used in Real Life; only for
	       debugging. Sawfish tries to work out when the error
	       handle might be called (i.e. after any XGet, XQuery, XFetch
	       type function) and then call emit_pending_destroys ()
	       as soon as possible, so that there's as small as possible
	       delay between the window being destroyed and the hook
	       being called.. */
	    if (rep_get_option ("--sync", 0))
		XSynchronize (dpy, True);

	    /* If I don't do this all the events that are created by
	       the window initialiation are ignored until the next
	       new event arrives (because of the XSync calls above) */
	    rep_mark_input_pending (ConnectionNumber(dpy));

	    rep_redisplay_fun = redisplay;
	    rep_beep_fun = beep;

	    return TRUE;
	}
	else
	{
	    fprintf(stderr, "sawfish: Can't open display: %s\n",
		    display_name ? display_name : "");
	    return FALSE;
	}
    }
    else
	return TRUE;
}

void
sys_kill (void)
{
    if(!batch_mode_p ())
    {
	XSetInputFocus (dpy, PointerRoot, 0, last_event_time);
	XDestroyWindow (dpy, no_focus_window);
	XCloseDisplay (dpy);
    }
}

/* utilities */

repv
x_atom_symbol (Atom atom)
{
    char *name = XGetAtomName (dpy, atom);
    if (name != 0)
    {
	repv sym = Fintern (rep_string_dup (name), rep_obarray);
	XFree (name);
	return sym;
    }
    else
	return Qnil;
}

Window
x_win_from_arg (repv arg)
{
    if (arg == Qroot)
	return root_window;
    else if (WINDOWP(arg))
	return VWIN(arg)->id;
    else if (rep_INTEGERP(arg))
	return rep_get_long_uint (arg);
    else
	return 0;
}

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

#if XlibSpecificationRelease < 6
Status
XGetAtomNames (Display *dpy, Atom *atoms, int count, char **names_ret)
{
    int i;
    for (i = 0; i < count; i++)
    {
	names_ret[i] = XGetAtomName (dpy, atoms[i]);
	if (names_ret[i] == 0)
	    break;
    }
    if (i == count)
	return 1;
    for (i--; i >= 0; i--)
	XFree (names_ret[i]);
    return 0;
}
#endif

void
db_printf(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    rep_db_vprintf(rep_common_db, fmt, args);
    va_end(args);
}
