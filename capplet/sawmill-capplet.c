/* sawmill-capplet.c -- embed sawmill-ui in the gnome control center
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

/* this was initially based on mouse-properties.c */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <build.h>

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#include <capplet-widget.h>
#include <gdk/gdkprivate.h>

static GtkWidget *ui_socket;
static GtkWidget *capplet;

static int ui_pid;
static int ui_stdin[2];
static int ui_stdout[2];

static char *group;

char *ui_argv[] = { "sawmill-ui",
		    "--notebook",
		    "--socket-id", 0,
		    "--group", 0,
		    0 };
#define UI_ARGV_SOCKET_SLOT 3
#define UI_ARGV_GROUP_SLOT 5



static guint32
gdk_window_xid (GdkWindow *win)
{
    GdkWindowPrivate *pri = (GdkWindowPrivate *)win;
    return pri->xwindow;
}


/* communicating with sawmill-ui */

static int
ui_command (char *str)
{
    u_char ret;
    write (ui_stdin[1], str, strlen(str));
    read (ui_stdout[0], &ret, 1);
    return ret;
}

static void
ui_output_callback (gpointer data, gint fd, GdkInputCondition cond)
{
    char out;
    if (read (ui_stdout[0], &out, 1) == 1)
    {
	switch (out)
	{
	case 'c':
	    capplet_widget_state_changed (CAPPLET_WIDGET (capplet), TRUE);
	    break;
	}
    }
}


/* capplet button callbacks */

static void
sawmill_help (void)
{
}

static void
sawmill_apply (void)
{
    ui_command("apply\n");
}

static void
sawmill_revert (void)
{
    ui_command ("revert\n");
}

static void
sawmill_ok (void)
{
    ui_command ("ok\n");
}

static void
sawmill_cancel (void)
{
    ui_command ("cancel\n");
    gtk_widget_destroy (capplet);
}


/* initialisation */

static void
sawmill_setup (void)
{
    ui_socket = gtk_socket_new ();
    capplet = capplet_widget_new ();
    
    gtk_signal_connect (GTK_OBJECT (capplet), "help",
			GTK_SIGNAL_FUNC (sawmill_help), NULL);
    gtk_signal_connect (GTK_OBJECT (capplet), "try",
			GTK_SIGNAL_FUNC (sawmill_apply), NULL);
    gtk_signal_connect (GTK_OBJECT (capplet), "revert",
			GTK_SIGNAL_FUNC (sawmill_revert), NULL);
    gtk_signal_connect (GTK_OBJECT (capplet), "ok",
			GTK_SIGNAL_FUNC (sawmill_ok), NULL);
    gtk_signal_connect (GTK_OBJECT (capplet), "cancel",
			GTK_SIGNAL_FUNC (sawmill_cancel), NULL);
 
    gtk_container_add (GTK_CONTAINER (capplet), ui_socket);
    gtk_widget_show_all (capplet);

    /* now fork the sawmill-ui script */
    pipe (ui_stdin);
    pipe (ui_stdout);
    switch (ui_pid = fork ())
    {
	char buf[64];

    case -1:
	exit (5);

    case 0:				/* child */
	dup2 (ui_stdin[0], 0);
	close (ui_stdin[1]);
	dup2 (ui_stdout[1], 1);
	close (ui_stdout[0]);
	sprintf (buf, "%d", gdk_window_xid (ui_socket->window));
	ui_argv[UI_ARGV_SOCKET_SLOT] = buf;
	if (group != 0)
	    ui_argv[UI_ARGV_GROUP_SLOT] = group;
	else
	    ui_argv[UI_ARGV_GROUP_SLOT-1] = 0;
	execvp (ui_argv[0], ui_argv);
	exit (10);

    default:				/* parent */
	close (ui_stdin[0]);
	close (ui_stdout[1]);
	gdk_input_add (ui_stdout[0], GDK_INPUT_READ,
		       (GdkInputFunction) ui_output_callback, 0);
    }
}


/* entry point */

int
main (int argc, char **argv)
{
    GnomeClient *client = NULL;
    GnomeClientFlags flags;
    gchar *session_args[3];
    int token, init_results;

    static struct poptOption options[] = {
	{ "sawmill-group", 0, POPT_ARG_STRING, &group,
	  0, "Sawmill customization group", "GROUP" },
	{ 0, 0, 0, 0, 0 }
    };

#if 0
    bindtextdomain (PACKAGE, GNOMELOCALEDIR);
    textdomain (PACKAGE);
#endif

    init_results = gnome_capplet_init("sawmill-properties", SAWMILL_VERSION,
				      argc, argv, options, 0, NULL);

    if (init_results < 0) {
	g_warning ("an initialization error occurred while "
		   "starting 'sawmill-properties-capplet'.\n"
		   "aborting...\n");
	exit (1);
    }

    client = gnome_master_client ();
    flags = gnome_client_get_flags(client);

    if (flags & GNOME_CLIENT_IS_CONNECTED) {
	token = gnome_startup_acquire_token("GNOME_SAWMILL_PROPERTIES",
					    gnome_client_get_id(client));

	if (token) {
	    session_args[0] = argv[0];
	    session_args[1] = "--init-session-settings";
	    session_args[2] = NULL;
	    gnome_client_set_priority (client, 20);
	    gnome_client_set_restart_style (client, GNOME_RESTART_ANYWAY);
	    gnome_client_set_restart_command (client, 2, session_args);
	}
	else 
	    gnome_client_set_restart_style (client, GNOME_RESTART_NEVER);

	gnome_client_flush (client);
    }
    else
	token = 1;

    if (init_results != 1) {
	sawmill_setup ();
	capplet_gtk_main ();
	if (ui_pid != 0)
	    waitpid (ui_pid, 0, 0);
    }
    return 0;
}
