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

#include "../build.h"
#include "../src/libclient.h"

#include <stdio.h>
#include <signal.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#include <capplet-widget.h>
#include <gdk/gdkx.h>

static GtkWidget *ui_socket;
static GtkWidget *capplet;

static int ui_pid;
static int ui_stdin[2];
static int ui_stdout[2];
static guint ui_handler_id;

static char *group;
static gboolean no_flatten = FALSE;


/* communicating with sawfish-ui */

#define X_IO(op, fd, buf, len)				\
	char *buf__ = (char *)buf;			\
	int todo__ = len;				\
	while(todo__ > 0) {				\
	    int this__ = op (fd, buf__, todo__);	\
	    if(this__ < 0) {				\
		if (errno != EINTR)			\
		    return -1;				\
	    }						\
	    else if(this__ == 0)			\
		break;					\
	    else {					\
		todo__ -= this__;			\
		buf__ += this__;			\
	    }						\
	}						\
	return len - todo__;

static int
x_write (int fd, void *buf, size_t len)
{
    X_IO (write, fd, buf, len);
}

static int
x_read (int fd, void *buf, size_t len)
{
    X_IO (read, fd, buf, len);
}

static int
ui_command (char *str)
{
    if (ui_pid != 0)
    {
	u_char ret;
	x_write (ui_stdin[1], str, strlen(str));
	x_read (ui_stdout[0], &ret, 1);
	return ret;
    }
    else
	return 0;
}

static void
ui_output_callback (gpointer data, gint fd, GdkInputCondition cond)
{
    char out;
    if (x_read (ui_stdout[0], &out, 1) == 1)
    {
	GtkWidget *label;

	switch (out)
	{
	case 'c':
	    capplet_widget_state_changed (CAPPLET_WIDGET (capplet), TRUE);
	    break;

	case 'g':			/* group doesn't exist */
	    waitpid (ui_pid, 0, 0);
	    ui_pid = 0;
	    gtk_container_remove (GTK_CONTAINER (capplet), ui_socket);
	    gtk_object_destroy (GTK_OBJECT (ui_socket));
	    ui_socket = 0;
	    label = gtk_label_new ("[That group doesn't exist]");
	    gtk_container_add (GTK_CONTAINER (capplet), label);
	    gtk_widget_show (label);
	    gtk_input_remove (ui_handler_id);
	    ui_handler_id = 0;
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
    gtk_container_remove (GTK_CONTAINER (capplet), ui_socket);
    ui_command ("ok\n");
}

static void
sawmill_cancel (void)
{
    gtk_container_remove (GTK_CONTAINER (capplet), ui_socket);
    ui_command ("cancel\n");
}

static void
sawmill_destroy (void)
{
    ui_command ("cancel\n");
}


/* initialisation */

/* returns non-zero if sawfish is running */
static gint
sawmill_running_p (void)
{
    int ret = client_open (0);	/* 0 == $DISPLAY */
    if (ret == 0)
	client_close ();
    return (ret == 0);
}

static void
sawmill_setup (void)
{
    capplet = capplet_widget_new ();

    if (!sawmill_running_p ())
    {
	GtkWidget *label = gtk_label_new ("[Sawfish isn't running]");
	gtk_container_add (GTK_CONTAINER (capplet), label);
	return;
    }

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
    gtk_signal_connect (GTK_OBJECT (capplet), "destroy",
			GTK_SIGNAL_FUNC (sawmill_destroy), NULL);

    ui_socket = gtk_socket_new ();
    gtk_container_add (GTK_CONTAINER (capplet), ui_socket);

    /* show this here so the widget gets realized */
    gtk_widget_show_all (capplet);

    /* now fork the sawmill-ui script */
    pipe (ui_stdin);
    pipe (ui_stdout);
    switch (ui_pid = fork ())
    {
	char buf[64];
	char *argv[10];
	int i;

    case -1:
	exit (5);

    case 0:				/* child */
	dup2 (ui_stdin[0], 0);
	dup2 (ui_stdout[1], 1);
	close (ui_stdin[0]);
	close (ui_stdin[1]);
	close (ui_stdout[0]);
	close (ui_stdout[1]);
	sprintf (buf, "%ld", (long)GDK_WINDOW_XWINDOW (ui_socket->window));
	i = 0;
	argv[i++] = "sawfish-ui";
	if (!no_flatten)
	    argv[i++] = "--flatten";
	else
	    argv[i++] = "--single-level";
	argv[i++] = "--socket-id";
	argv[i++] = buf;
	if (group != 0)
	{
	    argv[i++] = "--group";
	    argv[i++] = group;
	}
	argv[i++] = 0;
	execvp (argv[0], argv);
	exit (10);

    default:				/* parent */
	close (ui_stdin[0]);
	close (ui_stdout[1]);
	ui_handler_id = gdk_input_add (ui_stdout[0], GDK_INPUT_READ,
				       (GdkInputFunction)
				       ui_output_callback, 0);
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
	  0, "Sawfish customization group", "GROUP" },
	{ "sawfish-group", 0, POPT_ARG_STRING, &group,
	  0, "Sawfish customization group", "GROUP" },
	{ "sawfish-no-flatten", 0, POPT_ARG_NONE, &no_flatten,
	  0, "Don't flatten group trees", 0 },
	{ 0, 0, 0, 0, 0 }
    };

#if 0
    bindtextdomain (PACKAGE, GNOMELOCALEDIR);
    textdomain (PACKAGE);
#endif

    init_results = gnome_capplet_init("sawfish-properties", SAWFISH_VERSION,
				      argc, argv, options, 0, NULL);

    if (init_results < 0) {
	g_warning ("an initialization error occurred while "
		   "starting 'sawfish-properties-capplet'.\n"
		   "aborting...\n");
	exit (1);
    }

    client = gnome_master_client ();
    flags = gnome_client_get_flags(client);

#if 0
    /* XXX I copied this from mouse-properties.c, but the
       XXX GNOME_SAWFISH_PROPERTIES root property just grows
       XXX each time the capplet is started... */

    if (flags & GNOME_CLIENT_IS_CONNECTED) {
	token = gnome_startup_acquire_token("GNOME_SAWFISH_PROPERTIES",
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
#endif

    if (init_results != 1) {
	sawmill_setup ();
	gtk_widget_show_all (capplet);
	capplet_gtk_main ();
	if (ui_pid != 0)
	    waitpid (ui_pid, 0, 0);
    }
    return 0;
}
