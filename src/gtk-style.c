/* gtk-style.c -- print the default GTK rc style (if possible) as s-exprs
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

#include <stdio.h>
#include "gtk/gtk.h"

char *states[] = {
    "normal", "active", "prelight", "selected", "insensitive"
};

void
print_color (GdkColor *c)
{
    printf ("\"#%02x%02x%02x\"", c->red / 256, c->green / 256, c->blue / 256);
}

void
print_colors (char *name, GdkColor *x, int n)
{
    int i;
    printf ("(%s", name);
    for (i = 0; i < n; i++)
    {
	printf (" (%s . ", states[i]);
	print_color (x + i);
	printf (")");
    }
    printf (")\n");
}

int
print_rc_colors (char *name, GdkColor *x, int n,
		 GtkRcFlags *flags, GtkRcFlags mask)
{
    int i;
    for (i = 0; i < n; i++)
    {
	if (flags[i] & mask)
	    break;
    }
    if (i == n)
	return 1;
    printf ("(%s", name);
    for (i = 0; i < n; i++)
    {
	if (flags[i] & mask)
	{
	    printf (" (%s . ", states[i]);
	    print_color (x + i);
	    printf (")");
	}
    }
    printf (")\n");
    return 0;
}

void
print_strings (char *name, char **x, int n)
{
    int i;
    for (i = 0; i < n; i++)
    {
	if (x[i] != 0)
	    break;
    }
    if (i == n)
	return;
    printf ("(%s", name);
    for (i = 0; i < n; i++)
    {
	if (x[i] != 0)
	    printf (" (%s . \"%s\")", states[i], x[i]);
    }
    printf (")\n");
}

void
print_rc_style (GtkRcStyle *rc, GtkStyle *style)
{
    if (print_rc_colors ("fg", rc->fg, 5, rc->color_flags, GTK_RC_FG))
    	print_colors ("fg", style->fg, 5);
    if (print_rc_colors ("bg", rc->bg, 5, rc->color_flags, GTK_RC_BG))
    	print_colors ("bg", style->bg, 5);
    if (print_rc_colors ("text", rc->text, 5, rc->color_flags, GTK_RC_TEXT))
    	print_colors ("text", style->text, 5);
    if (print_rc_colors ("base", rc->base, 5, rc->color_flags, GTK_RC_BASE))
    	print_colors ("base", style->base, 5);
    print_colors ("light", style->light, 5);
    print_colors ("dark", style->dark, 5);
    print_colors ("mid", style->mid, 5);
    print_strings ("bg-pixmap", rc->bg_pixmap_name, 5);
}

void
print_style (GtkStyle *style)
{
    print_colors ("fg", style->fg, 5);
    print_colors ("bg", style->bg, 5);
    print_colors ("light", style->light, 5);
    print_colors ("dark", style->dark, 5);
    print_colors ("mid", style->mid, 5);
    print_colors ("text", style->text, 5);
    print_colors ("base", style->base, 5);
}

int
main (int argc, char **argv)
{
    GtkWidget *win;
    GtkStyle *style = 0;

    gtk_set_locale();
    gtk_init (&argc, &argv);
    win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    style = gtk_rc_get_style (win);
    if (style == 0)
	style = gtk_widget_get_style (win);
    if (style != 0)
    {
	if (style->rc_style != 0)
	    print_rc_style (style->rc_style, style);
	else
	    print_style (style);
    }
    return 0;
}
