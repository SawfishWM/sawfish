/* stacking-list.c -- maintain the stacking-ordered window list

   $Id$

   Copyright (C) 2001 Eazel, Inc.

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
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Authors: John Harper <jsh@eazel.com>  */

#include "sawmill.h"
#include <assert.h>

static Lisp_Window *lowest_window, *highest_window;

/* Remove window W from the stacking list */
void
remove_from_stacking_list (Lisp_Window *w)
{
    /* divert the links around W */

    if (w->above != 0)
	w->above->below = w->below;
    if (w->below != 0)
	w->below->above = w->above;

    /* Fix the end points */

    if (lowest_window == w)
	lowest_window = w->above;
    if (highest_window == w)
	highest_window = w->below;

    w->above = w->below = 0;
}

/* Insert W at the top of the stacking list */
void
insert_in_stacking_list_above_all (Lisp_Window *w)
{
    assert (w->below == 0 && w->above == 0);

    w->above = 0;
    w->below = highest_window;
    if (highest_window != 0)
	highest_window->above = w;
    highest_window = w;
    if (lowest_window == 0)
	lowest_window = w;
}

/* Insert W at the bottom of the stacking list */
void
insert_in_stacking_list_below_all (Lisp_Window *w)
{
    assert (w->below == 0 && w->above == 0);

    w->below = 0;
    w->above = lowest_window;
    if (lowest_window != 0)
	lowest_window->below = w;
    lowest_window = w;
    if (highest_window == 0)
	highest_window = w;
}

/* Insert W immediately above X */
void
insert_in_stacking_list_above (Lisp_Window *w, Lisp_Window *x)
{
    assert (highest_window != 0 && lowest_window != 0);
    assert (w->below == 0 && w->above == 0);

    if (x->above != 0)
	x->above->below = w;

    w->above = x->above;
    x->above = w;
    w->below = x;

    if (highest_window == x)
	highest_window = w;
}

/* Insert W immediately below X */
void
insert_in_stacking_list_below (Lisp_Window *w, Lisp_Window *x)
{
    assert (highest_window != 0 && lowest_window != 0);

    if (x->below != 0)
	x->below->above = w;

    w->below = x->below;
    x->below = w;
    w->above = x;

    if (lowest_window == x)
	lowest_window = w;
}

static inline Window
stackable_window_id (Lisp_Window *w)
{
    return w->reparented ? w->frame : w->id;
}

/* Make the physical stacking position of W match its current position
   in the stacking list */
void
restack_window (Lisp_Window *w)
{
    XWindowChanges wc;
    u_int mask = 0;

    if (w->above != 0)
    {
	wc.stack_mode = Below;
	wc.sibling = stackable_window_id (w->above);
	mask = CWStackMode | CWSibling;
    }
    else if (w->below != 0)
    {
	wc.stack_mode = Above;
	wc.sibling = stackable_window_id (w->below);
	mask = CWStackMode | CWSibling;
    }

    if (mask != 0)
	XConfigureWindow (dpy, stackable_window_id (w), mask, &wc);
}

/* Return a list of windows in top->bottom order */
repv
make_stacking_list (void)
{
    repv out = Qnil;
    Lisp_Window *ptr;
    for (ptr = lowest_window; ptr != 0; ptr = ptr->above)
	out = Fcons (rep_VAL (ptr), out);
    return out;
}
