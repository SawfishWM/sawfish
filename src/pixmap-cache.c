/* pixmap-cache.c -- Caching pixmaps
   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

/* Commentary:

   This file provides the pixmap cache, a mapping from (image, width,
   height) tuples to rendered pixmaps (both image and mask). This is
   only required when not using Imlib (Imlib has a built-in cache).

   It's implemented as two doubly-linked lists, each containing the
   same component nodes, but one list chains all pixmaps associated
   with a particular image object, the other chains all pixmaps in
   order of allocation/use (so that the oldest unused node can be
   discarded quickly when the cache overflows its upper bound).

   XXX Need to measure average length of image lists; if too long
   XXX they should be changed to doubly-linked hash tables. */

#include "sawmill.h"

static u_long cached_pixels, max_cached_pixels = 256 * 1024;
static u_long hits, misses;

#ifdef NEED_PIXMAP_CACHE

struct pixmap_cache_node_struct {
    pixmap_cache_node *next, *pred;
    pixmap_cache_node *newer, *older;
    Lisp_Image *im;
    int width, height;
    Pixmap p1, p2;
    int ref_count;
};

static pixmap_cache_node *oldest, *newest;


/* list manipulators */

static void
remove_from_age_list (pixmap_cache_node *n)
{
    if (n->newer != 0)
	n->newer->older = n->older;
    else
	newest = n->older;
    if (n->older != 0)
	n->older->newer = n->newer;
    else
	oldest = n->newer;
}

static void
prepend_to_age_list (pixmap_cache_node *n)
{
    n->newer = oldest;
    if (n->newer != 0)
	n->newer->older = n;
    oldest = n;
    n->older = 0;
    if (newest == 0)
	newest = n;
}

static void
remove_from_image (pixmap_cache_node *n)
{
    if (n->next != 0)
	n->next->pred = n->pred;
    else
	n->im->pixmap_last = n->pred;
    if (n->pred != 0)
	n->pred->next = n->next;
    else
	n->im->pixmap_first = n->next;
}

static void
prepend_to_image (pixmap_cache_node *n)
{
    Lisp_Image *im = n->im;
    n->next = im->pixmap_first;
    if (n->next != 0)
	n->next->pred = n;
    im->pixmap_first = n;
    n->pred = 0;
    if (im->pixmap_last == 0)
	im->pixmap_last = n;
}

static void
free_node (pixmap_cache_node *n, bool dealloc)
{
    if (n->p1 != 0)
	XFreePixmap (dpy, n->p1);
    if (n->p2 != 0)
	XFreePixmap (dpy, n->p2);
    if (dealloc)
	rep_free (n);
}


/* public interface */

bool
pixmap_cache_ref (Lisp_Image *im, int width, int height,
		  Pixmap *p1, Pixmap *p2)
{
    pixmap_cache_node *n;
    for (n = im->pixmap_first; n != 0; n = n->next)
    {
	if (n->width == width && n->height == height)
	{
	    remove_from_image (n);
	    prepend_to_image (n);
	    remove_from_age_list (n);
	    prepend_to_age_list (n);
	    n->ref_count++;
	    *p1 = n->p1;
	    *p2 = n->p2;
	    hits++;
	    return TRUE;
	}
    }
    misses++;
    return FALSE;
}

void
pixmap_cache_unref (Lisp_Image *im, Pixmap p1, Pixmap p2)
{
    pixmap_cache_node *n;
    for (n = im->pixmap_first; n != 0; n = n->next)
    {
	if (n->p1 == p1 && n->p2 == p2)
	{
	    n->ref_count--;
	    return;
	}
    }
    fprintf (stderr, "warning: unref'ing unknown image in pixmap-cache\n");
}

void
pixmap_cache_set (Lisp_Image *im, int width, int height,
		  Pixmap p1, Pixmap p2)
{
    int pixel_count = width * height;
    pixmap_cache_node *n = 0;

    while (pixel_count + cached_pixels > max_cached_pixels)
    {
	/* remove oldest node */
	pixmap_cache_node *this = oldest;
	while (this != 0 && this->ref_count > 0)
	    this = this->newer;
	if (this == 0)
	    break;
	remove_from_image (this);
	remove_from_age_list (this);
	cached_pixels -= this->width * this->height;
	free_node (this, n != 0);
	if (n == 0)
	    n = this;
    }

    if (n == 0)
	n = rep_alloc (sizeof (pixmap_cache_node));

    n->im = im;
    n->width = width;
    n->height = height;
    n->p1 = p1;
    n->p2 = p2;
    n->ref_count = 1;

    prepend_to_image (n);
    prepend_to_age_list (n);
    cached_pixels += pixel_count;
}

void
pixmap_cache_flush_image (Lisp_Image *im)
{
    pixmap_cache_node *n, *next;
    for (n = im->pixmap_first; n != 0; n = next)
    {
	next = n->next;
	remove_from_age_list (n);
	free_node (n, TRUE);
    }
    im->pixmap_first = im->pixmap_last = 0;
}

#endif /* NEED_PIXMAP_CACHE */

DEFUN ("pixmap-cache-control", Fpixmap_cache_control,
       Spixmap_cache_control, (repv max), rep_Subr1)
{
    if (rep_INTP (max) && rep_INT (max) > 0)
	max_cached_pixels = rep_INT (max);

    return rep_list_4 (rep_MAKE_INT (max_cached_pixels),
		       rep_MAKE_INT (cached_pixels),
		       rep_MAKE_INT (hits), rep_MAKE_INT (misses));
}

void
pixmap_cache_init (void)
{
    rep_ADD_SUBR (Spixmap_cache_control);
}
