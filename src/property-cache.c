/* property-cache.c -- maintain the X property cache

   $Id$

   Copyright (C) 2002 John Harper.

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

   Authors: John Harper <jsh@unfactored.org>  */

#include "sawmill.h"
#include <assert.h>

#define CACHE_ASSOC 4
#define CACHE_SETS 127
#define CACHE_SIZE (CACHE_ASSOC * CACHE_SETS)
#define CACHE_HASH(id,prop) ((((id) << 4) ^ ((prop) >> 3)) % CACHE_SETS)

static repv *cache_ids;
static repv *cache_props;
static repv *cache_values;
static repv cache_vec;
static u_int cache_ages[CACHE_SIZE];
static u_int cache_updates[CACHE_SIZE];
static u_int cache_clock;

static u_int cache_hits, cache_misses;

repv
property_cache_ref (repv id, repv prop)
{
    u_int h, i;

    if (cache_vec == rep_NULL)
	return Qnil;

    h = CACHE_HASH (id, prop) * CACHE_ASSOC;

    DB (("prop ref: 0x%x,%s (%d) -> ", id, rep_STR (rep_SYM (prop)->name), h));

    for (i = h; i < h + CACHE_ASSOC; i++)
    {
	if (cache_ids[i] == id && cache_props[i] == prop)
	{
	    cache_hits++;
	    DB (("hit\n"));
	    cache_ages[i] = ++cache_clock;
	    return cache_values[i];
	}
    }

    DB (("miss\n"));
    cache_misses++;
    return rep_NULL;
}

void
property_cache_set (repv id, repv prop, repv value, int invals)
{
    u_int h, i, oldest, oldest_age;

    if (cache_vec == rep_NULL)
    {
	cache_vec = Fmake_vector (rep_MAKE_INT (CACHE_SIZE * 3), Qnil);
	rep_mark_static (&cache_vec);

	cache_ids = rep_VECT (cache_vec)->array;
	cache_props = cache_ids + CACHE_SIZE;
	cache_values = cache_props + CACHE_SIZE;
    }

    h = CACHE_HASH (id, prop) * CACHE_ASSOC;

    oldest_age = UINT_MAX;
    oldest = -1;

    for (i = h; i < h + CACHE_ASSOC; i++)
    {
	if (cache_ids[i] == id && cache_props[i] == prop)
	{
	    cache_values[i] = value;
	    cache_updates[i] += invals;
	    return;
	}

	if (cache_ages[i] <= oldest_age)
	{
	    oldest_age = cache_ages[i];
	    oldest = i;
	}
    }

    assert (oldest != -1);

    if (cache_ids[oldest] != 0)
	DB (("prop eject: 0x%x (%d)\n", cache_ids[oldest], oldest));

    cache_ids[oldest] = id;
    cache_props[oldest] = prop;
    cache_values[oldest] = value;
    cache_ages[oldest] = ++cache_clock;
    cache_updates[oldest] = invals;

    DB (("set: 0x%x,%s (%d)\n", id, rep_STR (rep_SYM (prop)->name), oldest));
}

void
property_cache_invalidate_window (repv id)
{
    int i;

    if (cache_vec == rep_NULL)
	return;

    for (i = 0; i < CACHE_SIZE; i++)
    {
	if (cache_ids[i] == id)
	{
	    cache_ids[i] = 0;
	    cache_props[i] = Qnil;
	    cache_values[i] = Qnil;
	}
    }
}

void
property_cache_invalidate (repv id, repv prop)
{
    u_int h, i;

    if (cache_vec == rep_NULL)
	return;

    h = CACHE_HASH (id, prop) * CACHE_ASSOC;

    for (i = h; i < h + CACHE_ASSOC; i++)
    {
	if (cache_ids[i] == id && cache_props[i] == prop)
	{
	    if (cache_updates[i] == 0)
	    {
		cache_ids[i] = 0;
		cache_props[i] = Qnil;
		cache_values[i] = Qnil;
	    }
	    else
		cache_updates[i]--;
	}
    }
}
