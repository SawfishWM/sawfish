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

#include "sawfish.h"
#include <assert.h>

static Lisp_Window *lowest_window, *highest_window;
static int stacking_list_length ();

/* Assertions and consistency checking */

#if !defined (NDEBUG)
static void
assert_constraints (Lisp_Window *w)
{
    nonterminal_assert (w->above == 0 || WINDOWP (rep_VAL (w->above)));
    nonterminal_assert (w->below == 0 || WINDOWP (rep_VAL (w->below)));
    nonterminal_assert (lowest_window == 0 || WINDOWP (rep_VAL (lowest_window)));
    nonterminal_assert (highest_window == 0 || WINDOWP (rep_VAL (highest_window)));
}
#else
# define assert_constraints(w) do { ; } while (0)
#endif

/* Stacking list manipulation */

/* Return true if window W is currently in the stacking list. */
bool
window_in_stacking_list_p (Lisp_Window *w)
{
    Lisp_Window *ptr;
    for (ptr = lowest_window; ptr != 0; ptr = ptr->above)
    {
	if (ptr == w)
	    return TRUE;
    }
    return FALSE;
}

/* Remove window W from the stacking list */
void
remove_from_stacking_list (Lisp_Window *w)
{
    return_if_fail (!WINDOW_IS_GONE_FOR_STACKING_P (w));
    return_if_fail (window_in_stacking_list_p (w));

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

    assert_constraints (w);
}

/* Insert W at the top of the stacking list */
void
insert_in_stacking_list_above_all (Lisp_Window *w)
{
   return_if_fail (!WINDOW_IS_GONE_FOR_STACKING_P (w));
    return_if_fail (!window_in_stacking_list_p (w));

    w->above = 0;
    w->below = highest_window;
    if (highest_window != 0)
	highest_window->above = w;
    highest_window = w;
    if (lowest_window == 0)
	lowest_window = w;

    assert_constraints (w);
}

/* Insert W at the bottom of the stacking list */
void
insert_in_stacking_list_below_all (Lisp_Window *w)
{
    return_if_fail (!WINDOW_IS_GONE_FOR_STACKING_P (w));
    return_if_fail (!window_in_stacking_list_p (w));

    w->below = 0;
    w->above = lowest_window;
    if (lowest_window != 0)
	lowest_window->below = w;
    lowest_window = w;
    if (highest_window == 0)
	highest_window = w;

    assert_constraints (w);
}

/* Insert W immediately above X */
void
insert_in_stacking_list_above (Lisp_Window *w, Lisp_Window *x)
{
    return_if_fail (!WINDOW_IS_GONE_FOR_STACKING_P (w) && !WINDOW_IS_GONE_FOR_STACKING_P (x));
    return_if_fail (!window_in_stacking_list_p (w));
    return_if_fail (window_in_stacking_list_p (x));

    if (x->above != 0)
	x->above->below = w;

    w->above = x->above;
    x->above = w;
    w->below = x;

    if (highest_window == x)
	highest_window = w;

    assert_constraints (w);
}

/* Insert W immediately below X */
void
insert_in_stacking_list_below (Lisp_Window *w, Lisp_Window *x)
{
    return_if_fail (!WINDOW_IS_GONE_FOR_STACKING_P (w) && !WINDOW_IS_GONE_FOR_STACKING_P (x));
    return_if_fail (!window_in_stacking_list_p (w));
    return_if_fail (window_in_stacking_list_p (x));

    if (x->below != 0)
	x->below->above = w;

    w->below = x->below;
    x->below = w;
    w->above = x;

    if (lowest_window == x)
	lowest_window = w;

    assert_constraints (w);
}

/* Making the X stacking order reflect the stacking list */

static inline Window
stackable_window_id (Lisp_Window *w)
{
    return w->reparented ? w->frame : w->id;
}

/* to keep the Lisp code intact, yet change the implementation, we allow the Lisp to
 * start a `transaction', by setting this variable, then operations act on DATA only,
 * and in the end of the transaction, the DATA is sent to the X server. */
int restack_fast=0;
/* Make the physical stacking position of W match its current position
   in the stacking list. Tries to stack relative to the window above W
   before trying the window below W (if there's no window above W). */
void
restack_window (Lisp_Window *w)
{
    if (restack_fast)
        return;
    if (!WINDOW_IS_GONE_P (w))
    {
	XWindowChanges wc;
	unsigned int mask = 0;

	return_if_fail (window_in_stacking_list_p (w));

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
}

/* Miscellaneous public functions */

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

Lisp_Window*
bottom_window()
{
   return  lowest_window;
}

/* new restacking */

/* mmc's stacking:  using algorithm described here:
 * http://citeseer.ist.psu.edu/myers86ond.html
 */

/* fixme: if some window is gone? */
void
stack_window_below (Lisp_Window* w, Lisp_Window* the_one_above)
{
   XWindowChanges wc;
   wc.stack_mode = Below;
   wc.sibling = stackable_window_id (the_one_above);
   u_int mask = CWStackMode | CWSibling;
   XConfigureWindow (dpy, stackable_window_id (w), mask, &wc);
}


static int
stacking_list_length ()
{
   Lisp_Window *ptr;
   int i = 0;
   for (ptr = lowest_window; ptr != 0; ptr = ptr->above)
      i++;
   return i;
}

static int
save_stacking_list_to_c_array (Lisp_Window** array)
{
   Lisp_Window *ptr;
   int i = 0;

   for (ptr = lowest_window; ptr != 0; ptr = ptr->above)
      array[i++] = ptr;
#if 0
   /* this is the `TOP' */
   array[i] = 0;
#endif
   return i;    /* i windows are present. */
}



/*  `Level_matrix':

 *    For the lcs problem, i consider 2 matrices:
 *    * the boolean difference NxM matrix (N, M lenght of the sequences),
 *    I don't actually store it.

 *    Just ask for elements.
 *        with the `EQUAL' macro
 *    *  the `level' matrix (d * max)
 *
 *    values in a row d in the level matrix are (a level of)
 *    x-coordinats, of ending points of edit paths of lenght d, ending at
 *    the diagonal k (of the NxM matrix).

 *   `max' is the upper bound of the number of necessary (possibly visited) diagonals.
 *     here we see the level_matrix rotated 45' counter-clock-wise:
 *
 *      0                    `max' is the upper bound of steps on each level.
 *  0   ../.../.........|..........|
 *      ./.  /
 *level-/.../....X
 *      (level * 2 + 3)  ??
 *        /____ step
 *       /
 *      /
 *
 *  */


/* accessor to the `level' matrix:   because we keep it in a 1D array, we need `max', the lenght of a row. */
static inline int
get (int* V, int max, int level, int step)
{
   return V[((level +1) * 2 + 1)* max + step];
}

static inline
int
set (int* V, int max, int level, int step, int value)
{
   return V[(level * 2 + 3)* max + step] = value;
}




/* how to compare (equal or not) 2 elements of the sequences?  */
#if 0
typedef char* element;
#define EQUAL(x,y) (x==y)

#else

typedef Lisp_Window* element;
#define EQUAL(x,y) (x==y)
/*  (x->id == y->id) */
#endif




/* M,N  lenghts,  a1, a2 the sequences being compared,
 * max ... maximum difference  (diagonals possibly visited)
 *
 * V  the level matrix (k,d) the x-coordinate of the longest k d-path
 *    contains the `trace'
 * return_k    the actual lenght ?
 */
static int
lcs_myers_with_trace (int M, int N, element *a1, element a2[], int max,
                      int* V,
                      int* return_k)
{
   int d;
   int k;
   int x;
   int y;

   /*   -1/1   start value:
    *    |
    *   0/0  0/1 ....
    *   1/0
    *   .    .
    *   .      .
    *  */

   /* the maximal X-coordinate reachable with -1 `skips'/differences on the diagonal 1 is 0 */
   set(V, max, -1, 1, 0); /* fixme:  s/0/-1/  then   */


   for (d = 0; d <= max; d++)   /* the level */
      {

         /* diagonal k:    k = x -y  */
         /* V (d,k)  the x coord. of the `longest' path ending on diagonal k,
          * after d steps/differences.
          * We know, that it has form:
          * V(d-1,k +/- 1), followed by the longest `snake' */

         /* here we construct the new level:  */


         /*  . B
          *  A C    value C is computed out of A or B.
          *  At the ends only one of them is available.
          */

         for (k= -d; k <= d ;k += 2)
            {
               if ((k == -d)    /* beginning of the level: we can only
                                 * look upwards (not leftwards)*/
                   /* upwards is better than left, but only if left is available: */
                   || ( (k< d) && (get(V, max, d -1, k-1) < get(V, max, d-1, k+1))))
                  x = get(V, max, d-1, k+1);      /* moving y coordinate  i.e upwards */
               else
                  x = get(V, max, d-1, k-1) + 1;  /* moving x   i.e. leftwards */

               y = x - k;
               /* snake */

               while ((x < M) && (y < N) && (EQUAL(a1[x],a2[y])))
                  ++x, ++y;

               set(V, max, d ,k, x);

               if ((x >= M) && (y >= N))
                  {
                     *return_k = k;
                     return d;
                  }
            };
      };
   return -1;                   /* this would be error. */
}




/*   walk along the trace, and issue X calls:
 *  we start with:
 *
 *      myers_trace(table, max, d, k, get(table, max, d, k), stacking,
 *                  current_stacking, (element) NULL);
 *
 *  Walk along the trace and issue the X calls.  This function is called
 *  recursively !!!
 *
 *  V  is the level-matrix
 *  max is needed to access the matrix V
 *  d  is the difference
 *  k  is current diagonal
 *  x  x coordinate
 *  s1, s2  the sequences
 *  s2
 *  last_fix .... window under which to move eventually a window at the
 *      current position: missing from s1, and
 */


/* name of the window (above) */
#define  info_about(array, index)  ((array[index])?rep_STR( ((Lisp_Window*) array[index])->name):(u_char*)"top")

static void
myers_trace (int V[], int max, int d, int k, int x, element s1[], element s2[], int s2_len, element last_fix)
{

   /* X is the x-coordinate: we know X from the previous iteration !!!
    *
    * last_fix  is the element which is in the common subsequence.
    *
    *   |
    *   |
    *  _
    *   \_
    * */



   if (d == 0) /* the whole prefix is equal in both sequences. nothing more to do */
      return;


/*
   Where to go: up or down?

   what if we go outside of used values ??  impossible

   if (d == k)
   (d == - k)
   -> only 1 direction!
*/

   int up   = (k==d)? -1: get(V, max, d-1 , k+1);
   int left = (-k==d)?-1: get(V, max, d-1, k-1);




   int y = left - (k-1);    /* fixme: we grow by 1 y  ??? */


   /* \ o
    *  x|
    *-o-x          x possible snake
    *    x|
    *
    *  */

   /* fixme: if one of this is out of the rectangle ???  */

   if ((up > left) || (y<0)){           /* if == take ??  But if lef == our position ?? */
      /* x is the same, and we have k+1 ?? */

      /*       x
       *      |
       *y      \
       *  */

      y = up - (k+1);

      if (x - up > 0)      /* we found some alligned elements: use the last one as the 'above' */
         {

            if (up < s2_len) /* in 1st round this is false: so i ignore it for now: */
               {
                  last_fix = s1[up];
                  /* fixme: this is a non-issue: revert !!! */
               }

         };

      /* fixme:
       *          |
       *  --------+----------  limit  y = 0
       *          |
       *
       *
       * */

      element this = s2[y]; /* why -1 ??   b/c  we start w/ 0, but the  */


      if (!last_fix)         /* only at the top! */
         {
            XRaiseWindow(dpy, stackable_window_id((Lisp_Window*) this));
         }
      else
         {
            /* fixme: if some window is gone? */
            stack_window_below((Lisp_Window*) this, (Lisp_Window*) last_fix);
         }

      /* last_fix = this; */

      myers_trace(V, max, d-1, k+1, up, s1, s2, s2_len, this);
   } else {

      if (x - (left + 1) > 0)      /* we found some alligned elements: use the last one as the 'above' */
         {
            last_fix = s1[left + 1]; /* or + 2 ??? fixme! */
         };

      myers_trace(V, max, d-1, k-1, left, s1, s2, s2_len, last_fix);
      // printf("up: %d, left: %d\n", up, left);
      /* these are x. We have  v == v  on which snake? */
   }
}


/*    sequence1  =  current server stackin order
 *              x->
 *   ---------------------------|
 *s2 | \
 * d |  \_____
 * e |        \
 * s |y        | add   X call!
 * i ||         \
 * r |v          \____remove___  .. remove no X call!
 * e |                        |
 * d |                        |...
 *
 * */

/* fixme:  GC during this?
 *
 *      I keep the windows ... just X WIDs ?
 *      X calls -> error handler -> ...
 */


/*
 *  `current_server_stacking'  must be/is the stacking order on the X server.
 *  (`len' is the number of elements)
 *  Implicitly we use the data kept in this file ... lowest_window &
 *
 *  This function finds minimal restacking sequence to submit to the X server,
 *  in order to change the `current_server_stacking'
 *  into the stacking order kept by "our data": `lowest_window' & up.
 *  The request is also sent to the X server.
 */
void
push_stacking_list_to_server (Lisp_Window** current_server_stacking, int len)
{
    int diff_max, k = 0, d;

    /* make a C _array_ from "our data" on stacking order ... we need an
     * indexed/random access I prefer to ask for the lenght first, so I
     * can allocate the C array on the stack.
     */
    Lisp_Window** current_stacking = alloca (sizeof(Lisp_Window*) * stacking_list_length());
    int current_stacking_len = save_stacking_list_to_c_array(current_stacking);

    /* upper bound for the difference.   a maximum is enough? */
    diff_max = current_stacking_len + len;


    int temp_matrix_size = (2 * diff_max * (diff_max + 1)) * sizeof(element);
    /* fixme:  if too large to fit on stack?  allocate elsewhere
     * how to see if it fits?  register? & known limit.*/

    /* I could put this calculation to a function, but it's nice to
     * allocate on the stack! */
    int* level_matrix = (int*) alloca(temp_matrix_size); /* bzero not needed! */


    /* Find the difference */
    d = lcs_myers_with_trace(len, current_stacking_len,
                             current_server_stacking, current_stacking,
                             diff_max, level_matrix,
                             &k);

    /* `k' should be 0, because both the stackings have the same # of
     * elements. But we don't care. */


    /* walk the difference, and emit the XRestackWindows */
    /* both lenghts should be equal, right?? */
    assert (get(level_matrix, diff_max, d, k) == len); 

    /* fixme: isn't d-k = m-n ? i.e. on the diagoal ?  err no. it should be k = m-n ? */
    myers_trace(level_matrix, diff_max,  d, k,
                get(level_matrix, diff_max, d, k), current_server_stacking,
                current_stacking, current_stacking_len, (element) NULL);
}
