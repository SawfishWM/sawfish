/* gradient.c -- draw Afterstep-like gradients in images
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

#include "sawmill.h"

DEFSYM(gradient, "gradient");

DEFUN("draw-vertical-gradient", Fdraw_vertical_gradient,
      Sdraw_vertical_gradient, (repv img, repv from_, repv to_), rep_Subr3)
{
    u_char from[3], to[3];
    int width, height, stride, channels;
    int x, y;
    u_char *data;

    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE2(from_, COLORP);
    rep_DECLARE3(to_, COLORP);

    data = image_pixels (VIMAGE(img));
    width = image_width (VIMAGE(img));
    height = image_height (VIMAGE(img));
    stride = image_row_stride (VIMAGE(img));
    channels = image_channels (VIMAGE(img));

    from[0] = VCOLOR(from_)->red / 256;
    from[1] = VCOLOR(from_)->green / 256;
    from[2] = VCOLOR(from_)->blue / 256;
    to[0] = VCOLOR(to_)->red / 256;
    to[1] = VCOLOR(to_)->green / 256;
    to[2] = VCOLOR(to_)->blue / 256;

    for (y = 0; y < height; y++)
    {
	for (x = 0; x < width; x++)
	{
	    data[y*stride+x*channels+0] = (from[0] + (to[0] - from[0])
					   * y / height);
	    data[y*stride+x*channels+1] = (from[1] + (to[1] - from[1])
					   * y / height);
	    data[y*stride+x*channels+2] = (from[2] + (to[2] - from[2])
					   * y / height);
	}
    }

    image_changed (VIMAGE (img));
    return img;
}

DEFUN("draw-horizontal-gradient", Fdraw_horizontal_gradient,
      Sdraw_horizontal_gradient, (repv img, repv from_, repv to_), rep_Subr3)
{
    u_char from[3], to[3];
    int width, height, stride, channels;
    int x, y;
    u_char *data;

    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE2(from_, COLORP);
    rep_DECLARE3(to_, COLORP);

    data = image_pixels (VIMAGE(img));
    width = image_width (VIMAGE(img));
    height = image_height (VIMAGE(img));
    stride = image_row_stride (VIMAGE(img));
    channels = image_channels (VIMAGE(img));

    from[0] = VCOLOR(from_)->red / 256;
    from[1] = VCOLOR(from_)->green / 256;
    from[2] = VCOLOR(from_)->blue / 256;
    to[0] = VCOLOR(to_)->red / 256;
    to[1] = VCOLOR(to_)->green / 256;
    to[2] = VCOLOR(to_)->blue / 256;

    for (y = 0; y < height; y++)
    {
	for (x = 0; x < width; x++)
	{
	    data[y*stride+x*channels+0] = (from[0] + (to[0] - from[0])
					   * x / width);
	    data[y*stride+x*channels+1] = (from[1] + (to[1] - from[1])
					   * x / width);
	    data[y*stride+x*channels+2] = (from[2] + (to[2] - from[2])
					   * x / width);
	}
    }

    image_changed (VIMAGE (img));
    return img;
}

DEFUN("draw-diagonal-gradient", Fdraw_diagonal_gradient,
      Sdraw_diagonal_gradient, (repv img, repv from_, repv to_), rep_Subr3)
{
    double from[3], to[3];
    int width, height, stride, channels;
    int x, y;
    u_char *data;

    rep_DECLARE1(img, IMAGEP);
    rep_DECLARE2(from_, COLORP);
    rep_DECLARE3(to_, COLORP);

    data = image_pixels (VIMAGE(img));
    width = image_width (VIMAGE(img));
    height = image_height (VIMAGE(img));
    stride = image_row_stride (VIMAGE(img));
    channels = image_channels (VIMAGE(img));

    from[0] = VCOLOR(from_)->red / 256;
    from[1] = VCOLOR(from_)->green / 256;
    from[2] = VCOLOR(from_)->blue / 256;
    to[0] = VCOLOR(to_)->red / 256;
    to[1] = VCOLOR(to_)->green / 256;
    to[2] = VCOLOR(to_)->blue / 256;

    for (y = 0; y < height; y++)
    {
	for (x = 0; x < width; x++)
	{
	    data[y*stride+x*channels+0] = ((from[0]
					    + ((to[0] - from[0]) / 2.0)
					    * (((double) x / (double) width)
					       + ((double) y / (double) height)))
					   + 0.5);
	    data[y*stride+x*channels+1] = ((from[1]
					    + ((to[1] - from[1]) / 2.0)
					    * (((double) x / (double) width)
					       + ((double) y / (double) height)))
					   + 0.5);
	    data[y*stride+x*channels+2] = ((from[2]
					    + ((to[2] - from[2]) / 2.0)
					    * (((double) x / (double) width)
					       + ((double) y / (double) height)))
					   + 0.5);
	}
    }

    image_changed (VIMAGE (img));
    return img;
}


/* DL hooks */

repv
rep_dl_init (void)
{
    rep_ADD_SUBR(Sdraw_vertical_gradient);
    rep_ADD_SUBR(Sdraw_horizontal_gradient);
    rep_ADD_SUBR(Sdraw_diagonal_gradient);
    rep_INTERN (gradient);
    return Qgradient;
}
