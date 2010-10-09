;; recolor-image.jl -- simple code for recolouring images
;;
;; Copyright (C) 2001 Eazel, Inc.
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Authors: John Harper <jsh@eazel.com>

;; this module is marked as being fully-safe, so don't add any
;; dangerous functions!

(define-structure sawfish.wm.util.recolor-image

    (export make-image-recolorer*
	    make-image-recolorer
	    red-channel
	    green-channel
	    blue-channel
	    alpha-channel)

    (open rep
	  sawfish.wm.images
	  sawfish.wm.colors)

  (defconst red-channel 0)
  (defconst green-channel 1)
  (defconst blue-channel 2)
  (defconst alpha-channel 3)

  (define original-key (make-symbol "recolor-image/original"))

  (define (make-image-recolorer* colors #!key
				 (zero-channel red-channel)
				 (index-channels (list green-channel))
				 (save-original t))

    ;; XXX add doc string here..

    (let* ((color-rgbs (mapcar color-rgb-8 colors))
	   (red-luts (mapcar (lambda (c)
			       (make-lut (nth red-channel c))) color-rgbs))
	   (green-luts (mapcar (lambda (c)
				 (make-lut (nth green-channel c))) color-rgbs))
	   (blue-luts (mapcar (lambda (c)
				(make-lut (nth blue-channel c))) color-rgbs)))

      (lambda (image)

	;; Get the original state of the image
	(if (image-get image original-key)
	    ;; Hmm.. don't have a blit-image function, this will do
	    (tile-image image (image-get image original-key))

	  (when save-original
	    ;; save a copy for when we recolor in future
	    (image-put image original-key (copy-image image))))

	(image-map (lambda (pixel)
		     (when (zerop (nth zero-channel pixel))
		       (let loop ((rest index-channels)
				  (red-rest red-luts)
				  (green-rest green-luts)
				  (blue-rest blue-luts))
			 (when rest
			   (let ((index (nth (car rest) pixel)))
			     (if (not (zerop index))
				 (list (aref (car red-rest) index)
				       (aref (car green-rest) index)
				       (aref (car blue-rest) index)
				       (nth alpha-channel pixel))
			       (loop (cdr rest)
				     (cdr red-rest)
				     (cdr green-rest)
				     (cdr blue-rest))))))))
		   image))))

  (define (make-image-recolorer color #!key
				(zero-channel red-channel)
				(index-channel green-channel)
				(save-original t))

    "Returns a function of a single argument. Given an image this
function will recolor it based on COLOR. Any pixels in the image whose
ZERO-CHANNEL is zero will be given a new value, computed by indexing
into a 256 element array using the value of the pixel's INDEX-CHANNEL
to choose the element.

The original image is always modified. If SAVE-ORIGINAL is true, the
first time the image is recolored it's original contents will be
stored, and then used as the basis for any future recoloring."

    (make-image-recolorer* (list color)
			   #:zero-channel zero-channel
			   #:index-channels (list index-channel)
			   #:save-original save-original))

  ;; Create a lookup-table, a string containing 256 samples of a gradient
  ;; from 0 -> MID -> 255.
  (define (make-lut mid)
    (let ((lut (make-string 256))
	  (i 0))
      (while (< i 128)
	(aset lut i (inexact->exact (round (* (/ mid 128.) i))))
	(setq i (1+ i)))
      (while (< i 256)
	(aset lut i (inexact->exact
		     (round (+ mid (* (/ (- 255. mid) 128.) (- i 128.))))))
	(setq i (1+ i)))
      lut)))
