#| font.jl -- some font-related functions

   $Id$

   Author: John Harper <jsh@unfactored.org>

   Copyright (C) 2002 John Harper

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
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.util.font

    (export make-face
	    face?
	    face-families
	    face-size
	    face-styles
	    xft-description->face
	    face->xft-description
	    pango-description->face
	    face->pango-description)

    (open rep
	  rep.system
	  rep.regexp
	  rep.data.records)

  ;; Xft font descriptions are <family-list>-<size>:<name>=<value>...

  ;; Pango font descriptions are [family-list] [style-options] [size]

  (define-record-type :face
    (make-face families size styles)
    face?
    (families face-families)
    (size face-size)
    (styles face-styles))

  (define-record-discloser :face
    (lambda (x)
      `(face ,(face-families x) ,(face-size x) ,(face-styles x))))


;; Xft naming scheme

  (define (xft-description->face name)
    (let* ((fields (string-split "\\s*:\\s*" name))
	   (family "sans")
	   (size 12))

      ;; extract family and size
      (when (car fields)
	(cond ((string-match "\\s*-\\s*" (car fields))
	       (setq family (substring (car fields) 0 (match-start)))
	       (setq size (string->number
			   (substring (car fields) (match-end)))))
	      ((string-looking-at "\\d+" (car fields))
	       (setq size (string->number (car fields))))
	      (t (setq family (car fields)))))

      ;; extract styles
      (let loop ((rest (cdr fields))
		 (styles '()))
	(if (null rest)
	    (make-face family size (nreverse styles))
	  (if (string-match "\\s*=\\s*" (car rest))
	      (loop (cdr rest)
		    (cons (cons (substring (car rest) 0 (match-start))
				(substring (car rest) (match-end)))
			  styles))
	    (loop (cdr rest) (cons (cons (car rest)) styles)))))))

  (define (face->xft-description face)
    (mapconcat identity
	       (cons (format nil "%s-%s" (face-families face) (face-size face))
		     (mapcar (lambda (x)
			       (if (cdr x)
				   (format nil "%s=%s" (car x) (cdr x))
				 (car x))) (face-styles face))) #\:))


;; Pango naming scheme

  (define style-map
    '(("Oblique" "slant" . "oblique")
      ("Italic" "slant" . "italic")
      ("Small-Caps" "style" . "smallcaps")	;FIXME?
#|
      ("Ultra-Condensed" . ?)			;FIXME
      ("Extra-Condensed" . ?)
      ("Condensed" . ?)
      ("Semi-Condensed" . ?)
      ("Semi-Expanded" . ?)
      ("Expanded" . ?)
      ("Extra-Expanded" . ?)
      ("Ultra-Expanded" . ?)
|#
      ("Ultra-Light" "weight" . "ultralight")	;FIXME?
      ("Light" "weight" . "light")
      ("Medium" "weight" . "medium")
      ("Semi-Bold" "weight" . "demibold")
      ("Bold" "weight" . "bold")
      ("Ultra-Bold" "weight" . "ultrabold")	;FIXME?
      ("Heavy" "weight" . "black")))		;FIXME?

  (define (assoc-case x map)
    (let loop ((rest map))
      (cond ((null rest) nil)
	    ((string-equal x (caar rest)) (car rest))
	    (t (loop (cdr rest))))))

  (define (pango-description->face name)
    (let ((fields (string-split " " name))
	  (family "sans")
	  (size 12)
	  (styles '()))

      ;; have to parse backwards, since family names may contain spaces..
      (setq fields (nreverse fields))

      ;; look for a size at the end of the string
      (when (string-match "\\s*(\\d+)\\s*$" (car fields))
	(setq size (string->number (expand-last-match "\\1")))
	(setq fields (cdr fields)))

      ;; pop off style keywords
      (while (and fields (assoc-case (car fields) style-map))
	(setq styles (cons (cdr (assoc-case (car fields) style-map)) styles))
	(setq fields (cdr fields)))

      ;; whatever's left is the family name
      (setq family (mapconcat identity (nreverse fields) #\space))

      (make-face family size styles)))

  (define (face->pango-description face)
    (let loop ((rest (face-styles face))
	       (out '()))
      (if (null rest)
	  (format nil "%s %s %s"
		  (face-families face)
		  (mapconcat identity (nreverse out) #\space)
		  (face-size face))
	(let ((tem (rassoc (car rest) style-map)))
	  (if tem
	      (loop (cdr rest) (cons (car tem) out))
	    (loop (cdr rest) out)))))))
