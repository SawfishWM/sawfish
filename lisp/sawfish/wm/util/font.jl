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
	    face->pango-description
	    xlfd-description->face
	    face->xlfd-description)

    (open rep
	  rep.system
	  rep.regexp
	  rep.data.records)

  ;; Xft font descriptions are <family-list>-<size>:<name>=<value>...

  ;; Pango font descriptions are [family-list] [style-options] [size]

  ;; XLFD font descriptions are -<foundry>-<family>-<weight>-<slant>-
  ;; <setwidth>-<add-style>-<pixel-size>-<point-size>-<res-x>-<res-y>-
  ;; <spacing>-<avg-width>-<charset-registry>-<charset-encoding>

  ;; We'll use a neutral "face" structure to describe fonts in a
  ;; general way. (it actually uses the Xft style names, but whatever)

  (define-record-type :face
    (make-face families size styles)
    face?
    (families face-families)
    (size face-size)
    (styles face-styles))

  (define-record-discloser :face
    (lambda (x)
      `(face ,(face-families x) ,(face-size x) ,(face-styles x))))

  (define (assoc-case x map)
    (let loop ((rest map))
      (cond ((null rest) nil)
	    ((string-equal x (caar rest)) (car rest))
	    (t (loop (cdr rest))))))

  (define (face-style face style)
    (cdr (assoc-case style (face-styles face))))


;; Xft naming scheme

  (define xft-abbrev-map
    '(("light" "weight" . "light")
      ("medium" "weight" . "medium")
      ("demibold" "weight" . "demibold")
      ("bold" "weight" . "bold")
      ("black" "weight" . "black")
      ("roman" "slant" . "roman")
      ("italic" "slant" . "italic")
      ("proportional" "spacing" . "proportional")
      ("mono" "spacing" . "mono")
      ("charcell" "spacing" . "charcell")
      ("rgb" "rgba" . "rgb")
      ("bgr" "rgba" . "bgr")
      ("vrgb" "rgba" . "vrgb")
      ("vbgr" "rgba" . "vbgr")))

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
	  (cond ((string-match "\\s*=\\s*" (car rest))
		 (loop (cdr rest)
		       (cons (cons (substring (car rest) 0 (match-start))
				   (substring (car rest) (match-end)))
			     styles)))
		((assoc-case (car rest) xft-abbrev-map)
		 (loop (cdr rest)
		       (cons (cdr (assoc-case (car rest)
					      xft-abbrev-map)) styles)))
		;; drop unknown single words..
		(t (loop (cdr rest) styles)))))))

  (define (face->xft-description face)
    (mapconcat identity
	       (cons (format nil "%s-%s" (face-families face) (face-size face))
		     (mapcar (lambda (x)
			       (cond ((rassoc x xft-abbrev-map)
				      (car (rassoc x xft-abbrev-map)))
				     ((cdr x)
				      (format nil "%s=%s" (car x) (cdr x)))
				     (t (car x))))
			     (face-styles face))) #\:))


;; Pango naming scheme

  (define pango-style-map
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
      (while (and fields (assoc-case (car fields) pango-style-map))
	(setq styles (cons (cdr (assoc-case
				 (car fields) pango-style-map)) styles))
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
	(let ((tem (rassoc (car rest) pango-style-map)))
	  (if tem
	      (loop (cdr rest) (cons (car tem) out))
	    (loop (cdr rest) out))))))


;; XLFD naming scheme

  (define xlfd-style-names
    '((3 . "weight")
      (5 . "width")
      (6 . "add-style")
      (8 . "point-size")
      (9 . "resolution-x")
      (10 . "resolution-y")
      (12 . "average-width")
      (13 . "charset-registry")
      (14 . "charset-encoding")))

  (define xlfd-slant-map
    '(("i" . "italic")
      ("o" . "oblique")
      ("r" . "roman")))

  (define xlfd-spacing-map
    '(("c" . "charcell")
      ("p" . "proportional")
      ("m" . "mono")))

  (define (xlfd-description->face name)
    (let ((fields (string-split "-" name))
	  (family "sans")
	  (size 12)
	  (styles '()))

      (unless (string= (nth 2 fields) "*")
	(setq family (nth 2 fields)))

      (unless (string= (nth 7 fields) "*")
	(setq size (string->number (nth 7 fields))))

      (let ((slant (nth 4 fields)))
	(when (assoc slant xlfd-slant-map)
	  (setq styles (cons (cons "slant"
				   (cdr (assoc slant xlfd-slant-map)))
			     styles))))

      (let ((spacing (nth 11 fields)))
	(when (assoc spacing xlfd-spacing-map)
	  (setq styles (cons (cons "spacing"
				   (cdr (assoc spacing xlfd-spacing-map)))
			     styles))))

      (do ((i 1 (1+ i)))
	  ((= i 14))
	(let ((field (nth i fields))
	      (name (assq i xlfd-style-names)))
	  (when (and name field (not (string= field "*")))
	    (setq styles (cons (cons (cdr (assq i xlfd-style-names))
				     (nth i fields)) styles)))))

      (make-face family size styles)))

  (define (face->xlfd-description face)
    (let ((out '()))

      (do ((i 14 (1- i)))
	  ((= i 0))

	(cond ((= i 2)
	       (setq out (cons (face-families face) out)))

	      ((= i 4)
	       (let ((slant (car (rassoc (face-style face "slant")
					 xlfd-slant-map))))
		 (setq out (cons (or slant "*") out))))

	      ((= i 7)
	       (setq out (cons (number->string (face-size face)) out)))

	      ((= i 11)
	       (let ((spacing (car (rassoc (face-style face "spacing")
					   xlfd-spacing-map))))
		 (setq out (cons (or spacing "*") out))))

	      (t (let ((style (assq i xlfd-style-names))
		       (name (cdr (assq i xlfd-style-names))))
		   (if (or (not style) (not name))
		       (setq out (cons "*" out))
		     (setq out (cons (or (face-style face name) "*") out)))))))

      (mapconcat identity (cons "" out) "-"))))
