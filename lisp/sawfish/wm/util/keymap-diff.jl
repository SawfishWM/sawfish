;; keymap-diff.jl -- functions for diffing and patching keymaps
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

(define-structure sawfish.wm.util.keymap-diff

    (export diff-keymaps
	    patch-keymap)

    (open rep)

  ;; sorts non-destructively
  (define (sort-keymap map) (cons 'keymap (sort (copy-sequence (cdr map)))))

  ;; diff script elements are: (+ BINDING) or (- BINDING)

  (define (diff-keymaps from-map to-map)
    "Returns a list describing the changes need to transform the keymap
FROM-MAP to the keymap TO-MAP."

    (let loop ((from-rest (cdr (sort-keymap from-map)))
	       (to-rest (cdr (sort-keymap to-map)))
	       (diff '()))

      (cond ((and (null from-rest) (null to-rest))
	     ;; both lists ended
	     (cons 'keymap-diff (nreverse diff)))

	    ((null from-rest)
	     ;; from ended, so need to add rest of to
	     (loop '() (cdr to-rest) (cons (list '+ (car to-rest)) diff)))

	    ((null to-rest)
	     ;; to ended, so need to subtract rest of from
	     (loop (cdr from-rest)
		   '()
		   (cons (list '- (car from-rest)) diff)))

	    ((equal (car from-rest) (car to-rest))
	     ;; both equal, keep going
	     (loop (cdr from-rest) (cdr to-rest) diff))

	    ((< (car from-rest) (car to-rest))
	     ;; extra item in from list, so subtract it
	     (loop (cdr from-rest) to-rest
		   (cons (list '- (car from-rest)) diff)))

	    ((> (car from-rest) (car to-rest))
	     ;; extra item in to list, so add it
	     (loop from-rest (cdr to-rest)
		   (cons (list '+ (car to-rest)) diff))))))

  (define (patch-keymap map diff)
    "Returns a new copy of the keymap MAP, transformed following by the list
DIFF, which should have been created by the `diff-keymaps' function."

    (or (eq (car diff) 'keymap-diff)
	(error "Malformed keymap diff script"))

    (let loop ((rest (cdr diff))
	       (new-map (copy-sequence (cdr map))))

         (cond ((null rest)
                ;; end of the script, return the new map
                (cons 'keymap new-map))

               ((eq (caar rest) '-)
                ;; something to remove
                (loop (cdr rest) (delete (cadar rest) new-map)))

               ((eq (caar rest) '+)
                ;; something to add
                (loop (cdr rest) (cons (cadar rest) new-map)))

               (t (error "Malformed item in diff script: %s" (car rest)))))))
