;; nokogiri-utils -- utils for SawfishConfig
;;
;; Copyright (C) 2010 Christopher Roy Bratusek <zanghar@freenet.de>
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

(define-structure sawfish.cfg.utils

  (export beautify-symbol-name
          remove-newlines)

  (open rep
        rep.regexp)

  (define (beautify-symbol-name symbol)
    (cond ((stringp symbol) symbol)
	  ((not (symbolp symbol)) (format "%s" symbol))
	  (t
	   (let ((name (copy-sequence (symbol-name symbol))))
	     (while (string-match "[-:]" name)
	       (setq name (concat (substring name 0 (match-start))
				  ?  (substring name (match-end)))))
	     (aset name 0 (char-upcase (aref name 0)))
	     (_ name)))))

  (define (remove-newlines string)
    (let loop ((point 0)
	       (out '()))
         (if (string-match "\n" string point)
             (loop (match-end)
                   (list* #\space (substring string point (match-start)) out))
           (apply concat (nreverse (cons (substring string point) out)))))))
