;; configure.jl -- default configure-request handler
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'configure)

(defcustom configure-auto-gravity t
  "Automatically select window gravity from position on screen."
  :type boolean
  :group (move advanced))

(defun configure-request-handler (w alist)
  (let
      ((coords (window-position w))
       (dims (window-dimensions w))
       (fdims (window-frame-dimensions w))
       tem)
    (when (setq tem (cdr (assq 'stack alist)))
      (cond ((eq tem 'below)
	     (lower-window w))
	    ((eq tem 'above)
	     ;; for the old GNOME pager's benefit..
	     (uniconify-window w)
	     (raise-window w))))
    (when (setq tem (cdr (assq 'dimensions alist)))
      (let*
	  ((hints (window-size-hints w))
	   (gravity (or (window-get w 'gravity)
			(cdr (assq 'window-gravity hints)))))
	(cond
	 (gravity
	  (when (memq gravity '(east south-east north-east))
	    ;; [x] placed relative to the right of the frame
	    (rplaca coords (- (car coords) (- (car tem) (car dims)))))
	  (when (memq gravity '(north center south))
	    ;; [x] placed relative to the center
	    (rplaca coords (- (car coords) (/ (- (car tem) (car dims)) 2))))
	  (when (memq gravity '(south south-east south-west))
	    ;; [y] placed relative to the bottom of the frame
	    (rplacd coords (- (cdr coords) (- (cdr tem) (cdr dims)))))
	  (when (memq gravity '(north center south))
	    ;; [y] placed relative to the center
	    (rplacd coords (- (cdr coords) (/ (- (cdr tem) (cdr dims)) 2)))))

	 ;; I confess: I used Mathematica to find these identities. s
	 ;; is the screen size, l and r the left and right offsets from
	 ;; the screen edges, and c the window width. Capitalised
	 ;; values are after the resize.
	 ;;
	 ;; In[1]:= Solve[{s==l+r+c, s==L+R+C, l/r == L/R}, {L,R}]
	 ;;
	 ;;                -(C l) + l s         r (C - s)
	 ;; Out[1]= {{L -> ------------, R -> -(---------)}}
	 ;;                   l + r               l + r
	 ((or configure-auto-gravity (window-get w 'auto-gravity))
	  (let
	      ;; XXX assumes constant frame size
	      ((ftem (cons (+ (car tem) (- (car fdims) (car dims)))
			   (+ (cdr tem) (- (cdr fdims) (cdr dims))))))
	    (rplaca coords (/ (+ (- (* (car ftem) (car coords)))
				 (* (car coords) (screen-width)))
			      (- (screen-width) (car fdims))))
	    (rplacd coords (/ (+ (- (* (cdr ftem) (cdr coords)))
				 (* (cdr coords) (screen-height)))
			      (- (screen-height) (cdr fdims)))))))
	(setq dims tem)))
    (when (setq tem (cdr (assq 'position alist)))
      (setq coords tem))
    (move-resize-window-to w (car coords) (cdr coords) (car dims) (cdr dims))))

(add-hook 'configure-request-hook configure-request-handler)
