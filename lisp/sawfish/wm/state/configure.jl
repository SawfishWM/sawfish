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
  "Automatically choose window gravity by position on screen."
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
	  (rplacd coords (- (cdr coords) (/ (- (cdr tem) (cdr dims)) 2))))
	(setq dims tem)))
    (when (setq tem (cdr (assq 'position alist)))
      (setq coords tem))
    (move-resize-window-to w (car coords) (cdr coords) (car dims) (cdr dims))))

(defun choose-window-gravity-by-position (w)
  (let
      ((coords (window-position w))
       (dims (window-frame-dimensions w))
       i)
    (rplaca coords (+ (car coords) (/ (car dims) 2)))
    (rplacd coords (+ (cdr coords) (/ (cdr dims) 2)))
    (setq i (+ (max 0 (min 2 (/ (car coords) (/ (screen-width) 3))))
	       (max 0 (min 6 (* 3 (/ (cdr coords) (/ (screen-height) 3)))))))
    (window-put w 'gravity (aref [north-west north north-east
				  west center east
				  south-west south south-east] i))))

(defun choose-window-gravity-callback (w)
  (when configure-auto-gravity
    (choose-window-gravity-by-position w)))

(add-hook 'configure-request-hook configure-request-handler)
(add-hook 'after-add-window-hook choose-window-gravity-callback)
(add-hook 'window-moved-hook choose-window-gravity-callback)
