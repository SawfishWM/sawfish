;; maximize.jl -- window maximization
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

(provide 'maximize)

;; Commentary:

;; This sets the window property `unmaximized-geometry' of each
;; currently maximize window to `(X Y W H)', the saved geometry.

;; called when a window is maximized, args (W &optional DIRECTION)
(defvar window-maximized-hook nil)

;; called when a window is un-maximized, args (W &optional DIRECTION)
(defvar window-unmaximized-hook nil)

(defun window-maximized-p (w)
  (window-get w 'unmaximized-geometry))

(defun window-maximized-horizontally-p (w)
  (and (zerop (car (window-position w)))
       (= (car (window-frame-dimensions w)) (screen-width))))

(defun window-maximized-vertically-p (w)
  (and (zerop (cdr (window-position w)))
       (= (cdr (window-frame-dimensions w)) (screen-height))))

;;;###autoload
(defun maximize-window (w &optional direction)
  "Maximize the dimensions of the window."
  (interactive "W")
  (let
      ((coords (window-position w))
       (dims (window-dimensions w))
       (fdims (window-frame-dimensions w)))
    (unless (window-get w 'unmaximized-geometry)
      (window-put w 'unmaximized-geometry (list (car coords) (cdr coords)
						(car dims) (cdr dims))))
    (when (or (null direction) (eq direction 'horizontal))
      (rplaca coords 0)
      (rplaca dims (- (screen-width) (- (car fdims) (car dims)))))
    (when (or (null direction) (eq direction 'vertical))
      (rplacd coords 0)
      (rplacd dims (- (screen-height) (- (cdr fdims) (cdr dims)))))
    (move-window-to w (car coords) (cdr coords))
    (resize-window-to w (car dims) (cdr dims))
    (call-window-hook 'window-maximized-hook w (list direction))
    (call-window-hook 'window-state-change-hook w)))

;;;###autoload
(defun unmaximize-window (w &optional direction)
  "Restore the dimensions of the window to its original, unmaximized, state."
  (interactive "W")
  (let
      ((geom (window-get w 'unmaximized-geometry))
       (coords (window-position w))
       (dims (window-dimensions w)))
    (when geom
      (when (or (null direction) (eq direction 'horizontal))
	(rplaca coords (nth 0 geom))
	(rplaca dims (nth 2 geom)))
      (when (or (null direction) (eq direction 'vertical))
	(rplacd coords (nth 1 geom))
	(rplacd dims (nth 3 geom)))
      (resize-window-to w (car dims) (cdr dims))
      (move-window-to w (car coords) (cdr coords))
      (when (and (= (car coords) (nth 0 geom))
		 (= (cdr coords) (nth 1 geom))
		 (= (car dims) (nth 2 geom))
		 (= (cdr dims) (nth 3 geom)))
	(window-put w 'unmaximized-geometry nil))
      (call-window-hook 'window-unmaximized-hook w direction)
      (call-window-hook 'window-state-change-hook w))))

;;;###autoload
(defun maximize-window-vertically (w)
  "Maximize the vertical dimension of the window."
  (interactive "W")
  (maximize-window w 'vertical))

;;;###autoload
(defun maximize-window-horizontally (w)
  "Maximize the horizontal dimension of the window."
  (interactive "W")
  (maximize-window w 'horizontal))

;;;###autoload
(defun maximize-window-toggle (w &optional direction)
  "Toggle the state of the window between maximized and unmaximized."
  (interactive "W")
  (if (window-maximized-p w)
      (unmaximize-window w direction)
    (maximize-window w direction)))

;;;###autoload
(defun maximize-window-vertically-toggle (w)
  "Toggle the state of the window between vertically maximized and
unmaximized."
  (interactive "W")
  (maximize-window-toggle w 'vertical))

;;;###autoload
(defun maximize-window-horizontally-toggle (w)
  "Toggle the state of the window between horizontally maximized and
unmaximized."
  (interactive "W")
  (maximize-window-toggle w 'horizontal))


;; initialisation

(sm-add-saved-properties 'unmaximized-geometry)
