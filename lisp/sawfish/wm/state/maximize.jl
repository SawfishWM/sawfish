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

;;;###autoload
(defun maximize-window (w &optional direction)
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
    (resize-window-to w (car dims) (cdr dims))))

;;;###autoload
(defun unmaximize-window (w)
  (interactive "W")
  (let
      ((geom (window-get w 'unmaximized-geometry)))
    (when geom
      (resize-window-to w (nth 2 geom) (nth 3 geom))
      (move-window-to w (nth 0 geom) (nth 1 geom))
      (window-put w 'unmaximized-geometry nil))))

;;;###autoload
(defun maximize-window-vertically (w)
  (interactive "W")
  (maximize-window w 'vertical))

;;;###autoload
(defun maximize-window-horizontally (w)
  (interactive "W")
  (maximize-window w 'horizontal))

;;;###autoload
(defun maximize-window-toggle (w &optional direction)
  (interactive "W")
  (if (window-get w 'unmaximized-geometry)
      (unmaximize-window w)
    (maximize-window w direction)))

;;;###autoload
(defun maximize-window-vertically-toggle (w)
  (interactive "W")
  (maximize-window-toggle w 'vertical))

;;;###autoload
(defun maximize-window-horizontally-toggle (w)
  (interactive "W")
  (maximize-window-toggle w 'horizontal))
