;; grow-pack.jl -- window resize and movement
;; Id: grow-pack.jl,v 1.6 2000/02/22 16:50:18 grossjoh Exp 

;; Copyright (C) 2000 Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>

;; This file is free software; you can redistribute it and/or modify it
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

;;; Commentary:

;; This package provides functions to `grow' or `pack' a window in
;; four directions.  `Growing' means to grow the window in the
;; indicated direction until it `bumps into' another window.
;; `Packing' means to move the window in the indicated direction until
;; it `bumps into' another window.

;; Copy this file into a directory which is on your load-path, then
;; use it.  I installed this package by placing the following into my
;; ~/.sawmillrc:
;;
;; (require 'grow-pack)
;; (require 'menus)
;; (setq window-ops-menu
;;       (append window-ops-menu
;;               (list (cons "Grow/pack" grow-pack-menu))))


;;; Code:

(defcustom grow-window-repeat t
  "Whether growing an already grown window grows it again."
  :type boolean
  :group maximize)

(defcustom grow-is-maximize t
  "Whether growing is considered to be maximization.  When you turn
this on, you can use `unmaximize-window' or something similar to get
back to the original size."
  :type boolean
  :group maximize)

;; Entry points.

;;;###autoload
(defun grow-window-left (w)
  "Grows window to the left until it `bumps into' another window."
  (interactive "%W")
  (grow-window w 'left))

;;;###autoload
(defun grow-window-right (w)
  "Grows window to the right until it `bumps into' another window."
  (interactive "%W")
  (grow-window w 'right))

;;;###autoload
(defun grow-window-up (w)
  "Grows window upwards until it `bumps into' another window."
  (interactive "%W")
  (grow-window w 'up))

;;;###autoload
(defun grow-window-down (w)
  "Grows window downwards until it `bumps into' another window."
  (interactive "%W")
  (grow-window w 'down))

;;;###autoload
(defun pack-window-left (w)
  "Moves window to the left until it `bumps into' another window."
  (interactive "%W")
  (pack-window w 'left))

;;;###autoload
(defun pack-window-right (w)
  "Moves window to the right until it `bumps into' another window."
  (interactive "%W")
  (pack-window w 'right))

;;;###autoload
(defun pack-window-up (w)
  "Moves window upwards until it `bumps into' another window."
  (interactive "%W")
  (pack-window w 'up))

;;;###autoload
(defun pack-window-down (w)
  "Moves window downwards until it `bumps into' another window."
  (interactive "%W")
  (pack-window w 'down))

;; Convenience variable.

(defvar grow-pack-menu
  '(("Grow left" grow-window-left)
    ("Grow right" grow-window-right)
    ("Grow up" grow-window-up)
    ("Grow down" grow-window-down)
    ("Pack left" pack-window-left)
    ("Pack right" pack-window-right)
    ("Pack up" pack-window-up)
    ("Pack down" pack-window-down))
  "Menu of grow and pack operations.")

;; Implementation part.

(require 'rects)
(require 'maximize)

(defun gp-avoid-windows (w direction)
  "Returns list of windows to avoid when growing/filling window W in DIRECTION."
  (let* ((wpos    (window-position w))
         (wdim    (window-frame-dimensions w))
         (wleft   (car wpos))
         (wtop    (cdr wpos))
         (wright  (+ wleft (car wdim)))
         (wbottom (+ wtop (cdr wdim)))
         (nleft   wleft)
         (ntop    wtop)
         (nright  wright)
         (nbottom wbottom))
    (when (eq direction 'left)
      (setq nleft 0)
      (when grow-window-repeat
        (setq wleft (max (- wleft 1) 0))))
    (when (eq direction 'right)
      (setq nright (screen-width))
      (when grow-window-repeat
        (setq wright (min (+ wright 1) (screen-width)))))
    (when (eq direction 'up)
      (setq ntop 0)
      (when grow-window-repeat
        (setq wtop (max (- wtop 1) 0))))
    (when (eq direction 'down)
      (setq nbottom (screen-height))
      (when grow-window-repeat
        (setq wbottom (min (+ wbottom 1) (screen-height)))))
    (filter
             (lambda (x)
       (let* ((xpos (window-position x))
              (xdim (window-frame-dimensions x))
              (xleft (car xpos))
              (xtop (cdr xpos))
              (xright (+ xleft (car xdim)))
              (xbottom (+ xtop (cdr xdim))))
               ;; If window does not overlap W but does overlap the
               ;; larger W, then we need to avoid this window.
         (and (window-appears-in-workspace-p x current-workspace)
              (<= (rect-2d-overlap* (list xleft xtop xright xbottom)
                        (list wleft wtop wright wbottom)) 0)
              (> (rect-2d-overlap* (list xleft xtop xright xbottom)
                                   (list nleft ntop nright nbottom)) 0))))
     (managed-windows))))

(defun gp-surrounding-rect (wlist)
  "Returns the rectangle surrounding all given windows."
  (if wlist
      (let* ((w (car wlist))
             (wrest (cdr wlist))
             (wpos (window-position w))
             (wdim (window-frame-dimensions w))
             (rleft (car wpos))
             (rtop  (cdr wpos))
             (rright (+ rleft (car wdim)))
             (rbottom (+ rtop (cdr wdim))))
        (mapcar
         (lambda (x)
           (let* ((xpos (window-position x))
                  (xdim (window-frame-dimensions x))
                  (xleft (car xpos))
                  (xtop  (cdr xpos))
                  (xright (+ xleft (car xdim)))
                  (xbottom (+ xtop (cdr xdim))))
             (when (< xleft rleft) (setq rleft xleft))
             (when (< xtop  rtop) (setq rtop xtop))
             (when (> xright rright) (setq rright xright))
             (when (> xbottom rbottom) (setq rbottom xbottom))))
         wrest)
        (list rleft rtop rright rbottom))
    (list (screen-width) (screen-height) 0 0)))

(defun grow-window (w direction)
  "Grows window W in DIRECTION."
  (let* ((avoid-wins (gp-avoid-windows w direction))
         (surround   (gp-surrounding-rect avoid-wins))
         (wpos       (window-position w))
         (wdim       (window-dimensions w))
	 (fdim       (window-frame-dimensions w))
         (wleft      (car wpos))
         (wtop       (cdr wpos))
         (wwidth     (car wdim))
         (wheight    (cdr wdim))
	 (nwidth     wwidth)
	 (nheight    wheight)
         (sleft      (nth 0 surround))
         (stop       (nth 1 surround))
         (sright     (nth 2 surround))
         (sbottom    (nth 3 surround)))
    (when (eq direction 'left)
      (setq nwidth (- (+ wleft wwidth) sright)))
    (when (eq direction 'up)
      (setq nheight (- (+ wtop wheight) sbottom)))
    (when (eq direction 'right)
      (setq nwidth (- sleft wleft (- (car fdim) wwidth))))
    (when (eq direction 'down)
      (setq nheight (- stop wtop (- (cdr fdim) wheight))))
    (let
	((tem (cons nwidth nheight)))
      (maximize-truncate-dims w tem)	;truncate to column/row increments
      (setq nwidth (car tem))
      (setq nheight (cdr tem)))
    (when (eq direction 'left)
      (setq wleft (- wleft (- nwidth wwidth))))
    (when (eq direction 'up)
      (setq wtop (- wtop (- nheight wheight))))
    (when grow-is-maximize
      (unless (window-get w 'unmaximized-geometry)
        (window-put w 'unmaximized-geometry (list (car wpos) (cdr wpos)
                                                  (car wdim) (cdr wdim))))
      (if (memq direction '(left right))
          (window-put w 'maximized-horizontally t)
        (window-put w 'maximzed-vertically t)))
    (move-resize-window-to w wleft wtop nwidth nheight)
    (when maximize-raises (raise-window w))
    (when grow-is-maximize
      (call-window-hook 'window-maximized-hook w
			(list (if (member direction '(left right))
				  'horizontal 'vertical)))
      (call-window-hook 'window-state-change-hook w (list '(maximized))))))

(defun pack-window (w direction)
  (let* ((avoid-wins (gp-avoid-windows w direction))
         (surround   (gp-surrounding-rect avoid-wins))
         (wpos       (window-position w))
         (wdim       (window-frame-dimensions w))
         (wleft      (car wpos))
         (wtop       (cdr wpos))
         (wwidth     (car wdim))
         (wheight    (cdr wdim))
         (sleft      (nth 0 surround))
         (stop       (nth 1 surround))
         (sright     (nth 2 surround))
         (sbottom    (nth 3 surround)))
    (when (eq direction 'left) (setq wleft sright))
    (when (eq direction 'up) (setq wtop sbottom))
    (when (eq direction 'right) (setq wleft (- sleft wwidth)))
    (when (eq direction 'down) (setq wtop (- stop wheight)))
    (move-window-to w wleft wtop)
    (call-window-hook 'after-move-hook w
                      (list (list (if (memq direction '(left right))
                                      'horizontal 'vertical))))))

(provide 'grow-pack)

;; grow-pack.jl ends here.
