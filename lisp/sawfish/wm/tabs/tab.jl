;; tab.jl - emulate fluxbox tab system
;;
;; Copyright (C) Yann Hodique <Yann.Hodique@lifl.fr>
;;
;; This file is an official accepted contribution into sawfish.
;;
;; This script is free software; you can redistribute it and/or modify it
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

(define-structure sawfish.wm.tabs.tab

    (export tab-add-to-group
            raise-tabs-on-hover-setter)

    (open rep
          rep.system
          sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.frames
          sawfish.wm.tabs.tabgroup
          sawfish.wm.util.marks
          sawfish.wm.windows
          sawfish.wm.stacking)

  (define-structure-alias tab sawfish.wm.tabs.tab)

  ;; TODO:
  ;; - change other tab sizes when window resizes itself
  ;; - make calculations work with tiny windows
  ;; - hide some frame parts on leftmost and rightmost tabs
  ;; - add a drag-n-drop way to group windows by tabs

  ;;###autoload (defgroup tabs "Tabs")

  (defgroup tabs "Tabs")

  (defcustom tab-left-dec-width 11 "Width of tab's left-edge decoration"
    :group tabs
    :type number)

  (defcustom tab-right-dec-width 11 "Width of tab's right-edge decoration"
    :group tabs
    :type number)

  (defcustom tab-left-margin 16 "Width of tab area's left-edge decoration"
    :group tabs
    :type number)

  (defcustom tab-right-margin 16 "Width of tab area's right-edge decoration"
    :group tabs
    :type number)

  (defcustom tab-raise-on-hover nil
    "Raise Tabs on Hover"
    :group tabs
    :type boolean
    :after-set (lambda () (raise-tabs-on-hover-setter)))

  (define (get-tab-pos win)
    (let* ((group (tab-find-window win))
       	   (tabnum (tab-rank win (tab-group-window-list group))))
      (tab-pos group tabnum win)))

  (define (tab-pos group tabnum win)
    "find the left and right pixel offsets of a tab"
    (let* ((tabarea-width (+
                           ;; get width of a window in this group
                           ;;(car (window-dimensions
                           ;;      (car (tab-group-window-list group))))
                           (car (window-dimensions win))
                           (- tab-left-margin)
                           (- tab-right-margin)))
           (numtabs (length (tab-group-window-list group)))
           (left (quotient (* tabnum tabarea-width) numtabs))
           ;; the right edge is not always "left + (window-width / numtabs)"
           ;; that would be inaccurate due to rounding errors
           (right (quotient (* (+ tabnum 1) tabarea-width) numtabs))
           (width (- right left)))
      (list left right width)))

  (define (tab-title-text-width win)
    "width of the title text area is the tabwidth minus decorations"
    (let* ((tabwidth (nth 2 (get-tab-pos win))))
      (+ tabwidth
         (- tab-left-dec-width)
         (- tab-right-dec-width))))

  (define (tab-left-edge win)
    "Compute left edge of tab"
    (let* ((left (nth 0 (get-tab-pos win))))
      (+ left tab-left-margin)))

  (define (tab-right-dec-pos win)
    "Compute position of tab's right-edge decoration"
    (let* ((right (nth 1 (get-tab-pos win))))
      (+ right tab-left-margin (- tab-right-dec-width))))

  (define (tab-title-left-edge win)
    "Compute left edge of tab"
    (+ (tab-left-edge win) tab-left-dec-width))

  ;; new class : tab
  (define-frame-class 'tab
    `((cursor . left_ptr)
      (x-justify . center)
      (y-justify . center)
      (left-edge . ,tab-title-left-edge)
      (width . ,tab-title-text-width)))
  (set-frame-part-value 'tab 'keymap 'title-keymap)

  (define-frame-class 'tab-l
    `((cursor . left_ptr)
      (left-edge . ,tab-left-edge)) t)

  (define-frame-class 'tab-r
    `((cursor . left_ptr)
      (left-edge . ,tab-right-dec-pos)) t)

  (define (mygroup win)
    (if (marked-windows)
        (progn
          (apply-on-marked-windows (lambda (w) (tab-group-window w win)))
          (unmark-all-windows))
      (mark-window win)))

  (define-command 'tab-add-to-group mygroup #:spec "%W")

  (define (raise-tabs-on-hover-action win)
    (raise-window win))

  (define (raise-tabs-on-hover-setter)
    (if (eq tab-raise-on-hover 't)
        (add-hook 'enter-frame-part-hook raise-tabs-on-hover-action)
      (remove-hook 'enter-frame-part-hook raise-tabs-on-hover-action))))

;;(require 'x-cycle)
;;(define-cycle-command-pair
;;  'cycle-tabgroup 'cycle-tabgroup-backwards
;;  (lambda (w)
;;    (delete-if-not window-in-cycle-p
;;                   (delete-if (lambda (win)
;;                                (and (not (eq win w))
;;                                     (tab-same-group-p win w)))
;;                              (workspace-windows current-workspace))
;;                   )
;;    )
;;  #:spec "%W")

;;(require 'sawfish.wm.util.window-order)

