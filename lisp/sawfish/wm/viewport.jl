;; viewport.jl -- virtual desktops

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.viewport

    (export
      ;; I export these, since viewport-hi uses them
      viewport-y-offset
      viewport-x-offset
      ;; set-viewport
      screen-viewport
      ;; moved o -hi
      ;; set-screen-viewport
      ;; select-workspace-and-viewport
      ;; move-viewport
      ;; move-viewport-to-window
      window-outside-workspace-p
      window-outside-viewport-p
      window-outside-shifted-viewport-p
      move-window-to-current-viewport
      set-window-viewport
      move-window-viewport
      window-viewport
      window-absolute-position
      ;; mmc:
      new-viewport
      viewport?
      restore-current-viewport
      save-current-viewport
      describe-window-position
      )

    (open
     rep
     rep.system
     rep.mmsystem
     rep.trace
     rep.io.files
     rep.lang.math                      ; logior
      
     sawfish.wm.windows
     sawfish.wm.misc
     sawfish.wm.events
     sawfish.wm.commands
     sawfish.wm.workspace
     sawfish.wm.custom
     sawfish.wm.session.init
     
     sawfish.wm.adt
     ;rep.data.records
     )

  (define debug #f)

;; Virtual workspaces are implemented by moving windows in and out of
;; the screen dimensions. E.g. moving to the left moves all windows one
;; screen-width to the right. 

  (defgroup viewport "Viewport" :group workspace)

(defcustom viewport-dimensions '(1 . 1)
    "The number of columns and rows of the virtual desktop: \\w"
    :group (workspace viewport)
    :type (pair (number 1) (number 1))
    :tooltip "This is meaningless if dynamic viewport is enabled."
    ;fixme: :after-set (lambda () (viewport-size-changed t))
    )

  (defcustom viewport-minimum-dimensions '(1 . 1)
    "Minimum number of columns and rows of virtual desktop (if boundary mode is dynamic): \\w"
    :group (workspace viewport)
    :type (pair (number 1) (number 1))
    :after-set (lambda () (viewport-minimum-size-changed)))

  (defcustom uniconify-to-current-viewport t
    "Windows uniconify to the current viewport."
    :type boolean
    :group (workspace viewport))

;;; raw viewport handling

  ;; The unit is pixel, NOT (col, row).
  ;; Position (0, 0) means the top-left of the workspace.
  (defvar viewport-x-offset 0)
  (defvar viewport-y-offset 0)


;;; we need these  2 global vars !!

;; resizing ??
  (define (describe-viewport text vp)
    (if (viewport? vp)
        (message (format #f "%s: %d %d" text (x-of vp) (y-of vp)))))

  (define (save-current-viewport vp)
    ;; copy the environment  -> ADT
    (if (viewport? vp)
        (progn
          (set-x! vp viewport-x-offset)
          (set-y! vp viewport-y-offset)
          (if debug (describe-viewport "save-current-viewport: " vp)))
      ;;(message "error: save-current-viewport: bad vp")
      ))

;; soft restore.
  (define (restore-current-viewport vp)
    (if (viewport? vp)
        (progn
          (if debug (describe-viewport "restore-current-viewport: " vp))
          (setq viewport-x-offset (x-of vp))
          (setq viewport-y-offset (y-of vp)))
      ;; (message (format #f "restore-current-viewport: %d %d" (x-of vp) (y-of vp)))
      ))


;;;
  (defvar special-window #f "")
  (defvar special-window-2 #f "")


  (define (describe-window-position w #!optional text)
    (if (windowp w)
        (let ((pos (window-position w)))
          (if (or (eq w special-window)
                  (eq w special-window-2))
              (message (format #f "%sdescribe-window-position:  %s (%s)\t %d, %d (%d)"
                               (or text "") (window-name w)
                               (if (window-visible-p w) "+" "-")
                               (car pos) (cdr pos) current-workspace))))))


;;; I wanted to understand when some window unexplainably was moved
;;  fixme:  it should log all windows when they get outside of ws

  '(mm-add-hook 'pre-command-hook
     (lambda ()
       (describe-window-position special-window "pre "))
     #f 'describe-window-position)


  '(mm-add-hook 'post-command-hook
     (lambda ()
       (describe-window-position special-window "post "))
     #f 'describe-window-position)

  '(mm-add-hook 'window-moved-hook
     (lambda (w)
       (when (eq w special-window)
         (describe-window-position special-window "moved ")
                                        ;(backtrace (stderr-file))
         ))
     #f 'describe-window-position)

;; screen sized viewport handling

  (define (screen-viewport)
    (cons (quotient viewport-x-offset (screen-width))
          (quotient viewport-y-offset (screen-height))))



;;;;---------------------------------------------------
;; (caaaar '(1 2 3 4))
  (define (caar a) (car (cdr a)))
  (define (cddr a) (cdr (cdr a)))
  (define (caaar a) (car (cddr a)))
  (define (caaaar a) (car (cdddr a)))


                                        ; 4-list  is rec2 outside rec1  ?
  (define (rectangles-disjoint rec1 rec2)
    (or (> (car rec1) (caaar rec2))
        (> (caar rec1) (caaaar rec2))
        (< (caaar rec1) (car rec2))
        (< (caaaar rec1) (caar rec2))))

  (rectangles-disjoint '(0 0 1 1) '(1 2 100 100))

  '(define (window-outside-vp-viewport-p w vp)
    (let ((position (get-window-position-on-ws w))
          )
      (rectangles-disjoint )))


  (define (window-outside-workspace-p window)
    (let ((pos (window-position window))
          (dims (window-frame-dimensions window))
          (left (- viewport-x-offset))
          (right (- (* (car viewport-dimensions) (screen-width))
                    viewport-x-offset))
          (top (- viewport-y-offset))
          (bottom (- (* (cdr viewport-dimensions) (screen-height))
                     viewport-y-offset)))
      (or (>= (car pos) right)
          (>= (cdr pos) bottom)
          (<= (+ (car pos) (car dims)) left)
          (<= (+ (cdr pos) (cdr dims)) top))))


  ;; the window, after moving by shift, will be inside?
  (define (window-outside-shifted-viewport-p window shift)
    (let ((pos (window-position window))
          (dims (window-frame-dimensions window)))
      (or (<= (+ (car shift) (car pos) (car dims)) 0)
          (<= (+ (cdr shift) (cdr pos) (cdr dims)) 0)
          (>= (+ (car shift) (car pos)) (screen-width))
          (>= (+ (cdr shift) (cdr pos)) (screen-height)))))

  (define (window-outside-viewport-p window)
    (window-outside-shifted-viewport-p window '(0 . 0)))

  (define (move-window-to-current-viewport window)
    (when (and (window-outside-viewport-p window)
               (not (window-get window 'sticky-viewport)))
      (let ((pos (window-position window)))
        (move-window-to window (mod (car pos) (screen-width))
                        (mod (cdr pos) (screen-height))))))

  (define (set-window-viewport window col row)
    (unless (window-get window 'sticky-viewport)
      (let ((pos (window-position window)))
        (setq col (max 0 (min (1- (car viewport-dimensions)) col)))
        (setq row (max 0 (min (1- (cdr viewport-dimensions)) row)))
        (setq col (+ (* col (screen-width)) (mod (car pos) (screen-width))))
        (setq row (+ (* row (screen-height)) (mod (cdr pos) (screen-height))))
        (move-window-to
         window (- col viewport-x-offset) (- row viewport-y-offset)))))

  (define (move-window-viewport window col row)
    (let ((pos (window-position window)))
      (set-window-viewport window
                           (+ (quotient (+ (car pos) viewport-x-offset)
                                        (screen-width)) col)
                           (+ (quotient (+ (cdr pos) viewport-y-offset)
                                        (screen-height)) row))))

  (define (window-viewport w)
    (let ((position (window-position w)))
      (cons (quotient (+ (car position) viewport-x-offset) (screen-width))
            (quotient (+ (cdr position) viewport-y-offset) (screen-height)))))

  (define (window-absolute-position w)
    ;; position on the monitor/real screen
    (let ((position (window-position w)))
      (if (window-outside-viewport-p w)
          (cons (mod (+ (car position) viewport-x-offset) (screen-width))
                (mod (+ (cdr position) viewport-y-offset) (screen-height)))
        position)))

  
;;; session management, config

  (define (viewport-saved-state w)
    (let ((position (window-position w)))
      (when (window-get w 'sticky-viewport)
        (rplaca position (mod (car position) (screen-width)))
        (rplacd position (mod (cdr position) (screen-height))))
      `((position . ,(window-absolute-position w))
        (viewport . ,(window-viewport w)))))

;;
  (define (viewport-load-state w alist)
    (let ((position (cdr (assq 'position alist)))
          (viewport (cdr (assq 'viewport alist))))
      (when position
        (if (or (not viewport) (window-get w 'sticky-viewport))
            (move-window-to w (car position) (cdr position))
          (move-window-to w (+ (* (car viewport) (screen-width))
                               (car position)
                               (- viewport-x-offset))
                          (+ (* (cdr viewport) (screen-height))
                             (cdr position)
                             (- viewport-y-offset)))
          (when (window-outside-workspace-p w)
            (move-window-to-current-viewport w)))
        (window-put w 'placed t))))
			     
  (sm-add-saved-properties 'sticky-viewport)
  (add-hook-s 'sm-window-save-functions viewport-saved-state)
  (add-hook-s 'sm-restore-window-hook viewport-load-state)


  (define (viewport-window-uniconified w)
    (when uniconify-to-current-viewport
      (move-window-to-current-viewport w)))

  (add-hook-s 'uniconify-window-hook viewport-window-uniconified))
