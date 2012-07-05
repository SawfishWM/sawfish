;;; mousetrap.jl -- Zoom the mouse to any place with few keystrokes.

;; Author: Fernando Carmona Varo  <ferkiwi@gmail.com>
;; Maintainer: Christopher Roy Bratusek <nano@tuxfamily.org>
;; Version: 0.7

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

;;; Commentary:

;; This script provides a method for moving the mouse to any place of
;; the screen by zooming with just a few keystrokes (and as much
;; precision as you want).

;; When the command is invoked the mouse is warped to the center of
;; the screen and a outline is drawn dividing the screen in 4
;; squares (like trapping the mouse on it).

;; You can then press an arrow key (or H J K or L key) and the mouse will
;; move to a new position inside the outline, moving half the distance
;; between it's current position and the edge of the "cage".
;; Each new keystroke will half the area of the cage, increasing 
;; precision exponentially.
;; When you press the spacebar or enter a mouse click is performed. If
;; you press ESC no click will be made, but the mouse will be moved to
;; that position.

;; The idea was inspired by a similar proyect shown in the Yahoo
;; Hack Day '06: http://www.semicomplete.com/presentations/hackday06/

;; The keys Q W A and S can also be used for warping the mouse to one of the
;; four quarters of the square, which would result in even less 
;; keystrokes.

(define-structure sawfish.wm.ext.mousetrap

    (export mousetrap-action
            synthetic-click)

    (open rep
	  rep.system
	  rep.io.timers
          sawfish.wm
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.animation.modes
          sawfish.wm.util.x)

  (define-structure-alias mousetrap sawfish.wm.ext.mousetrap)
  
  (defcustom mousetrap-outline 'crosshair
    "Outline drawing to use for mousetrap area."
    :type (choice crosshair none)
    :group misc)

  (defcustom mousetrap-cursor-shape 'fleur
    "Mouse cursor shape to use for mousetrap."
    :type (choice fleur cross crosshair iron_cross target)
    :group misc)

  (defvar mousetrap-escape-key nil)

  (defvar mousetrap-move-right-key nil)
  (defvar mousetrap-move-left-key nil)
  (defvar mousetrap-move-up-key nil)
  (defvar mousetrap-move-down-key nil)

  (defvar mousetrap-move-quarter-top-right-key nil)
  (defvar mousetrap-move-quarter-top-left-key nil)
  (defvar mousetrap-move-quarter-bottom-right-key nil)
  (defvar mousetrap-move-quarter-bottom-left-key nil)

  (define (draw-crosshair-outline x y width height)
    "Crosshair outline drawing."
    (let ((gc (x-create-root-xor-gc))
	  (x-step (/ width 2))
	  (y-step (/ height 2)))
      (do ((i 0 (1+ i)))
	  ((= i 3))
	;; Horizontal line
	(x-draw-line 'root gc
		      (cons x 
			    (inexact->exact (round (+ y (* y-step i)))))
		      (cons (+ x width) 
			    (inexact->exact (round (+ y (* y-step i))))))
	;; Vertical line
	(x-draw-line 'root gc
		      (cons (inexact->exact (round (+ x (* x-step i)))) 
			    y)
		      (cons (inexact->exact (round (+ x (* x-step i)))) 
			    (+ y height))))
      ;; Crosshair ruler
      (do ((x (+ x (/ x-step 2)))
	    (y (+ y (/ y-step 2)))
	    (i 0 (1+ i)))
	  ((= i 2))
	(x-draw-line 'root gc
		      (cons (inexact->exact (round (+ x (* x-step 0.47))))
			    (inexact->exact (round (+ y (* y-step i)))))
		      (cons (inexact->exact (round (+ x (* x-step 0.53))))
			    (inexact->exact (round (+ y (* y-step i))))))
	(x-draw-line 'root gc
		      (cons (inexact->exact (round (+ x (* x-step i))))
			    (inexact->exact (round (+ y (* y-step 0.47)))))
		      (cons (inexact->exact (round (+ x (* x-step i))))
			  (inexact->exact (round (+ y (* y-step 0.53)))))))
      (x-destroy-gc gc)))

  (define-window-outliner 'crosshair draw-crosshair-outline)

  ;; prevent edge actions from beeing invoked while mousetrapping
  (defvar while-mousetrap nil)
  (defvar server-grabbed nil)

  (eval-when-compile
    (defvar mousetrap-read-event nil))

  (define (mousetrap-action)
    "Let the user move the mouse by 'trapping' it to a position.
Returns the event-name of the key pressed to finish, nil if cancelled."
    (when (grab-keyboard)
      (unwind-protect
        (setq while-mousetrap t)

	  (define (mousetrap-read-event)
	    "Keyboard events callback for `mousetrap-action'."
	    (throw 'mousetrap-read (event-name (current-event))))
	(add-hook 'unbound-key-hook mousetrap-read-event)

	    (grab-server) ; don't draw anything else
	    (grab-pointer nil mousetrap-cursor-shape)
	    (setq server-grabbed t)
	    
	      (catch 'exit-mousetrap
		(let* (
		      (override-keymap '(keymap))
		      (pos-x 0)
		      (pos-y 0)
		      (dim-x (screen-width))
		      (dim-y (screen-height))
		      (key ""))
		  
		  (while t
		    ;; Update cursor
		    (warp-cursor (floor (+ pos-x (/ dim-x 2)))
				(floor (+ pos-y (/ dim-y 2))))
		      
		    ;; Draw outline
		    (unless (eq mousetrap-outline 'none)
		      (draw-window-outline mousetrap-outline pos-x pos-y dim-x dim-y))
		    ;; Read a key
		      (setq key
			    (catch 'mousetrap-read
			      (recursive-edit)))
		      ;; Erase outline
		      (unless (eq mousetrap-outline 'none)
			(erase-window-outline mousetrap-outline pos-x pos-y dim-x dim-y))
		      ;; Act according to the key read
		      (cond ((or (equal key mousetrap-escape-key)
				 (equal key "ESC"))
			    (throw 'exit-mousetrap nil))
			    ((or (equal key "RET")
				 (equal key "SPC"))
			    (throw 'exit-mousetrap key))
			    ((or (equal key mousetrap-move-left-key); Left half
			         (equal key "Left")
			         (equal key "h"))
			    (setq dim-x (floor (/ dim-x 2))))
			    ((or (equal key mousetrap-move-up-key); Upper half
			         (equal key "Up")
			         (equal key "u"))
			    (setq dim-y (floor (/ dim-y 2))))
			    ((or (equal key mousetrap-move-down-key); Lower half
			         (equal key "Down")
			         (equal key "n"))
			    (setq pos-y (floor (+ pos-y (/ dim-y 2))))
			    (setq dim-y (floor (/ dim-y 2))))
			    ((or (equal key mousetrap-move-right-key); Right half
			         (equal key "Right")
			         (equal key "j"))
			    (setq pos-x (floor (+ pos-x (/ dim-x 2))))
			    (setq dim-x (floor (/ dim-x 2))))
			    ((or (equal key mousetrap-move-quarter-top-left-key) ; top-left quarter
			         (equal key "M-Left")
			         (equal key "r"))
			    (setq dim-y (floor (/ dim-y 2)))
			    (setq dim-x (floor (/ dim-x 2))))
			    ((or (equal key mousetrap-move-quarter-top-right-key) ; top-right quarter
			         (equal key "M-Right")
			         (equal key "t"))
			    (setq dim-y (floor (/ dim-y 2)))
			    (setq pos-x (floor (+ pos-x (/ dim-x 2))))
			    (setq dim-x (floor (/ dim-x 2))))
			    ((or (equal key mousetrap-move-quarter-bottom-left-key) ; bottom-left quarter
			         (equal key "C-Left")
			         (equal key "v"))
			    (setq pos-y (floor (+ pos-y (/ dim-y 2))))
			    (setq dim-y (floor (/ dim-y 2)))
			    (setq dim-x (floor (/ dim-x 2))))
			    ((or (equal key mousetrap-move-quarter-bottom-right-key) ; bottom-right quarter
			         (equal key "C-Right")
			         (equal key "b"))
			    (setq pos-y (floor (+ pos-y (/ dim-y 2))))
			    (setq dim-y (floor (/ dim-y 2)))
			    (setq pos-x (floor (+ pos-x (/ dim-x 2))))
			    (setq dim-x (floor (/ dim-x 2))))
			    ))))
	    (ungrab-server)
	    (ungrab-pointer)
	    (remove-hook 'unbound-key-hook mousetrap-read-event)
	    (ungrab-keyboard)))
            (make-timer (lambda () (setq while-mousetrap nil)) 1))

  (define (synthetic-click event-name)
    (let* ((w (query-pointer-window))
	  (event (lookup-event event-name)))
      ;; focus, if allowed
      (when (window-really-wants-input-p w)
	(set-input-focus w))
      ;; send click to window
      (when (or focus-click-through 
		(not (window-really-wants-input-p w)))
	(synthesize-event event w))
      ;; raise
      (raise-window w)))

  (define (mousetrap-invoke)
    "Move mouse by mousetrapping."
    (interactive)
    (mousetrap-action))

  (define (mousetrap-invoke-and-left-click)
    "Move mouse by mousetrapping and make a left click."
    (interactive)
    (and (mousetrap-action)
	(synthetic-click "Any-Button1-Click1")))

  (define (mousetrap-invoke-and-left-double-click )
    "Move mouse by mousetrapping and make a left double click."
    (interactive)
    (and (mousetrap-action)
	(synthetic-click "Any-Button1-Click2")))

  (define (mousetrap-invoke-and-middle-click)
    "Move mouse by mousetrapping and make a middle click."
    (interactive)
    (and (mousetrap-action)
	(synthetic-click "Any-Button2-Click1")))

  (define (mousetrap-invoke-and-right-click)
    "Move mouse by mousetrapping and make a right click."
    (interactive)
    (and (mousetrap-action)
	(synthetic-click "Any-Button3-Click1")))

  (define-command 'mousetrap-invoke mousetrap-invoke)
  (define-command 'mousetrap-invoke-and-left-click mousetrap-invoke-and-left-click)
  (define-command 'mousetrap-invoke-and-left-double-click mousetrap-invoke-and-left-double-click)
  (define-command 'mousetrap-invoke-and-middle-click mousetrap-invoke-and-middle-click)
  (define-command 'mousetrap-invoke-and-right-click mousetrap-invoke-and-right-click))


