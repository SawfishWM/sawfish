;; focus.jl -- implement standard focus behaviour
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

(define-structure sawfish.wm.focus

    (export define-focus-mode
	    autoload-focus-mode
	    focus-mode
	    set-focus-mode
	    focus-push-map
	    focus-pop-map)

    (open rep
	  rep.system
	  rep.util.autoloader
	  sawfish.wm.events
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.util.window-order
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.session.init)

  (defvar focus-modes nil
    "List containing all symbols naming focus modes.")

  (defcustom focus-mode 'enter-exit
    "When does the mouse pointer affect the input focus."
    :type symbol
    :user-level novice
    :group focus
    :before-set (lambda () (focus-mode-changed 'before))
    :after-set (lambda () (focus-mode-changed 'after)))

  (defcustom focus-click-through t
    "Does click-to-focus mode pass the click through to the window."
    :type boolean
    :user-level expert
    :group focus)

  (defvar focus-dont-push nil
    "When t, focusing a window doesn't change it's position in the stack of
most-recently focused windows.")


;;; general utilities

  (define (define-focus-mode name fun)
    "Define a new focus mode called NAME (a symbol). The function FUN will be
used to implement this focus mode, it will be called with arguments `(WINDOW
EVENT-NAME)', where EVENT-NAME may be one of the following symbols:
`pointer-in', `pointer-out', `focus-in', `focus-out', `add-window'
`before-mode-change', `after-mode-change'"

    (unless (memq name focus-modes)
      (setq focus-modes (nconc focus-modes (list name))))
    (custom-set-property 'focus-mode ':options focus-modes)
    (put name 'focus-mode fun))

  ;; autoload handling
  (define (getter name) (get name 'focus-mode))
  (define autoload-focus-mode (make-autoloader getter define-focus-mode))
  (define focus-mode-ref (autoloader-ref getter))

  (define (window-focus-mode w)
    (or (and w (windowp w) (window-get w 'focus-mode)) focus-mode))

  (define (focus-invoke-mode w . args)
    (let* ((mode (window-focus-mode w))
	   (fun (focus-mode-ref mode)))
      (when fun
	(apply fun w args))))

  (define (set-focus-mode w mode)
    "Set the focus mode of window W to the mode named by symbol MODE."
    (unless (eq (window-get w 'focus-mode) mode)
      (focus-invoke-mode w 'before-mode-change)
      (window-put w 'focus-mode mode)
      (focus-invoke-mode w 'after-mode-change)))

  (define (focus-mode-changed time)
    (let ((arg (if (eq time 'before)
		   'before-mode-change
		 'after-mode-change)))
      (map-windows
       (lambda (w)
	 (unless (window-get w 'focus-mode)
	   (focus-invoke-mode w arg))))))

  (define (focus-push-map w map)
    (let ((current (window-get w 'keymap))
	  (saved (window-get w 'focus-saved-keymap)))
      (unless (or (eq current map) (null current))
	(unless saved
	  (window-put w 'focus-saved-keymap current)
	  (window-put w 'ignore-fp-keymap t))
	(window-put w 'keymap map))))

  (define (focus-pop-map w)
    (let ((saved (window-get w 'focus-saved-keymap)))
      (when saved
	(window-put w 'keymap saved)
	(window-put w 'ignore-fp-keymap nil)
	(window-put w 'focus-saved-keymap nil))))


;;; modes

  (define-focus-mode 'enter-exit
    (lambda (w action . args)
      (case action
	((pointer-in)
	 (when (window-really-wants-input-p w)
	   (set-input-focus w)))
	((pointer-out)
	 ;; ignore grab/ungrab leave events
	 (when (eq (car args) 'normal)
	   (set-input-focus nil)))
	((enter-root)
	 ;; ensure that any desktop window gets focused
	 (set-input-focus w)))))

  (define-focus-mode 'enter-only
    (lambda (w action)
      (case action
	((pointer-in)
	 (when (window-really-wants-input-p w)
	   (set-input-focus w))))))

  (define (focus-click)
    (let ((w (current-event-window))
	  (event (current-event)))
      (when (window-really-wants-input-p w)
	(set-input-focus w))
      (focus-pop-map w)
      ;; do we need to do anything with the event?
      (if (and event (or (window-get w 'focus-click-through)
			 focus-click-through
			 (not (window-really-wants-input-p w))))
	  ;; allow-events called with replay-pointer ignores any passive
	  ;; grabs on the window, thus if the wm has a binding in the
	  ;; window's keymap, it would be ignored. So search manually..
	  (let ((command (lookup-event-binding event)))
	    (cond (command
		   (call-command command))
		  ((not (progn
			  (require 'sawfish.wm.util.decode-events)
			  (should-grab-button-event-p
			   event (window-get w 'keymap))))
		   ;; pass the event through to the client window unless we
		   ;; need to keep the grab for the events that would follow
		   (allow-events 'replay-pointer)
		   (forget-button-press))))
	;; ungrab the pointer so that the non-click-through thing
	;; works for window decorations as well as the client
	;; (does this break anything?)
	(when (window-really-wants-input-p w)
	  (ungrab-pointer)
	  (forget-button-press)))
      ;; set-input-focus may not actually change the focus
      (unless (eq (input-focus) w)
	(focus-push-map w click-to-focus-map))))

  (defvar click-to-focus-map
    (bind-keys (make-keymap)
      ;; apparently buttons 4 & 5 are usually bound to the
      ;; wheel on some mice
      "Any-Button1-Click1" focus-click
      "Any-Button2-Click1" focus-click
      "Any-Button3-Click1" focus-click))

  (define-focus-mode 'click
    (lambda (w action)
      (case action
	((focus-in)
	 (focus-pop-map w))
	((focus-out add-window)
	 (unless (eq w (input-focus))
	   (focus-push-map w click-to-focus-map)))
	((before-mode-change)
	 (focus-pop-map w))
	((after-mode-change)
	 (unless (eq w (input-focus))
	   (focus-push-map w click-to-focus-map))))))


;;; hooks

  (define (focus-enter-fun w mode)
    (cond ((desktop-window-p w)
	   (focus-invoke-mode w 'enter-root mode))
	  ((windowp w)
	   (focus-invoke-mode w 'pointer-in mode))))

  (define (focus-leave-fun w mode)
    (cond ((desktop-window-p w)
	   (focus-invoke-mode w 'leave-root mode))
	  ((windowp w)
	   (focus-invoke-mode w 'pointer-out mode))))

  (define (focus-in-fun w)
    (focus-invoke-mode w 'focus-in)
    (unless focus-dont-push
      (window-order-push w)))

  (define (focus-out-fun w) (focus-invoke-mode w 'focus-out))
  (define (focus-add-window w) (focus-invoke-mode w 'add-window))

  (add-hook 'enter-notify-hook focus-enter-fun t)
  (add-hook 'leave-notify-hook focus-leave-fun t)
  (add-hook 'focus-in-hook focus-in-fun t)
  (add-hook 'focus-out-hook focus-out-fun t)
  (add-hook 'map-notify-hook focus-add-window)

  (add-hook 'after-initialization-hook
	    (lambda () (map-windows focus-add-window)))

  (sm-add-saved-properties 'never-focus 'focus-mode)


;; bug prevention

  ;; XXX Pavel reported this bug (windows becoming unfocusable in
  ;; XXX click-to-focus mode). I can't see why it happens, hence this
  ;; XXX kludge for now..

  (define (scan-windows-for-bugs)
    (map-windows (lambda (w)
		   (when (eq (window-focus-mode w) 'click)
		     ;; check that the correct keymaps are in place
		     (unless (or (eq (input-focus) w)
				 (not (window-mapped-p w))
				 (eq (window-get w 'keymap)
				     click-to-focus-map))
		       (format standard-error
			       "Window lost focus keymap: %s, %s\n"
			       (window-name w) (window-get w 'keymap))
		       (beep) (beep)
		       (focus-push-map w click-to-focus-map))))))

  (add-hook 'idle-hook scan-windows-for-bugs))
