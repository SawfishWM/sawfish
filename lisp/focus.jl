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

(require 'window-order)
(provide 'focus)

(defvar focus-modes nil
  "List containing all symbols naming focus modes.")

(defcustom focus-mode 'enter-exit
  "When does the mouse pointer affect the input focus."
  :type symbol
  :group focus
  :before-set (lambda () (focus-mode-changed 'before))
  :after-set (lambda () (focus-mode-changed 'after)))

(defcustom focus-click-through t
  "Does click-to-focus mode pass the click through to the window."
  :type boolean
  :group (focus advanced))

(defvar focus-dont-push nil
  "When t, focusing a window doesn't change it's position in the stack of most-
recently focused windows.")


;; general utilities

(defun define-focus-mode (name fun)
  "Define a new focus mode called NAME (a symbol). The function FUN will be
used to implement this focus mode, it will be called with arguments `(WINDOW
EVENT-NAME)', where EVENT-NAME may be one of the following symbols:
`pointer-in', `pointer-out', `focus-in', `focus-out', `add-window'
`before-mode-change', `after-mode-change'"
  (unless (memq name focus-modes)
    (setq focus-modes (nconc focus-modes (list name))))
  (custom-set-property 'focus-mode ':options focus-modes)
  (put name 'focus-mode fun))

(defun focus-invoke-mode (w &rest args)
  (let*
      ((mode (or (and w (window-get w 'focus-mode)) focus-mode))
       (fun (get mode 'focus-mode)))
    (when fun
      (apply fun w args))))

(defun set-focus-mode (w mode)
  "Set the focus mode of window W to the mode named by symbol MODE."
  (unless (eq (window-get w 'focus-mode) mode)
    (focus-invoke-mode w 'before-mode-change)
    (window-put w 'focus-mode mode)
    (focus-invoke-mode w 'after-mode-change)))

(defun focus-mode-changed (time)
  (let
      ((arg (if (eq time 'before)
		'before-mode-change
	      'after-mode-change)))
    (map-windows
     (lambda (w)
       (unless (window-get w 'focus-mode)
	 (focus-invoke-mode w arg))))))

(defun focus-push-map (w map)
  (let
      ((current (window-get w 'keymap))
       (saved (window-get w 'focus-saved-keymap)))
    (unless (or (eq current map) (null current))
      (unless saved
	(window-put w 'focus-saved-keymap current))
      (window-put w 'keymap map))))

(defun focus-pop-map (w)
  (let
      ((saved (window-get w 'focus-saved-keymap)))
    (when saved
      (window-put w 'keymap saved)
      (window-put w 'focus-saved-keymap nil))))


;; modes

(define-focus-mode 'enter-exit
 (lambda (w action)
   (cond ((eq action 'pointer-in)
	  (when (window-really-wants-input-p w)
	    (set-input-focus w)))
	 ((memq action '(pointer-out enter-root))
	  (set-input-focus nil)))))

(define-focus-mode 'enter-only
 (lambda (w action)
   (cond ((eq action 'pointer-in)
	  (when (and (not (eq w 'root)) (window-really-wants-input-p w))
	    (set-input-focus w))))))

(defvar click-to-focus-map
  (bind-keys (make-keymap)
    ;; apparently buttons 4 & 5 are usually bound to the
    ;; wheel on some mice
    "Any-Button1-Click1" 'focus-click
    "Any-Button2-Click1" 'focus-click
    "Any-Button3-Click1" 'focus-click))

(defun focus-click (w)
  (interactive "%w")
  (when (window-really-wants-input-p w)
    (set-input-focus w))
  (focus-pop-map w)
  (when (or (window-get w 'focus-click-through)
	    focus-click-through
	    (not (window-really-wants-input-p w)))
    ;; there's a problem here. allow-events called with replay-pointer
    ;; ignores any passive grabs on the window, thus if the wm has a
    ;; binding in the window's keymap, it would be ignored. So search
    ;; manually..
    (let
	((command (lookup-event-binding (current-event))))
      (cond (command
	     (call-command command))
	    ((not (should-grab-button-event-p
		   (current-event) (window-get w 'keymap)))
	     ;; pass the event through to the client window unless we
	     ;; need to keep the grab for the events that would follow
	     (allow-events 'replay-pointer))))))

(define-focus-mode 'click
 (lambda (w action)
   (cond ((eq action 'focus-in)
	  (focus-pop-map w))
	 ((memq action '(focus-out add-window))
	  (unless (eq w (input-focus))
	    (focus-push-map w click-to-focus-map)))
	 ((eq action 'before-mode-change)
	  (focus-pop-map w))
	 ((eq action 'after-mode-change)
	  (unless (eq w (input-focus))
	    (focus-push-map w click-to-focus-map))))))


;; hooks

(defun focus-enter-fun (w)
  (cond ((windowp w)
	 (focus-invoke-mode w 'pointer-in))
	((eq w 'root)
	 (focus-invoke-mode (input-focus) 'enter-root))))

(defun focus-leave-fun (w)
  (cond ((windowp w)
	 (focus-invoke-mode w 'pointer-out))
	((eq w 'root)
	 (focus-invoke-mode (input-focus) 'leave-root))))

(defun focus-in-fun (w)
  (focus-invoke-mode w 'focus-in)
  (unless focus-dont-push
    (window-order-push w)))

(defun focus-out-fun (w)
  (focus-invoke-mode w 'focus-out))

(defun focus-add-window (w)
  (focus-invoke-mode w 'add-window))

(add-hook 'enter-notify-hook focus-enter-fun t)
(add-hook 'leave-notify-hook focus-leave-fun t)
(add-hook 'focus-in-hook focus-in-fun t)
(add-hook 'focus-out-hook focus-out-fun t)
(add-hook 'after-add-window-hook focus-add-window)

(sm-add-saved-properties 'never-focus 'focus-mode)
