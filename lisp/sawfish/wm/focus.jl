;; focus.jl -- implement standard focus behaviour

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

(define-structure sawfish.wm.focus

    (export define-focus-mode
	    autoload-focus-mode
	    focus-mode
	    set-focus-mode
	    focus-push-map
	    focus-pop-map
	    warp-pointer-if-necessary
            focus-revert
	    focus-within-click-event)

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

  (defcustom focus-mode 'click
    "When does the mouse pointer affect the input focus."
    :type symbol
    :group focus
    :before-set (lambda () (focus-mode-changed 'before))
    :after-set (lambda () (focus-mode-changed 'after)))

  (defcustom focus-click-through t
    "Click-to-focus mode passes the click through to the application."
    :type boolean
    :group focus)

  (defcustom focus-dont-push nil
    "Whether focusing a window doesn't change it's position in the stack."
    :type boolean
    :group focus)

  (defcustom focus-ignore-pointer-events nil
    "When true, pointer in/out events don't cause focus changes."
    :type boolean
    :group focus)

  (define focus-within-click-event (make-fluid nil)
          "When true, the current command is being called from within
a click-to-focus button press event.")

;;; general utilities

  (define (define-focus-mode name fun)
    "Define a new focus mode called NAME (a symbol). The function FUN will be
used to implement this focus mode, it will be called with arguments `(WINDOW
EVENT-NAME)', where EVENT-NAME may be one of the following symbols:
`pointer-in', `pointer-out', `focus-in', `focus-out', `add-window'
`before-mode-change', `after-mode-change', `warp-if-necessary' and
`focus-revert'."

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
	  (window-put w 'focus-saved-keymap current))
	(window-put w 'keymap map))))

  (define (focus-pop-map w)
    (let ((saved (window-get w 'focus-saved-keymap)))
      (when saved
	(window-put w 'keymap saved)
	(window-put w 'focus-saved-keymap nil))))

  ;; W is the focused window. Warp to it if a good idea
  (define (warp-pointer-if-necessary #!optional (w (input-focus)))
    (focus-invoke-mode w 'warp-if-necessary))

  ;; Put focus in some window that hopefully feels natural.  This is
  ;; typically used when the window in focus has disappeared.  Thus
  ;; there is no reference window.
  (define (focus-revert)
    (let ((fun (focus-mode-ref focus-mode)))
      (when fun (fun nil 'focus-revert))))

;;; modes

  #|
  From sawfish-1.5.0, enter-notify events invoked by grab are
  ignored in enter-exit and enter-only. This has a down side, too,
  but first let us see what's the intention.

  The problem that this solves is when you press Alt F2 in KDE, the
  Run Command popup appears and gets focused (if focus-when-mapped is
  non-nil), you start typing, the completion list pops up and takes
  focus, then you type a bit further and the completion list
  disappears.  Focus used to revert to the window the pointer was in,
  which usually was not the Run Command popup.  transient-unmap-window
  tries hard to get it right, then the grab-induced enter-notify
  brought focus to the wrong window anyway.

  The problem that this causes is when you open the KDE main menu by
  clicking the K icon in the panel, move the mouse into some
  application window and click to dismiss the open menu, focus reverts
  to the panel. You have to move the pointer out of the application
  window and back in to get it focused.
  |#
  (define-focus-mode 'enter-exit
    (lambda (w action . args)
      (case action
	((pointer-in)
	 (when (and (window-really-wants-input-p w)
		    ;; ignore grab/ungrab enter events
		    ;; See the above long comment
		    (eq (car args) 'normal))
	   (set-input-focus w)))
	((pointer-out)
	 ;; ignore grab/ungrab leave events
	 (when (eq (car args) 'normal)
	   (set-input-focus nil)))
	((enter-root)
	 ;; ensure that any desktop window gets focused
	 (set-input-focus w))
	((warp-if-necessary)
	 (unless (eq (query-pointer-window) w)
	   (warp-cursor-to-window w)))
        ((focus-revert)
         (setq w (query-pointer-window))
         (when (or (null w)
                   (window-really-wants-input-p w))
           (set-input-focus w))))))

  (define-focus-mode 'enter-only
    (lambda (w action . args)
      (case action
	((pointer-in)
	 (when (and (window-really-wants-input-p w)
		    ;; ignore grab/ungrab enter events
		    ;; See the comment above enter-exit
		    (eq (car args) 'normal))
	   (set-input-focus w)))
	((warp-if-necessary)
	 (let ((current (query-pointer-window)))
	   (unless (or (eq current w) (desktop-window-p current))
	     (warp-cursor-to-window w))))
        ((focus-revert)
         (setq w (query-pointer-window))
         (when (or (null w)
                   (desktop-window-p w))
           (setq w (window-order-most-recent)))
         (when (or (null w)
                   (window-really-wants-input-p w))
           (set-input-focus w))))))

  (define (focus-click)
    (let ((w (current-event-window))
	  (event (current-event)))
      (when (window-really-wants-input-p w)
	(set-input-focus w))
      (focus-pop-map w)
      ;; do we need to do anything with the event?
      (when event
	;; allow-events called with replay-pointer ignores any passive
	;; grabs on the window, thus if the wm has a binding in the
	;; window's keymap, it would be ignored. So search manually..
	(let ((command (lookup-event-binding event)))
	  (if command
	      (let-fluids ((focus-within-click-event t))
                 (call-command command))
	    (require 'sawfish.wm.util.decode-events)
	    (when (and (or focus-click-through
			   (window-get w 'focus-click-through)
			   (not (window-really-wants-input-p w)))
		       (not (should-grab-button-event-p
			     event (window-get w 'keymap))))
	      ;; pass the event through to the client window unless we
	      ;; need to keep the grab for the events that would follow
	      (allow-events 'replay-pointer)
	      (forget-button-press)))))
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
	 (unless (or (not (window-really-wants-input-p w))
		     (eq w (input-focus)))
	   (focus-push-map w click-to-focus-map)))
	((before-mode-change)
	 (focus-pop-map w))
	((after-mode-change)
	 (unless (or (not (window-really-wants-input-p w))
		     (eq w (input-focus)))
	   (focus-push-map w click-to-focus-map)))
        ((focus-revert)
         (setq w (window-order-most-recent))
         (when (or (null w)
                   (window-really-wants-input-p w))
           (set-input-focus w))))))

  (define-focus-mode 'enter-click
    (lambda (w action . args)
      (case action
        ((pointer-in warp-if-necessary focus-revert)
         (apply (focus-mode-ref 'enter-only) w action args))
        ((focus-in focus-out add-window before-mode-change after-mode-change)
         (apply (focus-mode-ref 'click) w action args)))))

;;; hooks

  (define (focus-enter-fun w mode)
    (unless focus-ignore-pointer-events
      (cond ((desktop-window-p w)
	     (focus-invoke-mode w 'enter-root mode))
	    ((windowp w)
	     (focus-invoke-mode w 'pointer-in mode)))))

  (define (focus-leave-fun w mode)
    (unless focus-ignore-pointer-events
      (cond ((desktop-window-p w)
	     (focus-invoke-mode w 'leave-root mode))
	    ((windowp w)
	     (focus-invoke-mode w 'pointer-out mode)))))

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

  (call-after-state-changed
   '(never-focus)
   (lambda (w)
     (if (window-get w 'never-focus)
	 (focus-pop-map w)
       (focus-add-window w))))

  (add-hook 'after-initialization-hook
	    (lambda () (map-windows focus-add-window)))

  (sm-add-saved-properties 'never-focus 'focus-mode)

;;; bug prevention

  ;; XXX Pavel reported this bug (windows becoming unfocusable in
  ;; XXX click-to-focus mode). I can't see why it happens, hence this
  ;; XXX kludge for now..

  (define (scan-windows-for-bugs)
    (map-windows (lambda (w)
		   (when (eq (window-focus-mode w) 'click)
		     ;; check that the correct keymaps are in place
		     (unless (or (eq (input-focus) w)
				 (not (window-mapped-p w))
				 (not (window-really-wants-input-p w))
				 (eq (window-get w 'keymap)
				     click-to-focus-map))
		       ;;(format standard-error
                       ;;        "Window lost focus keymap: %s, %s\n"
                       ;;        (window-name w) (window-get w 'keymap))
                       ;;(beep) (beep)
		       (focus-push-map w click-to-focus-map))))))

  (add-hook 'idle-hook scan-windows-for-bugs))
