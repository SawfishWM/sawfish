;; x-cycle.jl -- stack-based window cycling
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

;; Commentary:

;; Cycles through windows in MRU order. Whichever key is used to invoke
;; `cycle-windows' will continue to the next window on the stack when
;; pressed again. Releasing the initial modifier ends the cycling and
;; selects the current window

;; Thanks to Kuba Winnicki <blackwine@optimus.wroc.pl> for the idea:

;; [ assumes command invoked by MOD-KEY, i.e. M-TAB by default ]

;; MOD held;

;; * window 1            window 1            window 1          * window 3
;;   window 2    ==>   * window 2    ==>     window 2    ==>     window 1
;;   window 3    KEY     window 3    KEY   * window 3  release   window 2
;;   window 4            window 4            window 4    MOD     window 4

;; If cycle-raise-windows is enabled, focused window is brought to
;; front, but gets back to original placement in order when it's
;; defocused.

;; Also it'd be nice to be able to cycle through hidden windows as
;; well, like here:

;; MOD held;

;; * window 1            window 1            window 1          * window 3
;;  [window 2]   ==>   * window 2    ==>    [window 2]   ==>     window 1
;;  [window 3]   KEY    [window 3]   KEY   * window 3  release  [window 2]
;;   window 4            window 4            window 4    MOD     window 4

;; Each window may have an x-cycle-order property, an integer id
;; defining its position in the window stack, higher numbers equal
;; more recently selected. The ids may not be contiguous

;; Obviously there would be a problem when we overflow rep's integers, but
;; every now and then we compress the stack to make the ids contiguous

;; It might seem as though it should be possible to use the actual
;; window stacking to define MRU order. But since there are multiple
;; layers of windows this wouldn't work (selected windows may not reach
;; the top of the stack)

(define-structure sawfish.wm.commands.x-cycle

    (export define-cycle-command)

    (open rep
	  rep.system
	  rep.regexp
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.util.window-order
	  sawfish.wm.util.keymap
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.stacking
	  sawfish.wm.events
	  sawfish.wm.util.decode-events
	  sawfish.wm.util.groups
	  sawfish.wm.util.display-window)

  (define-structure-alias x-cycle sawfish.wm.commands.x-cycle)

  ;; we bind to disable-auto-raise and tooltips-enabled
  (eval-when-compile (require 'sawfish.wm.ext.auto-raise))
  (eval-when-compile (require 'sawfish.wm.ext.tooltips))

;;; customization options

  ;;###autoload (defgroup cycle "Window Cycling" :group focus :require sawfish.wm.commands.x-cycle)
  (defgroup cycle "Window Cycling" :group focus :require sawfish.wm.commands.x-cycle)

  (defcustom cycle-show-window-names t
    "Display window names while cycling through windows."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-include-iconified t
    "Include iconified windows when cycling."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-all-workspaces nil
    "Include windows on all workspaces when cycling."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-all-viewports nil
    "Include windows on all viewports when cycling."
    :group (focus cycle)
    :type boolean)

  (defcustom cycle-raise-windows t
    "Raise windows while they're temporarily selected during cycling."
    :group (focus cycle)
    :user-level expert
    :type boolean)

  (defcustom cycle-warp-pointer t
    "Warp the mouse pointer to windows as they're temporarily selected."
    :group (focus cycle)
    :user-level expert
    :type boolean)

  (defcustom cycle-focus-windows t
    "Focus windows when they're temporarily selected during cycling."
    :group (focus cycle)
    :user-level expert
    :type boolean)

  (defcustom cycle-disable-auto-raise nil
    "Disable auto-raising while temporarily selecting windows."
    :group (focus cycle)
    :user-level expert
    :type boolean)


;; variables

  ;; the current window
  (define x-cycle-current (make-fluid))

  ;; the original stacking order
  (define x-cycle-stacking (make-fluid))

  ;; the list of windows being cycled through
  (define x-cycle-windows (make-fluid))

  ;; alist of names (FORWARD . REVERSE) of all cycle commands
  (define cycle-commands '())


;;; code

  (define (forwards lst elt count)
    (let ((total (length lst))
	  (current (let loop ((rest lst) (i 0))
		     (cond ((null rest) 0)
			   ((eq (car rest) elt) i)
			   (t (loop (cdr rest) (1+ i)))))))
      (nth (mod (+ current count) total) lst)))

  (define (merge-unsorted x y)
    (let loop ((rest y)
	       (out x))
      (cond ((null rest) out)
	    ((memq (car rest) out) (loop (cdr rest) out))
	    (t (loop (cdr rest) (cons (car rest) out))))))

  (define ((cycle-next count))
    (let ((win (window-order (if cycle-all-workspaces
				 nil
			       current-workspace)
			     cycle-include-iconified cycle-all-viewports)))
      (unless (eq (fluid x-cycle-windows) t)
	(setq win (delete-if (lambda (w)
			       (not (memq w (fluid x-cycle-windows)))) win)))
      (setq win (delete-if-not window-in-cycle-p win))
      (unless win
	(throw 'x-cycle-exit t))
      (if (fluid x-cycle-current)
	  (when (or (window-get (fluid x-cycle-current) 'iconified)
		    (not (window-appears-in-workspace-p
			  (fluid x-cycle-current) current-workspace)))
	    (hide-window (fluid x-cycle-current)))
	;; first call, push the currently focused window onto
	;; the top of the stack
	(when (input-focus)
	  (fluid-set x-cycle-current (input-focus))
	  (window-order-push (fluid x-cycle-current))
	  (setq win (cons (fluid x-cycle-current)
			  (delq (fluid x-cycle-current) win)))))
      (when (fluid x-cycle-stacking)
	(restack-windows (fluid x-cycle-stacking))
	(fluid-set x-cycle-stacking nil))
      (if (fluid x-cycle-current)
	  (setq win (forwards win (fluid x-cycle-current) count))
	(setq win (car win)))
      (fluid-set x-cycle-current win)
      (when (not (window-get win 'sticky))
	(select-workspace (nearest-workspace-with-window
			   win current-workspace)))
      (move-viewport-to-window win)
      (when (window-get win 'iconified)
	(show-window win))
      (when cycle-raise-windows
	(fluid-set x-cycle-stacking (stacking-order))
	(raise-window win))
      (when cycle-warp-pointer
	(warp-cursor-to-window win))
      (when cycle-show-window-names
	(display-message (concat (and (window-get win 'iconified) ?[)
				 (window-name win)
				 (and (window-get win 'iconified) ?]))))
      (when (and cycle-focus-windows (window-really-wants-input-p win))
	(set-input-focus win))
      (allow-events 'sync-keyboard)))

  (define (x-cycle-exit) (throw 'x-cycle-exit t))

  (define (cycle-begin windows step)
    "Cycle through all windows in order of recent selections."
    (let ((tail-command nil))
      (let-fluids ((x-cycle-current nil)
		   (x-cycle-stacking nil)
		   (x-cycle-windows (or windows t)))
	(let* ((event (current-event))
	       (decoded (decode-event event))
	       (modifier-keys (apply append (mapcar modifier->keysyms
						    (nth 1 decoded))))
	       (eval-modifier-events t)
	       (eval-key-release-events t)
	       (override-keymap (make-keymap))
	       (focus-dont-push t)
	       (disable-auto-raise cycle-disable-auto-raise)
	       (tooltips-enabled nil)
	       (grab-win (input-focus))
	       (unmap-notify-hook (cons (lambda (w)
					  (when (eq w grab-win)
					    (setq grab-win nil)
					    (or (grab-keyboard nil nil t)
						(throw 'x-cycle-exit nil))
					    (allow-events 'sync-keyboard)))
					unmap-notify-hook))
	       (enter-workspace-hook (cons (lambda (space)
					     (when grab-win
					       (setq grab-win nil)
					       (or (grab-keyboard nil nil t)
						   (throw 'x-cycle-exit nil))
					       (allow-events 'sync-keyboard)))
					   enter-workspace-hook))
	       (unbound-key-hook
		(list (lambda ()
			(let ((ev (decode-event (current-event))))
			  (unless (or (memq 'release (nth 1 ev))
				      (modifier-keysym-p (nth 2 ev)))
			    ;; want to search the usual keymaps
			    (setq override-keymap nil)
			    (setq tail-command (lookup-event-binding
						(current-event)))
			    (unless tail-command
			      ;; no wm binding, so forward the event to
			      ;; the focused window (this is why we have
			      ;; to grab the keyboard synchronously)
			      (allow-events 'replay-keyboard))
			    (throw 'x-cycle-exit nil)))))))

	  (unless (and (eq 'key (car decoded)) (nth 1 decoded))
	    (error "%s must be bound to a key event with modifiers."
		   this-command))

	  ;; Use the event that invoked us to contruct the keymap
	  (bind-keys override-keymap event (cycle-next step))

	  ;; search the global-keymap for any bindings of the
	  ;; associated reverse command
	  (let ((reversed (reciprocal-command this-command)))
	    (when reversed
	      (let ((keys (where-is reversed)))
		(mapc (lambda (k)
			(bind-keys override-keymap k (cycle-next (- step))))
		      keys))))

	  (mapc (lambda (k)
		  (bind-keys override-keymap
		    (encode-event `(key (release any) ,k)) x-cycle-exit))
		modifier-keys)

	  ;; grab synchronously, so that event replaying works
	  (when (grab-keyboard (input-focus) nil t)
	    (unwind-protect
		(progn
		  (catch 'x-cycle-exit
		    ;; do the first step
		    ((cycle-next step))
		    (recursive-edit))
		  (when (fluid x-cycle-current)
		    (display-window (fluid x-cycle-current))))
	      (display-message nil)
	      (ungrab-keyboard)))))

      (when tail-command
	;; make sure that the command operates on the newly-focused
	;; window, not the window that was focused when the original
	;; event was received
	(current-event-window (input-focus))
	(call-command tail-command))))

  ;; return the name of the command cycling in the opposite direction
  ;; to NAME
  (define (reciprocal-command name)
    (or (cdr (assq name cycle-commands))
	(car (rassq name cycle-commands))))

;;; public entry point

  (define (define-cycle-command forward-name reverse-name selector . rest)
    "Create a pair of commands for cycling through windows. The command named
FORWARD-NAME cycles forwards, while the command named REVERSE-NAME cycles
backwards.

SELECTOR is called when initializing the cycle environment, it should
return the list of windows to cycle through, or the symbol `t' to
denote all cyclable windows.

Any extra arguments are passed to each call to define-command."
    (define (command-body step)
      (lambda args
	(let ((windows (apply selector args)))
	  (when windows
	    (cycle-begin windows step)))))
    (apply define-command forward-name (command-body +1) rest)
    (apply define-command reverse-name (command-body -1) rest)
    (unless (assq forward-name cycle-commands)
      (setq cycle-commands (cons (cons forward-name reverse-name)
				 cycle-commands))))

;;; commands

  (define-cycle-command 'cycle-windows 'cycle-windows-backwards (lambda () t))

  (define-cycle-command 'cycle-group 'cycle-group-backwards
			(lambda (w) (windows-in-group w))
			#:spec "%W")

  (define-cycle-command 'cycle-prefix 'cycle-prefix-backwards
			(lambda (w)
			  (when (string-match "^([^:]+)\\s*:" (window-name w))
			    (let* ((prefix (expand-last-match "\\1"))
				   (re (concat ?^ (quote-regexp prefix) "\\s*:")))
			      (filter-windows
			       (lambda (x)
				 (string-match re (window-name x)))))))
			#:spec "%W")

  (define-cycle-command 'cycle-class 'cycle-class-backwards
			(lambda (w)
			  (let ((class (window-class w)))
			    (filter-windows
			     (lambda (x) (equal (window-class x) class)))))
			#:spec "%W"))

;;###autoload (autoload-command 'cycle-windows 'sawfish.wm.commands.x-cycle)
;;###autoload (autoload-command 'cycle-windows-backwards 'sawfish.wm.commands.x-cycle)
;;###autoload (autoload-command 'cycle-group 'sawfish.wm.commands.x-cycle)
;;###autoload (autoload-command 'cycle-group-backwards 'sawfish.wm.commands.x-cycle)
;;###autoload (autoload-command 'cycle-prefix 'sawfish.wm.commands.x-cycle)
;;###autoload (autoload-command 'cycle-prefix-backwards 'sawfish.wm.commands.x-cycle)
;;###autoload (autoload-command 'cycle-class 'sawfish.wm.commands.x-cycle)
;;###autoload (autoload-command 'cycle-class-backwards 'sawfish.wm.commands.x-cycle)
