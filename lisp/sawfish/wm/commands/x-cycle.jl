;; x-cycle.jl -- stack-based window cycling

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

    (export define-cycle-command
	    define-cycle-command-pair)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.timers
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.util.window-order
	  sawfish.wm.util.keymap
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.focus
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.util.stacking
	  sawfish.wm.events
	  sawfish.wm.util.decode-events
	  sawfish.wm.util.groups
	  sawfish.wm.util.rects
	  sawfish.wm.util.display-window)

  (define-structure-alias x-cycle sawfish.wm.commands.x-cycle)

  ;; we bind to disable-auto-raise and tooltips-enabled
  (eval-when-compile (require 'sawfish.wm.ext.auto-raise))
  (eval-when-compile (require 'sawfish.wm.ext.tooltips))

;;; customization options

  ;;###autoload (defgroup cycle "Window Cycling" :group focus :require sawfish.wm.commands.x-cycle)
  (defgroup cycle "Window Cycling"
    :group focus
    :require sawfish.wm.commands.x-cycle)

  (defcustom cycle-show-window-names t
    "Display window names and icons while cycling through windows."
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
    :type boolean
    :group (focus cycle))

  (defcustom cycle-keymap (make-keymap)
    "Keymap containing bindings active only during window cycling operations."
    :group bindings
    :type keymap)

  (defvar after-cycle-step-hook '()
    "Window hook called after each step of window cycling.")

  ;; variables

  ;; the current window
  (define x-cycle-current (make-fluid))

  ;; the original stacking order
  (define x-cycle-stacking (make-fluid))

  ;; the list of windows being cycled through
  (define x-cycle-windows (make-fluid))

  ;; is this call during a cycle operation
  (define x-cycle-active (make-fluid))

  ;; list of all cycle command names
  (define cycle-commands '())

;;; code

  (define (forwards lst elt count)
    (let ((total (length lst))
	  (current (let loop ((rest lst) (i 0))
                        (cond ((null rest) 0)
                              ((eq (car rest) elt) i)
                              (t (loop (cdr rest) (1+ i)))))))
      (nth (mod (+ current count) total) lst)))

  (define (cycle-display-message)
    (require 'sawfish.wm.util.display-wininfo)
    (display-wininfo (fluid x-cycle-current)))

  (define (remove-message)
    (require 'sawfish.wm.util.display-wininfo)
    (display-wininfo nil))

  (define (cycle-next windows count)
    (fluid-set x-cycle-windows windows)
    (let ((win (window-order (if cycle-all-workspaces
                                 nil
                               current-workspace)
                             cycle-include-iconified cycle-all-viewports)))
      (setq win (delete-if (lambda (w)
                             (not (memq w windows))) win))
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
        (warp-pointer-if-necessary win)
        (raise-window* win))
      (when cycle-show-window-names
        (cycle-display-message))
      (when (window-really-wants-input-p win)
        (set-input-focus win))
      (allow-events 'sync-keyboard)
      (call-window-hook 'after-cycle-step-hook win)))

  (define (cycle-begin windows step)
    "Cycle through all windows in order of recent selections."
    (let ((tail-command nil)
	  (grab-win (input-focus)))
      (let-fluids ((x-cycle-current nil)
		   (x-cycle-stacking nil)
		   (x-cycle-windows windows)
		   (x-cycle-active t))

        (define (unmap-fun w)
          (when (eq w grab-win)
            (setq grab-win nil)
            (or (grab-keyboard nil nil t)
                (throw 'x-cycle-exit nil))
            (allow-events 'sync-keyboard)))

        (define (enter-fun space)
          (declare (unused space))
          (when grab-win
            (setq grab-win nil)
            (or (grab-keyboard nil nil t)
                (throw 'x-cycle-exit nil))
            (allow-events 'sync-keyboard)))

        (define (unbound-fun)
          (let* ((event (current-event))
                 (ev (decode-event event)))
            (cond
             ((memq 'release (nth 1 ev)) ; key released
              (when (and (modifier-keysym-p (nth 2 ev)) ; modifier released
                         (eq 2 (length (nth 1 ev))))    ; 'release + 1 modifier
                (throw 'x-cycle-exit t)))
             ((not (modifier-keysym-p (nth 2 ev)))
              ;; real key pressed
              (let* ((override-keymap cycle-keymap)
                     (command (lookup-event-binding event)))
                (unless command
                  ;; search cycle-keymap then the usual ones
                  (setq override-keymap nil)
                  (setq command (lookup-event-binding event)))
                (if (memq command cycle-commands)
                    ;; call without aborting cycle operation
                    (progn
                      (current-event-window (fluid x-cycle-current))
                      (call-command command))
                  (unless (setq tail-command command)
                    ;; no wm binding, so forward the event to
                    ;; the focused window (this is why we have
                    ;; to grab the keyboard synchronously)
                    (allow-events 'replay-keyboard))
                  (throw 'x-cycle-exit nil)))))))

        (let* ((decoded (decode-event (current-event)))
               (eval-modifier-events t)
               (eval-key-release-events t)
               (override-keymap (make-keymap))
               (focus-dont-push t)
               (disable-auto-raise t)
               (tooltips-enabled nil)
               (unmap-notify-hook (cons unmap-fun unmap-notify-hook))
               (enter-workspace-hook (cons enter-fun enter-workspace-hook))
               (unbound-key-hook (list unbound-fun)))

          (unless (and (eq 'key (car decoded)) (nth 1 decoded))
            (error "%s must be bound to a key event with modifiers."
                   this-command))

          ;; grab synchronously, so that event replaying works
          (when (grab-keyboard grab-win nil t)
            (unwind-protect
                (progn
                  (catch 'x-cycle-exit
                    ;; do the first step
                    (cycle-next windows step)
                    (setq focus-ignore-pointer-events t)
                    (recursive-edit))
                  (when (fluid x-cycle-current)
                    (display-window (fluid x-cycle-current))))
              (remove-message)
              (ungrab-keyboard)
              (make-timer (lambda ()
                            (setq focus-ignore-pointer-events nil))
                          0 100)))))

      (when tail-command
	;; make sure that the command operates on the newly-focused
	;; window, not the window that was focused when the original
	;; event was received
	(current-event-window (input-focus))
	(call-command tail-command))))

  (define (define-cycle-command name body . rest)
    "Create a command that will not cause the current cycle operation
to abort before execution.

All arguments are passed to define-command."
    (unless (memq name cycle-commands)
      (setq cycle-commands (cons name cycle-commands)))
    (apply define-command name body rest))

  (define (define-cycle-command-pair forward-name reverse-name selector . rest)
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
	    (if (fluid x-cycle-active)
		(cycle-next windows step)
	      (cycle-begin windows step))))))
    (when forward-name
      (apply define-cycle-command forward-name (command-body +1) rest))
    (when reverse-name
      (apply define-cycle-command reverse-name (command-body -1) rest)))

;;; commands

  (define-cycle-command-pair
    'cycle-windows 'cycle-windows-backwards
    (lambda () (filter-windows window-in-cycle-p)))

  (define-cycle-command-pair
    'cycle-group 'cycle-group-backwards
    (lambda (w)
      (delete-if-not window-in-cycle-p (windows-in-group w)))
    #:spec "%W")

  (define-cycle-command-pair
    'cycle-among-groups 'cycle-among-groups-backwards
    (lambda ()
      "Only cycle the top members of each group."
      (delete-if-not window-in-cycle-p
                     (mapcar (lambda (gid) (car (windows-by-group gid t)))
                             (window-group-ids)))))

  (define-cycle-command-pair
    'cycle-prefix 'cycle-prefix-backwards
    (lambda (w)
      (when (string-match "^([^:]+)\\s*:" (window-name w))
        (let* ((prefix (expand-last-match "\\1"))
               (re (concat ?^ (quote-regexp prefix) "\\s*:")))
          (delete-if-not window-in-cycle-p
                         (filter-windows
                          (lambda (x)
                            (string-match re (window-name x))))))))
    #:spec "%W")

  (define-cycle-command-pair
    'cycle-class 'cycle-class-backwards
    (lambda (w)
      (let ((class (window-class w)))
        (delete-if-not window-in-cycle-p
                       (filter-windows
                        (lambda (x) (equal (window-class x) class))
                        (window-order)))))
    #:spec "%W")

  (define-cycle-command-pair
    'cycle-step 'cycle-step-backwards
    (lambda ()
      (if (fluid x-cycle-active)
          (fluid x-cycle-windows)
        (error "%s must be bound to a key event in the cycle keymap."
               this-command))))

  (define-cycle-command-pair
    'cycle-dock 'cycle-dock-backwards
    (lambda ()
      (delete-if-not (lambda (x) (window-in-cycle-p x #:ignore-cycle-skip t))
                     (filter-windows dock-window-p)))))

#| autoload cookies:

###autoload (autoload-command 'cycle-windows 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-windows-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-group 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-group-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-among-groups 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-among-groups-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-prefix 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-prefix-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-class 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-class-backwards 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-dock 'sawfish.wm.commands.x-cycle)
###autoload (autoload-command 'cycle-dock-backwards 'sawfish.wm.commands.x-cycle)

|#

#| doc strings for the cycle commands:

::doc:sawfish.wm.commands.x-cycle#cycle-windows::
Cycle through all windows in order of recent selections.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-windows-backwards::
Cycle through all windows in reverse order of recent selections.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-group::
Cycle through all windows in the same group as the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-group-backwards::
Reverse cycle through all windows in the same group as the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-among-groups::
Cycle through different groups (only the top window of each group).
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-among-groups-backwards::
Reverse cycle through different groups (only the top window of each group).
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-prefix::
Cycle through all windows whose names match the leading colon-delimited
prefix of the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-prefix-backwards::
Reverse cycle through all windows whose names match the leading
colon-delimited prefix of the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-class::
Cycle through all windows with the same class as the current window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-class-backwards::
Reverse cycle through all windows with the same class as the current
window.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-step::
Step one window forwards through the current window cycle list.
This command should only be used in the cycle keymap.
::end::

::doc:sawfish.wm.commands.x-cycle#cycle-step-backwards::
Step one window backwards through the current window cycle list.
This command should only be used in the cycle keymap.
::end::

|#
