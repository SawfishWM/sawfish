;; menus.jl -- popup menus
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

(define-structure sawfish.wm.menus

    (export menu-start-process
	    menu-stop-process
	    popup-menu
	    popup-window-menu
	    popup-root-menu
	    popup-apps-menu
	    custom-menu)

    (open rep
	  rep.data.tables
	  sawfish.wm.events
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.frames
	  sawfish.wm.commands
	  sawfish.wm.util.groups
	  sawfish.wm.workspace)

  (define-structure-alias menus sawfish.wm.menus)

  ;; Suppress annoying compiler warnings
  (eval-when-compile (require 'rep.io.timers))

  (defvar menu-program (expand-file-name "sawfish-menu" sawfish-exec-directory)
    "Location of the program implementing sawfish's menu interface.")

  (defvar menu-program-stays-running 60
    "When non-nil, the user-interface program is never stopped. If a number,
then this is taken as the number of seconds to let the process hang around
unused before killing it.")

  ;; the active user interface process
  (define menu-process nil)

  ;; output from the user-interface process that's received but not
  ;; yet processed
  (define menu-pending nil)

  ;; non-nil when we're waiting for a response from the ui process
  ;; if a window, then it's the window that received the event causing
  ;; the menu to be shown
  (define menu-active nil)

  ;; hash table mapping nicknames to result objects without read syntax
  (define nickname-table)
  (define nickname-index)

  ;; if menu-program-stays-running is a number, this may be a timer
  ;; waiting to kill the process
  (define menu-timer nil)

  (defvar window-ops-menu
    `((,(_ "_Iconify") iconify-window)
      (,(_ "_Delete") delete-window)
      (,(_ "Destroy") destroy-window)
      (,(_ "_Toggle") . window-ops-toggle-menu)
      (,(_ "_Maximize")
       (,(_ "_Vertically") maximize-window-vertically)
       (,(_ "_Horizontally") maximize-window-horizontally)
       (,(_ "_Both") maximize-window)
       ()
       (,(_ "Fill vertically") maximize-fill-window-vertically)
       (,(_ "Fill horizontally") maximize-fill-window-horizontally)
       (,(_ "Fill both") maximize-fill-window))
       (,(_ "_Un-maximize") unmaximize-window)
      (,(_ "In _group") . window-group-menu)
      (,(_ "_Send window to")
       (,(_ "_Previous workspace") send-to-previous-workspace)
       (,(_ "_Next workspace") send-to-next-workspace)
       (,(_ "Copy to previous") copy-to-previous-workspace)
       (,(_ "Copy to next") copy-to-next-workspace)
       ()
       (,(_ "_Left") move-window-left)
       (,(_ "_Right") move-window-right)
       (,(_ "_Up") move-window-up)
       (,(_ "_Down") move-window-down))
      (,(_ "Depth")
       (,(_ "Raise") raise-window)
       (,(_ "Lower") lower-window)
       (,(_ "Upper layer") raise-window-depth)
       (,(_ "Lower layer") lower-window-depth))
      (,(_ "Frame type")
       (,(_ "Normal") set-frame:default)
       (,(_ "Title-only") set-frame:shaped)
       (,(_ "Border-only") set-frame:transient)
       (,(_ "Top-border") set-frame:shaped-transient)
       (,(_ "None") set-frame:unframed))
      (,(_ "Frame style") . frame-style-menu)))

  (defvar window-ops-toggle-menu
    `((,(_ "_Sticky") toggle-window-sticky)
      (,(_ "S_haded") toggle-window-shaded)
      (,(_ "_Ignored") toggle-window-ignored)
      (,(_ "_Focusable") toggle-window-never-focus)
      ,@(when (featurep 'gnome)
	  `((,(_ "In _window list") gnome-toggle-skip-winlist)
	    (,(_ "In _task list") gnome-toggle-skip-tasklist)))))

  (defvar window-menu nil)

  (defvar root-menu
    `((,(_ "_Windows") . window-menu)
      (,(_ "Work_spaces") . workspace-menu)
      (,(_ "_Programs") . apps-menu)
      (,(_ "_Customize") . custom-menu)
      (,(_ "_Help")
       (,(_ "_FAQ") help:show-faq)
       (,(_ "_News") help:show-news)
       (,(_ "_WWW page") help:show-homepage)
       (,(_ "_Manual") help:show-programmer-manual))
      ()
      (,(_ "Restart") restart)
      (,(_ "Quit") quit)))

  (defvar apps-menu
    `(("xterm" (system "xterm &"))
      ("Emacs" (system "emacs &"))
      ("Netscape" (system "netscape &"))
      ("The GIMP" (system "gimp &"))
      ("XFIG" (system "xfig &"))
      ("GV" (system "gv &"))
      ("xcalc" (system "xcalc &"))))

  (define (menu-start-process)
    (when menu-timer
      (delete-timer menu-timer)
      (setq menu-timer nil))
    (unless (and menu-process (process-in-use-p menu-process))
      (when menu-process
	(kill-process menu-process))
      (let ((menu-sentinel (lambda (process)
			     (when (and menu-process
					(not (process-in-use-p menu-process)))
			       (setq menu-process nil))
			     (when menu-timer
			       (delete-timer menu-timer)
			       (setq menu-timer nil))))
	    (menu-filter (lambda (output)
			   (setq output (concat menu-pending output))
			   (setq menu-pending nil)
			   (condition-case nil
			       (let
				   ((result (read-from-string output)))
				 ;; GTK takes the focus for its menu,
				 ;; but later returns it to the original
				 ;; window. We want the focus to be
				 ;; restored by the time the menu-chosen
				 ;; command is invoked..
				 (accept-x-input)
				 (menu-dispatch result))
			     (end-of-stream
			      (setq menu-pending output))))))
	(setq menu-process (make-process menu-filter menu-sentinel)))
      (set-process-error-stream menu-process nil)
      (or (start-process menu-process menu-program)
	  (error "Can't start menu backend: %s" menu-program))))

  (define (menu-stop-process #!optional force)
    (when menu-process
      (cond ((and (not force) (numberp menu-program-stays-running))
	     ;; number of seconds to let it hang around for
	     (require 'rep.io.timers)
	     (setq menu-timer (make-timer (lambda ()
					    (kill-process menu-process)
					    (setq menu-process nil)
					    (setq menu-timer nil))
					  menu-program-stays-running)))
	    ((or force (not menu-program-stays-running))
	     (kill-process menu-process)
	     (setq menu-process nil)))))

  (define (make-nickname obj)
    (let ((nick nickname-index))
      (setq nickname-index (1+ nickname-index))
      (table-set nickname-table nick obj)
      nick))

  (define (nicknamep arg) (fixnump arg))
  (define (nickname-ref nick) (table-ref nickname-table nick))

  (define (menu-preprocessor cell)
    (when cell
      (let ((label (car cell)))
	(cond ((functionp (cdr cell))
	       (setq cell (funcall (cdr cell))))
	      ((and (symbolp (cdr cell)) (not (null (cdr cell))))
	       (setq cell (symbol-value (cdr cell)))
	       (when (functionp cell)
		 (setq cell (funcall cell))))
	      (t (setq cell (cdr cell))))
	(cond ((and (consp (car cell)) (stringp (car (car cell))))
	       ;; recurse through sub-menu
	       (setq cell (mapcar menu-preprocessor cell)))
	      ((and cell (not (symbolp (car cell))))
	       ;; a non-symbol result, replace by a nickname
	       (setq cell (cons (make-nickname (car cell)) (cdr cell)))))
	(cons label cell))))

  (define (menu-dispatch result)
    (let ((orig-win menu-active))
      (menu-stop-process)
      (when (nicknamep result)
	(setq result (nickname-ref result)))
      (setq menu-active nil)
      (setq nickname-table nil)
      (frame-draw-mutex nil)
      (when result
	(when (windowp orig-win)
	  (current-event-window orig-win))
	(cond ((commandp result)
	       (call-command result))
	      ((functionp result)
	       (result))
	      ((consp result)
	       (user-eval result))
	      (t result)))))

  (define (popup-menu spec)
    (or spec (error "No menu given to popup-menu"))
    (if (and menu-active menu-process (process-in-use-p menu-process))
	(error "Menu already active")
      (let* ((part (clicked-frame-part))
	     (offset (and part (frame-part-position part)))
	     (dims (and part (frame-part-dimensions part))))
	(setq menu-active (or (current-event-window) (input-focus)))
	(condition-case error-data
	    (progn
	      (menu-start-process)
	      ;; prevent any depressed button being redrawn until the menu
	      ;; is popped down
	      ;; XXX expose events screw this up..
	      (when (clicked-frame-part)
		(frame-draw-mutex t))
	      ;; This function is probably called from a ButtonPress event,
	      ;; so cancel the implicit pointer grab (to allow the menu's grab
	      ;; to succeed)
	      (ungrab-pointer)
	      (ungrab-keyboard)
	      (sync-server)
	      (when (functionp spec)
		(setq spec (spec)))
	      ;; XXX this is a hack, but I want menus to appear under buttons
	      (if (and part (setq part (frame-part-get part 'class))
		       (windowp menu-active)
		       (string-match "-button$" (symbol-name part)))
		  (progn
		    (rplaca offset
			    (max 0 (+ (car offset)
				      (car (window-position menu-active)))))
		    (rplacd offset
			    (max 0 (+ (cdr offset) (cdr dims)
				      (cdr (window-position menu-active))))))
		(setq offset nil))
	      (setq nickname-table (make-table eq-hash eq))
	      (setq nickname-index 0)
	      (format menu-process "(popup-menu %s %S %S)\n"
		      ;; write out the menu spec in one chunk to
		      ;; avoid large numbers of system calls :-[
		      (format nil "%S" (mapcar menu-preprocessor spec))
		      (x-server-timestamp) offset))
	      (error
	       ;; prevents spurious errors with subsequent menus
	       (setq menu-active nil)
	       (apply signal error-data))))))

  (define (popup-window-menu)
    "Display the menu listing all window operations."
    (popup-menu window-ops-menu))

  (define (popup-root-menu)
    "Display the main menu."
    (popup-menu root-menu))

  (define (popup-apps-menu)
    "Display the applications menu."
    (popup-menu apps-menu))

;;; customize menu

  (defvar custom-menu-includes-all-settings t
    "When non-nil, the custom menu includes the `All settings' item.")

  (define (custom-menu)
    `(,@(and custom-menu-includes-all-settings
	     (list (list (_ "All settings") 'customize) nil))
      ,@(mapcar (lambda (sub)
		  (list (_ (cadr sub))
			(intern (concat "customize:"
					(symbol-name (car sub))))))
		(filter consp (cddr custom-groups)))
      ,@(and (frame-style-editable-p default-frame-style)
	     (list nil `(,(_"Edit theme...") edit-frame-style)))))

  ;;###autoload
  (define-command 'popup-window-menu popup-window-menu)
  (define-command 'popup-root-menu popup-root-menu)
  (define-command 'popup-apps-menu popup-apps-menu))
