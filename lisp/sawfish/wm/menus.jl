;; menus.jl -- popup menus

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

(define-structure sawfish.wm.menus

    (export menu-start-process
	    menu-stop-process
	    popup-menu
	    popup-window-menu
	    popup-root-menu
	    popup-apps-menu
	    add-window-menu-toggle
	    custom-menu)

    (open rep
	  rep.regexp
	  rep.io.files
	  rep.io.processes
	  rep.data.tables
	  sawfish.wm.events
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.frames
	  sawfish.wm.commands
	  sawfish.wm.util.groups
	  sawfish.wm.workspace
	  sawfish.wm.state.maximize
	  sawfish.wm.state.iconify
	  sawfish.wm.commands.launcher
	  sawfish.wm.ext.error-handler)

  (define-structure-alias menus sawfish.wm.menus)

  ;; Suppress annoying compiler warnings
  (eval-when-compile (require 'rep.io.timers))

  (defvar menus-include-shortcuts nil
    "Display key-binding information in menu items.")

  (defvar menu-program (expand-file-name "sawfish-menu" sawfish-exec-directory)
    "Location of the program implementing sawfish's menu interface.")

  (defvar menu-program-stays-running t
    "When non-nil, the menu program is never stopped. If a number, then this
is taken as the number of seconds to let the process hang around unused
before killing it.")

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
    `((,(_ "Mi_nimize") iconify-window
       (insensitive . ,(lambda (w)
                         (not (window-iconifiable-p w)))))
      (,(lambda (w)
          (if (window-maximized-p w)
              (_ "Unma_ximize")
            (_ "Ma_ximize"))) maximize-window-toggle
            (insensitive . ,(lambda (w)
                              (not (or (window-maximized-p w)
                                       (window-maximizable-p w))))))
      (,(_ "_Move") move-window-interactively)
      (,(_ "_Resize") resize-window-interactively)
      (,(_ "_Close") delete-window)
      ()
      (,(_ "_Toggle") . window-ops-toggle-menu)
      (,(_ "In _group") . window-group-menu)
      (,(_ "_Send window to")
       (,(_ "_Previous workspace") send-to-previous-workspace)
       (,(_ "_Next workspace") send-to-next-workspace))
      (,(_ "C_opy window to")
       (,(_ "P_revious workspace") copy-to-previous-workspace)
       (,(_ "Ne_xt workspace") copy-to-next-workspace))
      (,(_ "_Grow & Pack")
       (,(_ "Grow left") grow-window-left)
       (,(_ "Grow right") grow-window-right)
       (,(_ "Grow up") grow-window-up)
       (,(_ "Grow down") grow-window-down)
       ()
       (,(_ "Pack left") pack-window-left)
       (,(_ "Pack right") pack-window-right)
       (,(_ "Pack up") pack-window-up)
       (,(_ "Pack down") pack-window-down))
      (,(_ "Shrink & _Yank")
       (,(_ "Shrink left") shrink-window-left)
       (,(_ "Shrink right") shrink-window-right)
       (,(_ "Shrink up") shrink-window-up)
       (,(_ "Shrink down") shrink-window-down)
       ()
       (,(_ "Yank left") yank-window-left)
       (,(_ "Yank right") yank-window-right)
       (,(_ "Yank up") yank-window-up)
       (,(_ "Yank down") yank-window-down))
      (,(_ "Stac_king")
       (,(_ "_Raise") raise-window)
       (,(_ "_Lower") lower-window)
       ()
       (,(_ "_Upper layer") raise-window-depth)
       (,(_ "Lo_wer layer") lower-window-depth))
      (,(_ "Frame ty_pe") . frame-type-menu)
      (,(_ "Frame sty_le") . frame-style-menu)))

  (defvar window-ops-toggle-menu '())

  (defvar window-menu nil)

  (defvar root-menu
    `((,(_ "Sawfish Rootmenu"))
      ()
      (,(_ "_Windows") . window-menu)
      (,(_ "Work_spaces") . workspace-menu)
      ()
      (,(_ "_Programs") . apps-menu)
      (,(_ "_Customize") . custom-menu)
      ()
      (,(_ "Sessi_on")
       (,(_ "Display Errors") display-errors)
       (,(_ "Reload Appsmenu") update-apps-menu)
       ()
       (,(_ "Restart Sawfish") restart)
       (,(_ "Quit Sawfish") quit))
      ()
      (,(_ "_Help")
       (,(_ "_FAQ...") help:show-faq)
       (,(_ "_News...") help:show-news)
       (,(_ "_WWW page...") help:show-homepage)
       (,(_ "_Manual...") help:show-programmer-manual)
       ()
       (,(_ "_About Sawfish...") help:about))
      ()
      (,(_ "Kill Window") (system "xkill &"))))

  (defvar apps-menu)

  (define (menu-start-process)
    (when menu-timer
      (delete-timer menu-timer)
      (setq menu-timer nil))
    (unless (and menu-process (process-in-use-p menu-process))
      (when menu-process
	(kill-process menu-process)
	(setq menu-process nil))
      (let ((menu-sentinel (lambda ()
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
					    (when menu-process
					      (kill-process menu-process)
					      (setq menu-process nil))
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

  (define menu-args (make-fluid '()))
  (define where-is-fun (make-fluid '()))

  (define (menu-preprocessor cell)
    (define (inner cell)
      (when cell
	(let ((label (car cell)))
	  (when (functionp label)
	    (setq label (apply label (fluid menu-args))))
	  (cond ((functionp (cdr cell))
		 (setq cell (apply (cdr cell) (fluid menu-args))))
		((and (symbolp (cdr cell)) (not (null (cdr cell))))
		 (setq cell (symbol-value (cdr cell)))
		 (when (functionp cell)
		   (setq cell (apply cell (fluid menu-args)))))
		(t (setq cell (cdr cell))))
	  (when cell
	    (if (and (consp (car cell)) (stringp (car (car cell))))
		;; recurse through sub-menu
		(setq cell (mapcar inner cell))
	      (let* ((action (car cell))
		     (options (cdr cell))
		     (shortcut (and (fluid where-is-fun)
				    (symbolp action)
				    ((fluid where-is-fun) action))))
		(when (not (symbolp action))
		  ;; a non-symbol result, replace by a nickname
		  (setq action (make-nickname (car cell))))
		;; scan the alist of options
		(setq options (mapcar
			       (lambda (cell)
				 (if (functionp (cdr cell))
				     (cons (car cell)
					   (apply (cdr cell)
						  (fluid menu-args)))
				   cell)) options))
		(when shortcut
		  (setq options (cons (cons 'shortcut shortcut) options)))
		(setq cell (cons action options)))))
	  (cons label cell))))
    (let-fluids ((where-is-fun (and menus-include-shortcuts
				    (require 'sawfish.wm.util.keymap)
				    (make-memoizing-where-is
				     (list global-keymap window-keymap)))))
       (inner cell)))

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
	      (sync-server t)
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

  (define (popup-window-menu w)
    "Display the menu listing all window operations."
    (let-fluids ((menu-args (list w)))
       (popup-menu window-ops-menu)))

  (define (popup-root-menu)
    "Display the main menu."
    (popup-menu root-menu))

  (define (popup-apps-menu)
    "Display the applications menu."
    (popup-menu apps-menu))

  ;;###autoload
  (define-command 'popup-window-menu popup-window-menu #:spec "%W")
  (define-command 'popup-root-menu popup-root-menu)
  (define-command 'popup-apps-menu popup-apps-menu)

;;; menu modifiers

  (define (add-window-menu-toggle label command #!optional predicate)
    (let ((item (list* label command
		       (and predicate (list (cons 'check predicate))))))
      (let loop ((rest window-ops-toggle-menu))
         (cond
          ((null rest)
           (setq window-ops-toggle-menu (nconc window-ops-toggle-menu
                                               (list item))))
          ((eq (cadar rest) command)
           (rplaca rest item))
          (t (loop (cdr rest)))))))

;;; customize menu

  (defvar custom-menu-includes-all-settings t
    "When non-nil, the custom menu includes the `All settings' item.")

  (define (custom-menu)
    `(,@(and custom-menu-includes-all-settings
	     (list (list (_ "S_awfishConfig") 'customize) nil))
      ,@(mapcar (lambda (sub)
		  (list (_ (cadr sub))
			(intern (concat "customize:"
					(symbol-name (car sub))))))
		(filter consp (cddr custom-groups)))
      ,@(and (frame-style-editable-p default-frame-style)
	     (list nil `(,(_ "Edit theme...") edit-frame-style))))))
