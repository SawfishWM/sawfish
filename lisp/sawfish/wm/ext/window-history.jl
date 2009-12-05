;; window-history.jl -- store state across window instances

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

;; This provides a much-asked-for feature -- being able to remember
;; window attributes without using the match-windows feature. It works
;; a bit like session management, but across window instances, not
;; session instances.

;; I'm still not convinced that this is a stunningly great idea, hence
;; it's not enabled by default, do (require 'window-history) to load it.

;; It matches against WM_CLASS, and remembers window properties after
;; they're manually altered. E.g. move a window, then other windows of
;; that class/instance pair will subsequently appear at that position.
;; Similarly for window sizes and attributes (e.g. frame style/type)

(define-structure sawfish.wm.ext.window-history

    (export window-history-save-position
	    window-history-save-dimensions
	    window-history-save-attributes
	    window-history-forget
	    window-history-load
	    window-history-save
	    window-history-clear)

    (open rep
	  rep.system
	  rep.io.files
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.windows
	  sawfish.wm.viewport
	  sawfish.wm.workspace
	  rep.data.tables
	  sawfish.wm.menus
	  sawfish.wm.state.maximize
	  sawfish.wm.state.iconify
	  sawfish.wm.ext.match-window)

  (define-structure-alias window-history sawfish.wm.ext.window-history)

;;; Configuration / variables

  (defvar window-history-file "~/.sawfish/window-history"
    "Name of the file used to store persistent window state.")

  ;; a hash table mapping encoded WM_CLASS text property to state alist
  (define window-history-state nil)

  ;; non-nil when window-history-state contains unsaved changes
  (define window-history-dirty nil)

  ;; list of states in window-state-change-hook that should be tracked
  (defvar window-history-states '(sticky ignored never-focus type maximized
                                         frame-style cycle-skip
                                         window-list-skip))

  ;; property matched on
  (defvar window-history-key-property '(WM_CLASS WM_WINDOW_ROLE))

  (defvar window-history-menu
    `((,(_ "Remember _position") window-history-save-position)
      (,(_ "Remember _dimensions") window-history-save-dimensions)
      (,(_ "Remember _attributes") window-history-save-attributes)
      (,(_ "_Forget saved state") window-history-forget)
      ()
      (,(_ "_Clear window-istory") window-history-clear)
      (,(_ "_Save window-history") window-history-save)
      (,(_ "_Reload window-history") window-history-load)))

;;; customizations

  (defgroup window-history "History" :group match-window)

  (defcustom window-history-auto-save-position t
    "Automatically remember window positions."
    :group (match-window window-history)
    :type boolean
    :require sawfish.wm.ext.window-history)

  (defcustom window-history-auto-save-dimensions nil
    "Automatically remember window sizes."
    :group (match-window window-history)
    :type boolean
    :require sawfish.wm.ext.window-history)

  (defcustom window-history-auto-save-state nil
    "Automatically remember other window attributes."
    :group (match-window window-history)
    :type boolean
    :require sawfish.wm.ext.window-history)

  (defcustom window-history-ignore-transients t
    "Don't automatically remember details of transient windows."
    :group (match-window window-history)
    :type boolean)

;;; matching windows

  (define (window-history-key w)

    ;; This function returns the key used to match windows, it can be any
    ;; lisp object with a read syntax (preferably one that hashes well),
    ;; if `(equal (window-history-key X) (window-history-key Y))' then
    ;; the two windows are assumed to be the `same' (whatever that means)

    ;; It has been suggested that WM_NAME should be factored in to this
    ;; somehow, but I want to avoid that, since windows I consider the
    ;; same often have different names (e.g. `Netscape: FOO' vs
    ;; `Netscape: BAR')

    ;; Instead I'm going to try just using this method for transient
    ;; windows, this should avoid the annoying cases where you save the
    ;; state of the app window, then a transient with the same class gets
    ;; given the same state..

    (define (get-prop p) (nth 2 (get-x-property w p)))

    (let ((props (append (if (listp window-history-key-property)
			     window-history-key-property
			   (list window-history-key-property))
			 (and (window-transient-p w)
			      (list 'WM_NAME)))))
      ;; This isn't the best method of creating keys, but it
      ;; preserves some backwards compatibility with old versions
      (mapconcat identity (delq nil (mapcar get-prop props)) "/")))

  (define (window-history-ref w)
    (let ((class (window-history-key w)))
      (when class
	(window-history-load)
	(table-ref window-history-state class))))

  (define (window-history-set w alist)
    (let ((class (window-history-key w)))
      (when class
	(window-history-load)
	(if alist
	    (table-set window-history-state class alist)
	  (table-unset window-history-state class))
	(setq window-history-dirty t))))

  (define (window-history-match w)
    (let ((alist (window-history-ref w)))
      (when alist
	(window-history-apply w alist))))

;;; recording attributes

  (define (window-history-snapshotter w state)
    (unless (or (window-get w 'no-history)
		(and window-history-ignore-transients (window-transient-p w)))
      (let* ((alist (window-history-ref w))
	     (value (if (get state 'window-history-snapshotter)
			((get state 'window-history-snapshotter) w)
		      (window-get w state)))
	     (cell (assq state alist)))
	(cond ((and value cell)
	       (rplacd cell value))
	      (value
	       (setq alist (cons (cons state value) alist)))
	      (cell
	       (setq alist (delq cell alist))))
	(window-history-set w alist))))

  (put 'position 'window-history-snapshotter window-absolute-position)
  (put 'dimensions 'window-history-snapshotter window-dimensions)

  (put 'maximized 'window-history-snapshotter
       (lambda (w)
	 (if (window-maximized-horizontally-p w)
	     (if (window-maximized-vertically-p w)
		 'both
	       'horizontal)
	   (if (window-maximized-vertically-p w)
	       'vertical
	     nil))))

  (put 'sticky 'window-history-snapshotter
       (lambda (w)
	 (if (window-sticky-p/viewport w)
	     (if (window-sticky-p/workspace w)
		 t
	       'viewport)
	   (if (window-sticky-p/workspace w)
	       'workspace
	     nil))))

  (define (window-history-position-snapshotter w)
    (when (and window-history-auto-save-position
	       (not (window-get w 'client-set-position)))
      (window-history-snapshotter w 'position)))

  (define (window-history-dimensions-snapshotter w)
    (when (and window-history-auto-save-dimensions
	       (not (window-get w 'client-set-position)))
      (window-history-snapshotter w 'dimensions)))

  (define (window-history-state-snapshotter w states)
    (when (and window-history-auto-save-state states)
      (when (memq (car states) window-history-states)
	(window-history-snapshotter w (car states)))
      (window-history-state-snapshotter w (cdr states))))

;;; manual state recording

  (defun window-history-save-position (w)
    "Remember the current position of the focused window."
    (window-history-snapshotter w 'position))

  (defun window-history-save-dimensions (w)
    "Remember the current dimensions of the focused window."
    (window-history-snapshotter w 'dimensions))

  (defun window-history-save-attributes (w)
    "Remember the current attributes of the focused window."
    (mapc (lambda (state)
	    (window-history-snapshotter w state))
	  window-history-states))

  (defun window-history-forget (w)
    "Forget any persistent state associated with the current window."
    (window-history-set w nil))

  (define-command 'window-history-save-position
    window-history-save-position #:spec "%W")
  (define-command 'window-history-save-dimensions
    window-history-save-dimensions #:spec "%W")
  (define-command 'window-history-save-attributes
    window-history-save-attributes #:spec "%W")
  (define-command 'window-history-forget
    window-history-forget #:spec "%W")

;;; restoring attributes

  (define (window-history-apply w alist)
    ;; handle the `position' attribute specially
    (let ((position (cdr (assq 'position alist))))
      (when position
	;; we don't want to place two windows of the same class
	;; at the same position, that's just pointless
	(when (or (window-get w 'placed)
		  (catch 'out
		    (let ((key (window-history-key w))
			  (space (or (cdr (assq 'workspace alist))
				     current-workspace)))
		      (when key
			(map-windows
			 (lambda (x)
			   (when (and (equal (window-history-key x) key)
				      (window-appears-in-workspace-p x space)
				      (equal position
					     (window-absolute-position x)))
			     ;; here's our match..
			     (throw 'out t))))))))
	  (setq alist (filter (lambda (cell)
				(not (eq (car cell) 'position)))
			      alist)))))

    ;; also do `dimensions'
    (let ((dims (cdr (assq 'dimensions alist))))
      (when (and dims (not (cdr (assq 'user-size (window-size-hints w)))))
	(resize-window-with-hints* w (car dims) (cdr dims))))

    ;; then the rest (borrowed from sm-load.jl)
    (mapc (lambda (sym)
	    (let ((tem (cdr (assq sym alist))))
	      (when tem
		(let ((setter (get sym 'window-history-setter)))
		  (if setter
		      (setter w tem)
		    (window-put w sym tem))))))
	  window-history-states)
    (call-window-hook 'sm-restore-window-hook w (list alist)))

  (put 'maximized 'window-history-setter
       (lambda (w value)
	 (when (memq value '(both vertical))
	   (window-put w 'queued-vertical-maximize t))
	 (when (memq value '(both horizontal))
	   (window-put w 'queued-horizontal-maximize t))))

  (put 'sticky 'window-history-setter
       (lambda (w value)
	 (when (memq value '(t both vertical))
	   (window-put w 'sticky-viewport t))
	 (when (memq value '(t both workspace))
	   (window-put w 'sticky t))))

;;; saving and loading state

  (define (window-history-load)
    (unless window-history-state
      (setq window-history-state (make-table equal-hash equal))
      (setq window-history-dirty nil)
      (when (file-exists-p window-history-file)
	(let
	    ((file (open-file window-history-file 'read)))
	  (when file
	    (unwind-protect
		(condition-case nil
		    (while t
		      (let ((cell (read file)))
			(table-set window-history-state
				   (car cell) (cdr cell))))
		  (end-of-stream))
	      (close-file file)))))))

  (define (window-history-save)
    (when window-history-dirty
      (unless (file-exists-p (file-name-directory window-history-file))
	(make-directory-recursively (file-name-directory window-history-file)))
      (let
	  ((file (open-file window-history-file 'write)))
	(when file
	  (unwind-protect
	      (progn
		(format file (concat ";; history saved for %s@%s\n"
				     ";; sawfish version %s; %s\n\n")
			(user-login-name) (system-name)
			sawfish-version (current-time-string))
		(table-walk (lambda (key value)
			      (print-alist file (cons key value)))
			    window-history-state)
		(setq window-history-dirty nil))
	    (close-file file))))))

  (define (window-history-clear)
    "Forget all saved window history."
    (setq window-history-state (make-table equal-hash equal))
    (setq window-history-dirty t))

  (define-command 'window-history-clear window-history-clear)

  (define (print-alist stream alist)
    (if (null alist)
	(write stream "()\n")
      (let (not-first)
	(mapc (lambda (x)
		(format stream "%s%S" (if not-first "\n " "\(") x)
		(setq not-first t))
	      alist)
	(write stream "\)\n\n"))))

;;; init

  (let ((tem (get-command-line-option "--window-history-file" t)))
    (when tem
      (setq window-history-file tem)))

  (add-hook 'before-add-window-hook window-history-match t)
  (add-hook 'after-move-hook window-history-position-snapshotter)
  (add-hook 'after-resize-hook window-history-dimensions-snapshotter)
  (add-hook 'window-state-change-hook window-history-state-snapshotter)
  (add-hook 'before-exit-hook window-history-save)

  (setq window-ops-menu
	(nconc window-ops-menu
	       (list `(,(_ "_History") . window-history-menu))))

  (define-match-window-property 'no-history 'other 'boolean))
