;; window-history.jl -- store state across window instances
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(eval-when-compile (require 'match-window))

(require 'tables)
(provide 'window-history)

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


;; Configuration / variables

(defvar window-history-file "~/.sawfish/window-history"
  "Name of the file used to store persistent window state.")

;; a hash table mapping encoded WM_CLASS text property to state alist
(define window-history-state nil)

;; non-nil when window-history-state contains unsaved changes
(define window-history-dirty nil)

;; list of states in window-state-change-hook that should be tracked
(define window-history-states '(sticky ignored never-focus type frame-style))

;; property matched on
(define window-history-key-property 'WM_CLASS)

(define window-history-menu
  `((,(_ "Save _position") window-history-save-position)
    (,(_ "Save _dimensions") window-history-save-dimensions)
    (,(_ "Save _attributes") window-history-save-attributes)
    ()
    (,(_ "_Forget saved state") window-history-forget)))


;; customizations

(defgroup window-history "Window History" :group workspace)

(defcustom window-history-auto-save-position t
  "Automatically remember window positions."
  :group (workspace window-history)
  :type boolean
  :require window-history)

(defcustom window-history-auto-save-dimensions nil
  "Automatically remember window sizes."
  :group (workspace window-history)
  :type boolean
  :require window-history)

(defcustom window-history-auto-save-state nil
  "Automatically remember other window attributes."
  :group (workspace window-history)
  :type boolean
  :require window-history)


;; matching windows

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

  (if (window-transient-p w)
      (cons (nth 2 (get-x-property w window-history-key-property))
	    (window-name w))
    (nth 2 (get-x-property w window-history-key-property))))

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


;; recording attributes

(define (window-history-snapshotter w state)
  (unless (window-get w 'no-history)
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
(put 'type 'window-history-snapshotter (lambda (w)
					 ;; XXX hacky
					 (if (window-get w 'shaded)
					     (window-get w 'shaded-old-type)
					   (window-type w))))

(define (window-history-position-snapshotter w)
  (when (and window-history-auto-save-position
	     (not (window-get w 'fixed-position)))
    (window-history-snapshotter w 'position)))

(define (window-history-dimensions-snapshotter w)
  (when (and window-history-auto-save-dimensions
	     (not (window-get w 'fixed-position)))
    (window-history-snapshotter w 'dimensions)))

(define (window-history-state-snapshotter w states)
  (when (and window-history-auto-save-state states)
    (when (memq (car states) window-history-states)
      (window-history-snapshotter w (car states)))
    (window-history-state-snapshotter w (cdr states))))


;; manual state recording

(defun window-history-save-position (w)
  (interactive "%W")
  (window-history-snapshotter w 'position))

(defun window-history-save-dimensions (w)
  (interactive "%W")
  (window-history-snapshotter w 'dimensions))

(defun window-history-save-attributes (w)
  (interactive "%W")
  (mapc (lambda (state)
	  (window-history-snapshotter w state))
	window-history-states))

(defun window-history-forget (w)
  "Forget any persistent state associated with the current window."
  (interactive "%W")
  (window-history-set w nil))


;; restoring attributes

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

  ;; borrowed from sm-load.jl
  (let (tem)
    (when (setq tem (cdr (assq 'dimensions alist)))
      (resize-window-to w (car tem) (cdr tem)))
    (mapc (lambda (sym)
	    (when (setq tem (cdr (assq sym alist)))
	      (window-put w sym tem))) window-history-states)
    (call-window-hook 'sm-restore-window-hook w (list alist))))


;; saving and loading state

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
		      (table-set window-history-state (car cell) (cdr cell))))
		(end-of-stream))
	    (close-file file)))))))

(define (window-history-save)
  (when window-history-dirty
    (require 'sm-save)
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
			    (sm-print-alist file (cons key value)))
			  window-history-state)
	      (setq window-history-dirty nil))
	  (close-file file))))))


;; init

(add-hook 'before-add-window-hook window-history-match)
(add-hook 'after-move-hook window-history-position-snapshotter)
(add-hook 'after-resize-hook window-history-dimensions-snapshotter)
(add-hook 'window-state-change-hook window-history-state-snapshotter)
(add-hook 'before-exit-hook window-history-save)

(require 'menus)
(setq window-ops-menu (nconc window-ops-menu
			     (list `(,(_ "_History") . window-history-menu))))

(if (featurep 'match-window)
    (setq match-window-properties (nconc match-window-properties
					 (list '(no-history boolean))))
  (eval-after-load "match-window"
    '(setq match-window-properties (nconc match-window-properties
					  (list '(no-history boolean))))))
