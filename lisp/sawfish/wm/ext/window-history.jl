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

;; an alist mapping encoded WM_CLASS text property to state alist
(define window-history-state t)

;; non-nil when window-history-state contains unsaved changes
(define window-history-dirty nil)

;; list of states in window-state-change-hook that should be tracked
(define window-history-states '(iconified sticky ignored never-focus
				shaded type frame-style))

;; property matched on
(define window-history-key-property 'WM_CLASS)


;; customizations

(defgroup window-history "Window History" :group workspace)

(defcustom window-history-save-position t
  "Remember window positions."
  :group (workspace window-history)
  :type boolean
  :require window-history)

(defcustom window-history-save-dimensions t
  "Remember window sizes."
  :group (workspace window-history)
  :type boolean
  :require window-history)

(defcustom window-history-save-state t
  "Remember other window attributes."
  :group (workspace window-history)
  :type boolean
  :require window-history)


;; matching windows

(define (window-history-key w)
  (nth 2 (get-x-property w window-history-key-property)))

(define (window-history-find w)
  (let ((class (window-history-key w)))
    (window-history-load)
    (assoc class window-history-state)))

(define (window-history-match w)
  (let ((alist (cdr (window-history-find w))))
    (when alist
      (window-history-apply w alist))))


;; recording attributes

(define (window-history-snapshotter w state)
  (unless (window-get w 'no-history)
    (let* ((alist (window-history-find w))
	   (value (if (get state 'window-history-snapshotter)
		      ((get state 'window-history-snapshotter) w)
		    (window-get w state)))
	   (cell (assq state (cdr alist))))
      (unless alist
	(setq alist (list (window-history-key w)))
	(setq window-history-state (cons alist window-history-state)))
      (cond ((and value cell)
	     (rplacd cell value))
	    (value
	     (rplacd alist (cons (cons state value) (cdr alist))))
	    (cell
	     (rplacd alist (delq cell (cdr alist)))))
      (setq window-history-dirty t))))

(put 'position 'window-history-snapshotter window-absolute-position)
(put 'dimensions 'window-history-snapshotter window-dimensions)

(define (window-history-position-snapshotter w)
  (when window-history-save-position
    (window-history-snapshotter w 'position)))

(define (window-history-dimensions-snapshotter w)
  (when window-history-save-dimensions
    (window-history-snapshotter w 'dimensions)))

(define (window-history-state-snapshotter w states)
  (when (and window-history-save-state states)
    (when (memq (car states) window-history-states)
      (window-history-snapshotter w (car states)))
    (window-history-state-snapshotter w (cdr states))))

(defun window-history-forget (w)
  "Forget any persistent state associated with the current window."
  (interactive "%W")
  (let
      ((alist (window-history-find w)))
    (when alist
      (setq window-history-state (delq alist window-history-state))
      (setq window-history-dirty t))))


;; restoring attributes

(define (window-history-apply w alist)
  ;; handle the `position' attribute specially
  (let ((position (cdr (assq 'position alist))))
    (when position
      ;; we don't want to place two windows of the same class 
      ;; at the same position, that's just pointless
      (let ((key (window-history-key w))
	    (space (or (cdr (assq 'workspace alist)) current-workspace)))
	(catch 'out
	  (map-windows
	   (lambda (x)
	     (when (and (equal (window-history-key x) key)
			(window-appears-in-workspace-p x space)
			(equal position (window-absolute-position x)))
	       ;; here's our match..
	       (setq alist (filter (lambda (cell)
				     (not (eq (car cell) 'position)))
				   alist))
	       (throw 'out t))))))))

  ;; the session manager code will do the right thing for the rest
  (require 'sm-load)
  (sm-apply-to-window w alist))


;; saving and loading state

(define (window-history-load)
  (unless (listp window-history-state)
    (setq window-history-state nil)
    (setq window-history-dirty nil)
    (when (file-exists-p window-history-file)
      (let
	  ((file (open-file window-history-file 'read)))
	(when file
	  (unwind-protect
	      (condition-case nil
		  (while t
		    (setq window-history-state (cons (read file)
						     window-history-state)))
		(end-of-stream
		 (setq window-history-state (nreverse window-history-state))))
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
	      (mapc (lambda (x)
		      (sm-print-alist file x)) window-history-state)
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
			     (list `(,(_ "_Forget state")
				     window-history-forget))))

(if (featurep 'match-window)
    (setq match-window-properties (nconc match-window-properties
					 (list '(no-history boolean))))
  (eval-after-load "match-window"
    '(setq match-window-properties (nconc match-window-properties
					  (list '(no-history boolean))))))
