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

(require 'sm-save)
(require 'sm-load)
(provide 'window-history)

;; Commentary:

;; This provides a much-asked-for feature -- being able to remember
;; window attributes without using the match-windows feature. It works
;; a bit like session management, but across window instances, not
;; session instances.

;; It's currently a bit flaky, hence it's not enabled by default. It
;; matches against WM_CLASS, and remembers window properties after
;; they're manually altered. E.g. move a window, then other windows of
;; that class/instance pair will subsequently appear at that position.


;; Configuration / variables

;; an alist mapping encoded WM_CLASS text property to state alist
(define window-history-state t)

(define window-history-file "~/.sawfish/window-history")


;; matching windows

(define (window-history-key w)
  (nth 2 (get-x-property w 'WM_CLASS)))

(define (window-history-find w)
  (let ((class (window-history-key w)))
    (window-history-load)
    (assoc class window-history-state)))

(define (window-history-match w)
  (let ((alist (cdr (window-history-find w))))
    (when alist
      (sm-apply-to-window w alist))))


;; recording attributes

(define (window-history-snapshotter state)
  (lambda (w)
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
	       (rplacd alist (delq cell (cdr alist)))))))))

(put 'position 'window-history-snapshotter window-absolute-position)
(put 'dimensions 'window-history-snapshotter window-dimensions)

(define (window-history-state-snapshotter w states)
  (when states
    ((window-history-snapshotter (car states)) w)
    (window-history-state-snapshotter w (cdr states))))


;; saving and loading state

(define (window-history-load)
  (unless (listp window-history-state)
    (setq window-history-state nil)
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
  (when (listp window-history-state)
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
		      (sm-print-alist file x)) window-history-state))
	  (close-file file))))))


;; init

(add-hook 'before-add-window-hook window-history-match)
(add-hook 'after-move-hook (window-history-snapshotter 'position))
(add-hook 'after-resize-hook (window-history-snapshotter 'dimensions))
(add-hook 'window-state-change-hook window-history-state-snapshotter)
(add-hook 'before-exit-hook window-history-save)
