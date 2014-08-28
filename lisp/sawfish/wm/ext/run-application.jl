;; run-application.jl -- prompt for an application and run it

;; Copyright (C) 2008 Sergey I. Sharybin <sharybin@nm.ru>
;; Copyright (C) 2007 Sven Schoenung <sven.schoenung@gmail.com>
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; run-application is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; run-application is distributed in the hope that it will be useful, 
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Sawfish, the program this script is written for; see the
;; file COPYING.  If not, write to  the Free Software Foundation,
;; 675 Mass Ave, Cambridge, MA 02139, USA.

;; Version: 0.1
;;  - Initial release
;;
;; Version: 0.2
;;  - Custom path support
;;  - Application exclusion support
;;
;; Version 0.3
;;  - Added customization of font and colors
;;    NOTE: New version of prompt.jl is required

(declare (in-module sawfish.wm.util.prompt))

(require 'sawfish.wm.util.prompt)
(require 'sawfish.wm.commands)
(require 'sawfish.wm.colors)
(require 'rep.io.files)
(require 'rep.regexp)

;; FIXME: This could probably be better ...
(define (prompt-application-executable file dir)
   (let ((f (expand-file-name file dir)))
    (and (not (file-directory-p f))
	 (string-match "x" (file-modes-as-string f)))))

(define (prompt-application-duplicate file dirs)
  (delete-if-not (lambda (dir)
		   (file-exists-p (expand-file-name file dir)))
		 dirs))

(define (prompt-mismatch-application str dir file dirs)
  (or (not (string-head-eq file str))
      (prompt-application-duplicate file dirs)
      (not (prompt-application-executable file dir))
      (and run-application:use-application-exclude
	   (string-match run-application:application-exclude file))))

(define (prompt-application-path)
  (let ((path (getenv "PATH"))
        (path-list '())
        (dir "")
        (start 0)
        (end 0))
   (while (< start (length path))
     (setq end (if (string-match ":" path start)
		   (match-start) (length path)))
     (setq dir (substring path start end))
     (when (file-exists-p dir)
       (setq path-list (append path-list (list dir))))
     (setq start (1+ end)))
   path-list))

(define (prompt-complete-application str)
  (let ((path (prompt-application-path)))
    (apply #'nconc 
	   (mapcar (lambda (dir)
		     (let ((dirs (cdr (member dir path))))
		       (delete-if 
			 (lambda (file) 
			   (prompt-mismatch-application str dir file dirs))
			 (directory-files dir))))
		   path))))

(define (prompt-for-application #!optional title default)
  "Prompt for an application in $PATH"
  (unless (stringp title)
    (setq title "Enter application:"))
  (let ((str (prompt #:title title
		     #:completion-fun prompt-complete-application)))
    (when (and (string= str "") default)
      (setq str default))
    str))

(define (run-application)
  "Prompt for an application and run it"
  (setq prompt-window-position 
	(cons (case run-application:x-position
		((left) run-application:x-offset)
		((right) (- -1 run-application:x-offset))
		(t nil))
	      (case run-application:y-position
		((top) run-application:y-offset)
		((bottom) (- -1 run-application:y-offset))
		(t nil))))
  (system (format nil "%s &" (prompt-for-application "Run application: "))))

(define-command 'run-application run-application)
