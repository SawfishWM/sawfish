;; gnome-menu.jl -- replace the apps-menu by the gnome menu tree
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

(provide 'gnome-menu)


;; variables

(defvar gnome-share-directory
  ;; search $PATH for a known GNOME binary..
  (catch 'out
    (let
	((path (getenv "PATH"))
	 (point 0)
	 end tem)
      (while (< point (length path))
	(setq end (if (string-match ":" path point)
		      (match-start)
		    (length path)))
	(setq tem (substring path point end))
	(when (file-exists-p (expand-file-name "gnome-session" tem))
	  (throw 'out (expand-file-name "../share/gnome" tem)))
	(setq point (1+ end))))
    nil))

(defvar gnome-menu-lang (let
			    ((lang (getenv "LANG")))
			  (when (string= lang "en")
			    (setq lang nil))
			  lang)
  "Language code used when constructing GNOME menus.")

(defvar gnome-menu-roots (list (expand-file-name
				"apps" gnome-share-directory)
			       "/etc/X11/applnk"	;on RedHat systems
			       "~/.gnome/apps")
  "List of directories to read GNOME menu entries from.")

;; previously read menus
(defvar gnome-menus nil)


;; code

(defun gnome-menu-read-desktop-entry (filename)
  (let
      ((file (condition-case nil
		 (open-file filename 'read)
	       (file-error nil)))
       (section nil)
       name exec terminal type
       line)
    (when file
      (unwind-protect
	  (while (setq line (read-line file))
	    (cond ((string-looking-at "\\[Desktop Entry\\]" line 0 t)
		   (setq section 'desktop-entry))
		  ((string-looking-at "\\s*$" line)
		   (setq section nil))
		  ((and (eq section 'desktop-entry)
			(string-looking-at "Name=(.*)\n" line 0 t))
		   (setq name (expand-last-match "\\1")))
		  ((and (eq section 'desktop-entry) gnome-menu-lang
			(string-looking-at
			 "Name\\[([a-z]+)\\]=(.*)\n" line 0 t)
			(string= gnome-menu-lang (expand-last-match "\\1")))
		   (setq name (expand-last-match "\\2")))
		  ((and (eq section 'desktop-entry)
			(string-looking-at "Exec=(.*)\n" line 0 t))
		   (setq exec (expand-last-match "\\1")))
		  ((and (eq section 'desktop-entry)
			(string-looking-at "Terminal=(.*)\n" line 0 t))
		   (setq terminal (expand-last-match "\\1"))
		   (setq terminal (not (string-match
					"^0|false$" terminal 0 t))))
		  ((and (eq section 'desktop-entry)
			(string-looking-at "Type=(.*)\n" line 0 t))
		   (setq type (expand-last-match "\\1")))))
	(close-file file))
      (cond ((and type (string-match "Directory" type 0 t))
	     `(,name ,@(gnome-menu-read-directory
			(file-name-directory filename))))
	    (exec
	     ;; create a menu item
	     `(,name (system ,(concat (if terminal
					  (concat "xterm -e " exec)
					exec) " &"))))))))

(defun gnome-menu-read-order (filename)
  (let
      ((file (condition-case nil
		 (open-file filename 'read)
	       (file-error nil))))
    (when file
      (unwind-protect
	  (let
	      (order tem)
	    (while (setq tem (read-line file))
	      (when (string-match "\\s+$" tem)
		(setq tem (substring tem 0 (match-start))))
	      (setq order (cons tem order)))
	    (nreverse order))
	(close-file file)))))

(defun gnome-menu-read-item (dirname file)
  (unless (= (aref file 0) ?.)
    (setq file (expand-file-name file dirname))
    (cond
     ((file-regular-p file)
      (gnome-menu-read-desktop-entry file))
     ((file-directory-p file)
      (if (file-exists-p (expand-file-name ".directory" file))
	  (gnome-menu-read-desktop-entry (expand-file-name ".directory" file))
	(cons (file-name-nondirectory file)
	      (gnome-menu-read-directory file)))))))

(defun gnome-menu-read-directory (dirname)
  (let
      ((order (and (file-exists-p (expand-file-name ".order" dirname))
		   (gnome-menu-read-order
		    (expand-file-name ".order" dirname))))
       menus item)
    (mapc #'(lambda (file)
	      (when (file-exists-p (expand-file-name dirname file))
		(when (setq item (gnome-menu-read-item dirname file))
		  (setq menus (cons item menus)))))
	  order)
    (mapc #'(lambda (file)
	      (unless (or (= (aref file 0) ?.) (member file order))
		(when (setq item (gnome-menu-read-item dirname file))
		  (setq menus (cons item menus)))))
	  (directory-files dirname))
    (nreverse menus)))

(defun gnome-menus-merge-dups (menus)
  (let
      (ptr inner-ptr item tem)
    (setq ptr menus)
    (while ptr
      (setq item (car ptr))
      (when (and item (consp (cdr item)) (not (functionp (cdr item))))
	(setq inner-ptr (cdr ptr))
	(while inner-ptr
	  (setq tem (car inner-ptr))
	  (setq inner-ptr (cdr inner-ptr))
	  (when (and tem (string= (car item) (car tem))
		     (consp (cdr tem)) (not (functionp (cdr tem))))
	    ;; we've found a later occurrence of this sub-menu
	    (setq menus (delq tem menus))
	    (nconc item (cdr tem)))))
      (setq ptr (cdr ptr)))
    ;; now we've uniqued the top-level, recurse through any sub-menus
    ;(setq ptr menus)
    (while ptr
      (setq item (car ptr))
      (setq ptr (cdr ptr))
      (when (and item (consp (cdr item)) (not (functionp (cdr item))))
	(rplacd item (gnome-menus-merge-dups (cdr item)))))
    menus))

(defun gnome-menus-update ()
  (interactive)
  (setq gnome-menus nil)
  (mapc #'(lambda (dir)
	    (when (and (stringp dir) (file-directory-p dir))
	      (setq gnome-menus (nconc gnome-menus
				       (gnome-menu-read-directory dir)))))
	gnome-menu-roots)
  (setq gnome-menus (gnome-menus-merge-dups gnome-menus))
  gnome-menus)

(defun gnome-menus ()
  (unless gnome-menus
    (gnome-menus-update))
  gnome-menus)


;; init

(unless (boundp 'apps-menu)
  (setq apps-menu (cons (_ "System Menus") 'gnome-menus)))
