;-*-sawfish-*-
;; (fdo-menu.jl (v1.0.1) sawfish-wm-menu-generation librep)

;; (c) 2009 Matthew Love

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

;;; Description:
;;
;; Create a sawfish wm menu from .desktop files
;; in your /usr/share/applications folder.

;;; Code:

(define-structure sawfish.wm.ext.fdo-menu

    (export update-saw-menu)
    
    (open rep
	  rep.io.files
	  rep.io.streams
	  rep.system
	  sawfish.wm
	  sawfish.wm.commands)

  (define-structure-alias fdo-menu sawfish.wm.ext.fdo-menu)

  (unless batch-mode

    (defvar this-line nil)
    (defvar *loc-menu* nil)
    (make-variable-special 'apps-menu)

    ;; fdo-desktop-file-parsing
    
    (defun desktop-file-p (directory-file)
      (let ((this-file (open-file directory-file 'read)))
	(string= (read-line this-file) "[Desktop Entry]\012")))

    (defun desktop-group-p (instring)
      (string= (substring instring 0 1) "["))

    (defun desktop-skip-line-p (instring)
      (or (not instring)
	  (string= (substring instring 0 1) "#")
	  (string= (substring instring 0 1) "\012")))

    (defun get-key-break (instring key)
      (if instring
	  (do ((mcount 0 (1+ mcount))) 
	      ((or (string= (substring instring mcount (+ mcount 1)) "\n")
		   (string= (substring instring mcount (+ mcount 1)) key)
		   (= mcount 398)) mcount))))

    (defun get-desktop-key (instring)
      (if (> (length instring) 3)
	  (let ((break-number (get-key-break instring "=")))
	    (if (< break-number 20)
		(substring instring 0 break-number)))))

    (defun get-desktop-value (instring)
      (if (> (length instring) 3)
	  (let ((break-number (get-key-break instring "=")))
	    (if (< break-number 20)
		(substring instring (+ 1 break-number))))))

    (defun get-desktop-group (instring)
      (substring instring 1 (- (length instring) 2)))

    (defun parse-desktop-file-line (infile)
      (if (setq this-line (read-line infile))
	  (if (not (desktop-skip-line-p this-line))
	      (cons
	       (if (desktop-group-p this-line)
		   (get-desktop-group this-line)
		 (if (not (desktop-group-p this-line))
		     (cons (get-desktop-key this-line) (get-desktop-value this-line))))
	       (parse-desktop-file-line infile))
	    (parse-desktop-file-line infile))))

    (defun parse-desktop-file (infile)
      (unless (not (desktop-file-p infile))
	(let ((d-file (open-file infile 'read)))
	  (parse-desktop-file-line d-file))))

    ;; generic functions
    
    (defun map-desk-files (in-desk-files in-directory)
      (if in-desk-files
	  (cons (expand-file-name (car in-desk-files) in-directory)
		(map-desk-files (cdr in-desk-files) in-directory))))
    
    (defun map-dir-files (directories)
      (if directories
	  (if (file-directory-p (car directories))
	      (let ((desk0 (directory-files (car directories))))
		(cons (map-desk-files desk0 (car directories)) 
		      (map-dir-files (cdr directories))))
	    (map-dir-files (cdr directories)))))

    (defun flatten (input)
      (cond ((null input) nil)
	    ((atom input) (list input))
	    (t (append (flatten (car input))
		       (flatten (cdr input))))))
    
    (defun trim-end (string)
      (cond
       ((string= (aref string (- (length string) 3)) 37)
	(substring string 0 (- (length string) 4)))
       (string
	(substring string 0 (- (length string) 1)))))

    (defun find-lang-string ()
      (cond
       ((getenv "LANGUAGE")
	(let ((mlang (getenv "LANGUAGE")))
	  (if (> (length mlang) 2)
	      (substring mlang 0 5)
	    (substring mlang 0 2))))
       ((getenv "LC_ALL")
	(let ((mlang (getenv "LC_ALL")))
	  (if (> (length mlang) 2)
	      (substring mlang 0 5)
	    (substring mlang 0 2))))
       ((getenv "LC_MESSAGES")
	(let ((mlang (getenv "LC_MESSAGES")))
	  (if (> (length mlang) 2)
	      (substring mlang 0 5)
	    (substring mlang 0 2))))
       ((getenv "LANG")
	(let ((mlang (getenv "LANG")))
	  (if (> (length mlang) 2)
	      (substring mlang 0 5)
	    (substring mlang 0 2))))))

    ;; Variables that can be set in .sawfish[/]rc    
    
    (if (not (boundp 'desktop-directory))
	(defvar desktop-directory '("/usr/share/applications")))
    
    (if (not (boundp 'my-lang-string))
	(defvar my-lang-string (find-lang-string)))

    (if my-lang-string
	(defvar name-string "Name["))

    (if (not (boundp 'ignore-no-display))
	(defvar ignore-no-display '()))

    (if (not (boundp 'want-alphabetize))
	(defvar want-alphabetize 't))

    (if (not (boundp 'my-term-string))
	(defvar my-term-string "xterm -e "))

    (if (not (boundp 'use-fdo-menu))
	(defvar use-fdo-menu 't))

    ;; The Master Category List

    (defvar menu-cat-alist
      '(("Desktop" .  ("X-Desktop" "X-DesktopApplets" "X-DesktopCountry" \
		       "DesktopSettings" "GNOME" "KDE" "X-GNOME-PersonalSettings" \
		       "X-Xfce-Toplevel"))
	("Personal" . ("X-Personal" "X-PersonalUtility" "Calendar" "ContactManagement"))
	("Office" . ("Office" "WordProcessor" "Presentation" "X-Document" \
		     "TextEditor" "SpreadSheet" "Calculator" "X-Calculate" \
		     "Chart" "FlowChart" "Finance"))
	("Internet" . ("Telephony" "Network" "Dialup" "VideoConference" \
		       "RemoteAccess" "News" "HamRadio" "FileTransfer" \
		       "X-Internet" "P2P" "Email" "WebBrowser" "IRCClient" "Chat" \
		       "InstantMessaging" "Chat" "WebDevelopment"))
	("Games" . ("Game" "ActionGame" "AdventureGame" "ArcadeGame" "BoardGame" "Emulator"\
		    "BlocksGame" "CardGame" "KidsGame" "LogicGame" "RolePlaying" "Simulation"))
	("Graphics" . ("RasterGraphics" "VectorGraphics" "X-GraphicUtility" \
		       "2DGraphics" "3dGraphics" "3DGraphics" "Scanning" "OCR" "Photography" \
		       "Viewer" "Publishing" "Art" "ImageProcessing"))
	("Media" . ("AudioVideo" "Audio", "Video" "Midi" "Mixer" "Sequencer" "Tuner" \
		    "TV" "AudioVideoEditing" "Player" "Recorder" "DiscBurning" "Music"))
	("Science" . ("Science" "Astrology" "ArtificialIntelligence" "Astronomy" \
		      "Biology" "Chemistry" "ComputerScience" "DataVisualization" \
		      "Electricity" "Robotics" "Physics" "Math" "Education" "Geography"))
	("Development" . ("GUIDesigner" "IDE" "Profiling" "RevisionControl" \
			  "ProjectManagement" "Translation" "GTK" "Development" \
			  "Qt" "Development" "Documentation"))
	("Utility" . ("X-SystemMemory" "Security" "Utility" \
		      "X-SetupEntry" "X-SetupUtility" "X-SystemMemory" \
		      "TextTools" "TelephonyTools" "Accessibility" "Clock" \
		      "ConsoleOnly"))
	("Filesystem" .  ("X-FileSystemFind" "X-FileSystemUtility" "Archiving" \
			  "FileManager" "X-FileSystemMount" "Compression"))
	("System" . ("X-SystemSchedule" "System" "X-SystemMemory" \
		     "TerminalEmulator" "Dictionary" "Puppy" "Printing" "Monitor" "Security"))
	("Settings" . ("Settings" "HardwareSettings" "PackageManager"))))

    ;; Get the correct Name value based on language settings
    (defun find-lang-in-desktop-file (fdo-list)
      (if (assoc (concat name-string my-lang-string "]") fdo-list)
	  (concat name-string my-lang-string "]")
	(if (assoc (concat name-string (substring my-lang-string 0 2) "]") fdo-list)
	    (concat name-string (substring my-lang-string 0 2) "]")
	  "Name")))

    ;; Functions for categories
    (defun fix-sub-cats (cat-list loc-list)
      (if cat-list
	  (let ((cat-val (car cat-list)))
	    (if (assoc cat-val loc-list)
		(cons (cdr (assoc cat-val loc-list))
		      (fix-sub-cats cat-list (remove (assoc cat-val loc-list) loc-list)))
	      (fix-sub-cats (cdr cat-list) loc-list)))))

    ;; Associate values from the Master Category list with sub-categories from file
    (defun fix-cats (cat-list)
      (if cat-list
	  (let ((cat-val (car (car cat-list)))
		(c-list (fix-sub-cats (car cat-list) *loc-menu*)))
	    (if (car c-list)
		(cons (cons cat-val c-list) (fix-cats (cdr cat-list)))
	      (fix-cats (cdr cat-list))))))

    ;; Convert a Categories key value from ; delineated records to a list
    (defun build-cat-list (line) 
      (if (> (length line) 1)
	  (let ((this-cat (prin1-to-string (read-from-string line))))
	    (cons this-cat 
		  (if (< (length this-cat) (length line))
		      (build-cat-list (substring line (+ 1 (length this-cat)))))))))

    ;; Helper for (parse-desk-line)
    ;; Determine best category to use... :|
    (defun parse-cat-list (cat-list)
      (if (cdr cat-list)
	  (let ((this-cat (car cat-list)))
	    (if (or
		 (string= this-cat "GNOME")
		 (string= this-cat "GTK")
		 (string= this-cat "KDE")
		 (string= this-cat "Qt")
		 (string= this-cat "X-XFCE")
		 (string= this-cat "Application"))
		(parse-cat-list (cdr cat-list))
	      this-cat))
	(car cat-list)))

    ;; Alphabetize the entries in the category menus
    (defun alphabetize-entries (saw-menu)
      (if saw-menu
	  (cons (cons (car (car saw-menu)) 
		      (sort (cdr (car saw-menu)) string<)) 
		(alphabetize-entries (cdr saw-menu)))))

    ;; generate a saw-fish menu entry from a .desktop file
    (defun generate-menu-entry (desk-file)
      (if (and (not (file-directory-p desk-file))
	       (desktop-file-p desk-file))
	  (let ((fdo-list (parse-desktop-file desk-file)))
	    (if (not (string= (cdr (assoc "NoDisplay" fdo-list)) "true\n"))
		(cons (parse-cat-list (build-cat-list (trim-end (cdr (assoc "Categories" fdo-list)))))
		      (cons (trim-end (cdr (assoc (find-lang-in-desktop-file fdo-list) fdo-list)))
			    (if (string= (cdr (assoc "Terminal" fdo-list)) "true\012")
				(cons (list 
				       'system (concat my-term-string (trim-end (cdr (assoc "Exec" fdo-list))) " &")))
			      (cons (list 'system (concat (trim-end (cdr (assoc "Exec" fdo-list))) " &"))))))))))

    ;; Update the menu
    (define (update-saw-menu)
      (unless (not use-fdo-menu)
	(setq *loc-menu* nil)
	(let ((desk-files (flatten (map-dir-files desktop-directory))))
	  (mapc (lambda (x)
		  (setq *loc-menu* (append *loc-menu* (list (generate-menu-entry x))))) desk-files)
	  (if want-alphabetize
	      (setq apps-menu (alphabetize-entries (fix-cats menu-cat-alist)))
	    (setq apps-menu (fix-cats menu-cat-alist))))))

    (define-command 'update-saw-menu update-saw-menu)
))    
    ;; END-FILE

