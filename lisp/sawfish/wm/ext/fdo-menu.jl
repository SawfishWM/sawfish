;-*-sawfish-*-
;; (fdo-menu.jl (v0.6.1) --- sawfish wm menu generation -- librep)

;; (c) 2009 Matthew Love
;; Christopher Bratusek

;; This file will be part of Sawfish

;;; Description:
;;
;; Create a sawfish wm menu from .desktop files
;; in your /usr/share/applications folder.

#| 

Usage:

Make sure the mk-saw-menu.jl is in your load path
(i.e. ~/.sawfish/lisp), then in your .sawfish[/]rc file add:

;; Optional Part, config bits, defaults listed below
;; can be savely skipped, if you're fine with the defaults

;; change xterm -e to your appropriate string (must have
;; a space at the end) 

(setq my-term-string "xterm -e ") 

;; what locale for localized strings to use, if available
;; eg.: set to de to read Dokumentbetrachter instead of Documentviewer

(setq my-lang-string '())

;; if your .desktop files are located somewhere else than
;; /usr/share/applications, then change the desktop-directory

(setq desktop-directory '("/usr/share/applications"))

;; some entries are hidden from the menu, especially GNOME Apps
;; like eog, nautilus or evince & Co, if you want to have them
;; added to your menu, then replace '() by 't in the following

(setq ignore-no-display '())

;; if you don't want your menus to be sorted alphabetically
;; then replace 't by '() in the following

(setq want-alphabetize 't)

;; Necessary part, can't be skipped

;; load our script

(require 'fdo-menu)

;; generate and set the menu

(update-saw-menu)

|#

;;; TODO:

;; adhere to the desktop entry file specifications.
;; http://standards.freedesktop.org/desktop-entry-spec/latest/
;; add support for field codes: <a href="http://standards.freedesktop.org/desktop-entry-spec/latest/ar01s06.html"
;; add support for Comment=/Comment[lang]=
;; add support for Icon=

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

;  (defun update-vars
    ;; Some defaults
    ;;my-comm ()
    ;;my-icon ()
    (defvar my-name nil)
    (defvar my-disp nil)
    (defvar my-term nil)
    (defvar my-exec nil)
    (defvar my-cat nil)
    (defvar file-line nil)
    (defvar *loc-menu* nil)
    (make-variable-special 'apps-menu)
        
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

    ;; Variables that can be set in .sawfish[/]rc
    
    (if (not (boundp 'desktop-directory))
	(defvar desktop-directory '("/usr/share/applications")))

    (defvar desk-files (flatten (map-dir-files desktop-directory)))

    (defun find-lang-string ()
      (if (getenv "LANG")
	  (if (> (length (getenv "LANG")) 2)
	      (substring (getenv "LANG") 0 5)
	    (substring (getenv "LANG") 0 2))
	'()))

    (if (not (boundp 'my-lang-string))
	(defvar my-lang-string (find-lang-string)))
    
    (if my-lang-string
	(define name-string "Name[")
      (defvar name-string "Name="))
    
    (if (not (boundp 'ignore-no-display))
	(defvar ignore-no-display '()))
    
    (if (not (boundp 'want-alphabetize))
	(defvar want-alphabetize 't))
    
    (if (not (boundp 'my-term-string))
	(defvar my-term-string "xterm -e "))
    
    (if (not (boundp 'use-fdo-menu))
	(defvar use-fdo-menu 't))
;    )

    
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

    ;; Some file-verifiers
    (defun desktop-file-p (directory-file)
      (let ((this-file (open-file directory-file 'read)))
	(let ((this-line (read-line this-file)))
	  (if (string= this-line "[Desktop Entry]\012")
	      't
	    '()))))
    
    (defun backup-file-p (input-file-name)
      (if (or (string= "~" (substring input-file-name (- (length input-file-name) 1)))
	      (string= "#" (substring input-file-name 0 1)))
	  't
	'()))

    ;; Helper for (parse-desk-line) - specifically, for categories.
    (defun build-cat-list (line) ;; line must be excluding the \
      ;; categories= part -> (substring line 11)
      (if (> (length line) 1)
	  (let ((this-cat (prin1-to-string (read-from-string line))))
	    (cons this-cat (build-cat-list (substring line (+ 1 (length this-cat))))))))


    ;; Helper function for (fix-cats)
    (defun fix-sub-cats (cat-list loc-list)
      (if cat-list
	  (let ((cat-val (car cat-list)))
	    (if (assoc cat-val loc-list)
		(cons (cdr (assoc cat-val loc-list))
		      (fix-sub-cats cat-list (remove (assoc cat-val loc-list) loc-list)))
	      (fix-sub-cats (cdr cat-list) loc-list)))))


    ;; Second Part of process, after (parse-desktop-file) has run \
    ;; through and generated the *loc-menu*.
    ;; Will run through the Category alist and assign menu values accordingly, and \
    ;; will output the base of the menu, to \
    ;; be fed into (build-saw-menu)
    (defun fix-cats (cat-list)
      (if cat-list
	  (let ((cat-val (car (car cat-list)))
		(c-list (fix-sub-cats (car cat-list) *loc-menu*)))
	    (if (car c-list)
		(cons (cons cat-val c-list) (fix-cats (cdr cat-list)))
	      (fix-cats (cdr cat-list))))))

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
	      ;; to do specific things for the above entries (uncomment below, \
	      ;; and comment out the above (parse-cat-list (cdr cat-list)):
	      ;;(let ((dm-specific this-cat))
	      ;;  (if (or (string= dm-specific "Application")
	      ;;      (string= dm-specific "Qt")
	      ;;      (string= dm-specific "GTK"))
	      ;;  (parse-cat-list (cdr cat-list))
	      ;;"Settings"))
	      this-cat))
	(car cat-list)))


    ;; Helper function for (create-menu), which will parse the string input there.
    ;; Will only give an output for specified lines (i.e. category, name, etc.)
    (defun parse-desk-line (line desk-value)
      (let ((line-len (length line)))
	(cond

	 ;; this section is for the multi-lingual name string
	 ((and (> line-len 7) 
	       my-lang-string
	       (string= desk-value (substring line 0 5))
	       (string= (substring line 4 5) "[")
	       (or (string= my-lang-string (substring line 5 (+ (length my-lang-string) 5)))
		   (string= (substring my-lang-string 0 2) (substring line 5 (+ (length my-lang-string) 5)))))
	  (substring line (+ 4 (length my-lang-string) 3) (- line-len 1)))

	 ;; this section is for the exec and default name strings
	 ((and (> line-len 5)
	       (string= (substring desk-value 0 4) (substring line 0 4))
	       (string= (substring line 4 5) "="))
	  (if (string= (aref line (- line-len 3)) 37)
	      (substring line 5 (- line-len 4))
	    (substring line 5 (- line-len 1))))

	 ;; this section is for the category string
	 ((and (> line-len 10) (string= desk-value (substring line 0 11)))
	  (let ((cat-string (parse-cat-list (build-cat-list (substring line 11)))))
	    cat-string))

	 ;; this section is for the terminal string
	 ((and (> line-len 8) (string= desk-value (substring line 0 9)))
	  (substring line 9 (- line-len 1)))
	 
	 ;; this section is for the nodisplay string
	 ((and (> line-len 9) (string= desk-value (substring line 0 10)))
	  (substring line 10 (- line-len 1))))))

    ;; begining of parsing for (parse-desktop-file), feed a .desktop file line into this.
    ;; (create-menu "Name=Emacs ") ==> "Emacs"
    ;; (create-menu "Categories=Development;TextEditor;") ==> "Development"
    (defun create-menu (line)
      (cond
       ((parse-desk-line line "Categories=")
	(setq my-cat (parse-desk-line line "Categories=")))
       ((parse-desk-line line name-string)
	(setq my-name (parse-desk-line line name-string)))
       ;;((parse-desk-line line "Comment=")
       ;; (setq my-comm (parse-desk-line line "Comment=")))
       ;;((parse-desk-line line "Icon=")
       ;; (setq my-icon (parse-desk-line line "Icon=")))
       ((parse-desk-line line "Exec=")
	(setq my-exec (concat (parse-desk-line line "Exec=") " &")))
       ((parse-desk-line line "Terminal=")
	(setq my-term (parse-desk-line line "Terminal=")))
       ((parse-desk-line line "NoDisplay=")
	(setq my-disp (parse-desk-line line "NoDisplay=")))))


    ;; Parse a .desktop file into a list suitable for a menu
    ;; ex. (parse-desktop-file "emacs.desktop") ==> ("Development" "Emacs Text Editor" (system "emacs &"))

    (defun parse-desktop-file (desktop-file)
      (let ((desktop-file-name desktop-file))
	;;desktop-file-name
	(if (and (file-exists-p desktop-file-name)
		 (desktop-file-p desktop-file-name)
		 (not (file-directory-p desktop-file-name)))
	    ;;(not (backup-file-p desktop-file-name))
	    ;;(file-regular-p desktop-file-name))
	    (let ((new-file (open-file desktop-file-name 'read)))
	      (while (setq file-line (read-line new-file))
		(create-menu file-line))
	      (if ignore-no-display
		  (setq my-disp '()))
	      (if (not (string= my-disp "true"))
		  (if (not (string= (string-downcase my-term) "true"))
		      (cons my-cat (cons my-name (cons (list 'system my-exec) nil)))
		    ;;(cons my-cat (cons my-icon (cons my-name (cons my-comm (cons (list 'system my-exec) nil)))))
		    (cons my-cat (cons my-name (cons (list 'system (concat my-term-string my-exec)) nil))))
		;;(cons my-cat (cons my-icon (cons my-name (cons my-comm (cons (list 'system (concat my-term-string my-exec)) nil))))))
		(setq my-disp ()))))))

    ;; Format the menu for sawfish
    (defun build-saw-menu (entry)
      `(defvar saw-apps-menu ',entry))

    ;; Alphabetize the entries in the category menus
    (defun alphabetize-entries (saw-menu)
      (if saw-menu
	  (cons (cons (car (car saw-menu)) 
		      (sort (cdr (car saw-menu)) string<)) 
		(alphabetize-entries (cdr saw-menu)))))

    (define (update-saw-menu)
      (unless (not use-fdo-menu)
	(setq *loc-menu* nil)
	(setq desk-files (flatten (map-dir-files desktop-directory)))
	(mapc (lambda (x)
		(setq *loc-menu* (append *loc-menu* (list (parse-desktop-file x))))) desk-files)
	(if want-alphabetize
	    (setq apps-menu (alphabetize-entries (fix-cats menu-cat-alist)))
	  (setq apps-menu (fix-cats menu-cat-alist)))))
	
    (define-command 'update-saw-menu update-saw-menu)

))
