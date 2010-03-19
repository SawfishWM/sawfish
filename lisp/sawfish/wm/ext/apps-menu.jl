;; apps-menu.jl -- generate applications menu from *.desktop files

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
;; Generate applications menu from *.desktop files in the directory
;; /usr/share/applications .

;; "Desktop entry specification", *.desktop files spec, is defined in:
;; http://standards.freedesktop.org/desktop-entry-spec/latest/

;; 'fdo' in some names stands for "freedesktop.org".

;;; Todo:
;;  * Acquisition of the locale is wrong.

;;; Notes: we don't handle non-utf8 encoding.

;;; Code:

(define-structure sawfish.wm.ext.apps-menu

    (export generate-apps-menu
	    init-apps-menu
	    update-apps-menu)

    (open rep
	  rep.io.files
	  rep.io.streams
	  rep.system
	  rep.regexp
	  sawfish.wm
	  sawfish.wm.menus
	  sawfish.wm.commands
	  sawfish.wm.commands.launcher)

  (define-structure-alias apps-menu sawfish.wm.ext.apps-menu)

  ;; User Options
  (defvar apps-menu-autogen t
    "If non-nil, `apps-menu' is automatically generated from `user-apps-menu'
and *.desktop files. If you set `apps-menu', then it won't happen anyway.")

  (defvar user-apps-menu '()
    "Your own applications menu entries. It is followed by auto generated
applications menu.")

  (defvar apps-menu-ignore-no-display nil
    "Some entries are hidden from the menu, especially GNOME Apps like
eog, nautilus or evince. If you want to have them added to your menu,
set this to non-nil.")

  (defvar desktop-directory '("/usr/share/applications")
    "List of directories to look for *.desktop files.")

  (defvar apps-menu-alphabetize t
    "Sort the apps menu alphabetically.")

  (defvar apps-menu-lang nil
    "Language for applications menu, in string. Default is set from locale.")

  (define this-line nil)
  (define local-menu)
  (define name-string "Name[")

  ;; fdo-desktop-file-parsing

  (define (desktop-file-p directory-file)
    (condition-case nil
	(let ((this-file (open-file directory-file 'read)))
	  (string= (read-line this-file) "[Desktop Entry]\n"))
      ;; unreadable -> return nil
      (file-error)))

  (define (desktop-skip-line-p instring)
    (or (eq (aref instring 0) ?#)
	(eq (aref instring 0) ?\n)))

  (define (desktop-group-p instring)
    (eq (aref instring 0) ?\[))

  ;; returns (key . value)
  (define (get-key-value-pair instring)
    ;; Sorry, \\s doesn't work. Why??
    (if (string-match "^([^ \t=]+)[ \t]*=[ \t]*([^\n]+)" instring)
	(cons (expand-last-match "\\1") (expand-last-match "\\2"))
      ;; Ususally, it doesn't reach here.
      (cons "" "")))

  (define (get-desktop-group instring)
    (substring instring 1 (- (length instring) 2)))

  ;; Returns (group1 (key1 . value1) ... group2 (keyA . valueA) ...)
  (define (parse-desktop-file-line infile)
    (when (setq this-line (read-line infile))
      (if (not (desktop-skip-line-p this-line))
	  (cons
	   (if (desktop-group-p this-line)
	       (get-desktop-group this-line)
	     (get-key-value-pair this-line))
	   (parse-desktop-file-line infile))
	(parse-desktop-file-line infile))))

  (define (parse-desktop-file infile)
    (let ((d-file (open-file infile 'read)))
      (parse-desktop-file-line d-file)))

  ;; generic functions

  (define (map-desk-files in-desk-files in-directory)
    (when in-desk-files
      (cons (expand-file-name (car in-desk-files) in-directory)
	    (map-desk-files (cdr in-desk-files) in-directory))))

  (define (map-dir-files directories)
    (when directories
      (if (file-directory-p (car directories))
	  (let ((desk0 (directory-files (car directories))))
	    (cons (map-desk-files desk0 (car directories))
		  (map-dir-files (cdr directories))))
	(map-dir-files (cdr directories)))))

  (define (flatten input)
    (cond ((null input) nil)
	  ((atom input) (list input))
	  (t (append (flatten (car input))
		     (flatten (cdr input))))))

  ;; Cut the string before % sign if present.
  ;; In fact, %% means "escaped %". Let's forget :/
  (define (trim-percent string)
    (if (string-match "%" string)
	(substring string 0 (match-start))
      string))

  ;; This is wrong.  Read the desktop entry spec to see how it should
  ;; be done.  It's complicated.
  (define (find-lang-string)
    (define (simplify mlang)
      ;; N.B.: returns nil if mlang is "C" or "POSIX",
      ;; "fi" if it is "finnish", "sw" if it is "swedish"
      ;; Swedes can set locale to "sv_SE" or start learning Swahili.
      (and (string-looking-at "([a-z][a-z])(_..)?" mlang)
	   (expand-last-match "\\0")))
    (or
     (let loop ((lang-vars '("LC_ALL" "LC_MESSAGES" "LANG")))
	  (and lang-vars
	       (let ((mlang (getenv (car lang-vars))))
		 (if mlang (simplify mlang)
		   (loop (cdr lang-vars))))))
     ;; Kluge to keep braindead code from breaking.
     "xx"))

  ;; The Master Category List

  (defvar desktop-cat-alist
    '(("Desktop" . ("X-Desktop" "X-DesktopApplets" "X-DesktopCountry"
		    "DesktopSettings" "GNOME" "KDE"
		    "X-GNOME-PersonalSettings" "X-Xfce-Toplevel"))
      ("Personal" . ("X-Personal" "X-PersonalUtility" "Calendar"
		     "ContactManagement"))
      ("Office" . ("Office" "WordProcessor" "Presentation" "X-Document"
		   "TextEditor" "SpreadSheet" "Calculator" "X-Calculate"
		   "Chart" "FlowChart" "Finance"))
      ("Internet" . ("Telephony" "Network" "Dialup" "VideoConference"
		     "RemoteAccess" "News" "HamRadio" "FileTransfer"
		     "X-Internet" "P2P" "Email" "WebBrowser" "IRCClient"
		     "Chat" "InstantMessaging" "Chat" "WebDevelopment"))
      ("Games" . ("Game" "ActionGame" "AdventureGame" "ArcadeGame"
		  "BoardGame" "Emulator" "BlocksGame" "CardGame" "KidsGame"
		  "LogicGame" "RolePlaying" "Simulation"))
      ("Graphics" . ("RasterGraphics" "VectorGraphics" "X-GraphicUtility"
		     "2DGraphics" "3dGraphics" "3DGraphics" "Scanning"
		     "OCR" "Photography" "Viewer" "Publishing" "Art"
		     "ImageProcessing"))
      ("Media" . ("AudioVideo" "Audio", "Video" "Midi" "Mixer" "Sequencer"
		  "Tuner" "TV" "AudioVideoEditing" "Player" "Recorder"
		  "DiscBurning" "Music"))
      ("Science" . ("Science" "Astrology" "ArtificialIntelligence"
		    "Astronomy" "Biology" "Chemistry" "ComputerScience"
		    "DataVisualization" "Electricity" "Robotics" "Physics"
		    "Math" "Education" "Geography"))
      ("Development" . ("GUIDesigner" "IDE" "Profiling" "RevisionControl"
			"ProjectManagement" "Translation" "GTK"
			"Development" "Qt" "Documentation" "Editors"))
      ("Utility" . ("X-SystemMemory" "Security" "Utility" "X-SetupEntry"
		    "X-SetupUtility" "X-SystemMemory" "TextTools"
		    "TelephonyTools" "Accessibility" "Clock" "ConsoleOnly"))
      ("Filesystem" . ("X-FileSystemFind" "X-FileSystemUtility" "Archiving"
		       "FileManager" "X-FileSystemMount" "Compression"))
      ("System" . ("X-SystemSchedule" "System" "X-SystemMemory"
		   "TerminalEmulator" "Dictionary" "Puppy" "Printing"
		   "Monitor" "Security"))
      ("Settings" . ("Settings" "HardwareSettings" "PackageManager"))
      ("Exiles" . ("Exile"))))

  ;; Get the correct Name entry based on language settings
  ;; This is wrong.  Read the desktop entry spec to see how it should
  ;; be done.  It's complicated.
  (define (find-lang-in-desktop-file fdo-list)
    (or (and apps-menu-lang
	     (or (assoc (concat name-string apps-menu-lang "]")
			fdo-list)
		 (and (> (length apps-menu-lang) 2)
		      (assoc (concat name-string
				     (substring apps-menu-lang 0 2)
				     "]")
			     fdo-list))))
	(assoc "Name" fdo-list)))

  ;; Functions for categories
  (define (fix-sub-cats cat-list loc-list)
    (when cat-list
      (let ((cat-val (car cat-list)))
	(if (assoc cat-val loc-list)
	    (cons (cdr (assoc cat-val loc-list))
		  (fix-sub-cats cat-list (remove (assoc cat-val loc-list)
						 loc-list)))
	  (fix-sub-cats (cdr cat-list) loc-list)))))

  ;; Associate values from the Master Category list with sub-categories
  ;; from file
  (define (fix-cats cat-list)
    (when cat-list
      (let ((cat-val (car (car cat-list)))
	    (c-list (fix-sub-cats (car cat-list) local-menu)))
	(if (car c-list)
	    (cons (cons cat-val c-list) (fix-cats (cdr cat-list)))
	  (fix-cats (cdr cat-list))))))

  ;; Determine the best :| category to use. This will further be
  ;; converted with fix-cats.
  (define (determine-category line)
    (let loop ((cat-list (string-split ";" line))
	       this-cat)
	 (if (cdr cat-list)
	     (progn
	       (setq this-cat (car cat-list))
	       (if (or
		    (string= this-cat "GNOME")
		    (string= this-cat "GTK")
		    (string= this-cat "KDE")
		    (string= this-cat "Qt")
		    (string= this-cat "X-XFCE")
		    (string= this-cat "Application"))
		   (loop (cdr cat-list) nil)
		 this-cat))
	   (car cat-list))))

  ;; Alphabetize the entries in the category menus
  (define (alphabetize-entries saw-menu)
    (if saw-menu
	(cons (cons (car (car saw-menu))
		    (sort (cdr (car saw-menu)) string<))
	      (alphabetize-entries (cdr saw-menu)))))

  (define (fdo-exile fdo-list)
    "Exile `fdo-list' -- i.e., mark it as an invalid or garbled
desktop file."
    (let ((exile-comment
	   (cons "fdo-Comment" "This .desktop file was exiled, use \
with caution, file may be corrupt.\n"))
	  (exile-cmd
	   (cons "Exec" "sawfish-client -c 'display-errors'\n")))
      (setq fdo-list
	    (append fdo-list (list exile-comment)))
      (if (assoc "NoDisplay" fdo-list)
	  (rplacd (assoc "NoDisplay" fdo-list) "true")
	(setq fdo-list (append fdo-list (cons (cons "NoDisplay"
						    "true")))))
      (when (not (assoc "Exec" fdo-list))
	(setq fdo-list (append fdo-list (list exile-cmd))))
      (when (and (not (assoc "Name" fdo-list))
		 (not (assoc (concat name-string apps-menu-lang "]")
			     fdo-list)))
	(setq fdo-list (append fdo-list (cons (cons "Name"
						    "Unknown")))))
      (if (assoc "Categories" fdo-list)
	  (rplacd (assoc "Categories" fdo-list) "Exile")
	(setq fdo-list (append fdo-list (cons (cons "Categories"
						    "Exile")))))
      fdo-list))

  (define (fdo-check-exile fdo-list)
    "If `fdo-list' doesn't have a Categories, Exec, or Name field
exile it."
    (when fdo-list
      (if (or (not (assoc "Categories" fdo-list))
	      (not (assoc "Exec" fdo-list))
	      (and (not (assoc "Name" fdo-list))
		   (not (assoc (concat name-string
				       apps-menu-lang "]")
			       fdo-list))))
	  (fdo-exile fdo-list)
	fdo-list)))

  ;; generate a sawfish menu entry from a .desktop file
  (define (generate-menu-entry desk-file)
    "Generate a menu entry to run the program specified in the the
desktop file `desk-file'."
    (when (and (not (file-directory-p desk-file))
	       (desktop-file-p desk-file))
      (let ((fdo-list (fdo-check-exile (parse-desktop-file desk-file))))
	(if apps-menu-ignore-no-display
	    (let ((a (assoc "NoDisplay" fdo-list)))
	      (if a (rplacd a "false")
		(setq fdo-list (cons (cons "NoDisplay" "false")
				     fdo-list)))))
	(if (not (string= (cdr (assoc "NoDisplay" fdo-list)) "true"))
	    (list
	     (determine-category
	      (cdr (assoc "Categories" fdo-list)))
	     (cdr (find-lang-in-desktop-file fdo-list))
	     (if (string= (cdr (assoc "Terminal" fdo-list))
			  "true")
		 (list 'system
		       (concat xterm-program " -e "
			       (trim-percent (cdr (assoc "Exec" fdo-list)))
			       " &"))
	       (list 'system
		     (concat (trim-percent (cdr (assoc "Exec" fdo-list)))
			     " &"))))))))

  (define (generate-apps-menu)
    "Returns the list of applications menu which can be used for `apps-menu'."
    (unless apps-menu-lang
      (setq apps-menu-lang (find-lang-string)))
    (setq local-menu nil)
    (if (< (length apps-menu-lang) 2)
	(setq apps-menu-lang "xx"))
    (let ((desk-files (flatten (map-dir-files desktop-directory))))
      (mapc (lambda (x)
	      (setq local-menu
		    (append local-menu
			    (list (generate-menu-entry x))))) desk-files)
      (if apps-menu-alphabetize
	  (alphabetize-entries (fix-cats desktop-cat-alist))
	(fix-cats desktop-cat-alist))))

  (define (init-apps-menu)
    "If `apps-menu' is nil, then call `update-apps-menu'. This function
is intended to be called during Sawfish initialization."
    (unless apps-menu
      (update-apps-menu)))

  (define (update-apps-menu)
    "Set `apps-menu' to `user-apps-menu', and if `apps-menu-autogen' is non-nil,
append the auto generated one."
    (if apps-menu-autogen
	(setq apps-menu
	      (append user-apps-menu (generate-apps-menu)))
      (setq apps-menu user-apps-menu)))

  (define-command 'update-apps-menu update-apps-menu)
  )
