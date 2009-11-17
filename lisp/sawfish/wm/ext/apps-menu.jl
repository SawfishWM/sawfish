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
;; Generate applications menu from .desktop files in the directory
;; /usr/share/applications .

;; "Desktop entry specification", *.desktop files spec, is defined in:
;; http://standards.freedesktop.org/desktop-entry-spec/latest/

;; 'fdo' in some names stands for "freedesktop.org".

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
    (let ((this-file (open-file directory-file 'read)))
      (string= (read-line this-file) "[Desktop Entry]\n")))

  (define (desktop-group-p instring)
    (string= (substring instring 0 1) "["))

  (define (desktop-skip-line-p instring)
    (or (not instring)
	(string= (substring instring 0 1) "#")
	(string= (substring instring 0 1) "\n")))

  (define (get-key-break instring key)
    (if instring
	(let ((mlength (length instring)))
	  (do ((mcount 0 (1+ mcount)))
	      ((or (string= (substring instring mcount (+ mcount 1)) "\n")
		   (string= (substring instring mcount (+ mcount 1)) key)
		   (= mcount (- mlength 2))
		   (= mcount 398)) mcount)))))

  (define (get-desktop-key instring)
    (if (> (length instring) 3)
	(let ((break-number (get-key-break instring "=")))
	  (if (< break-number 20)
	      (substring instring 0 break-number)))))

  (define (get-desktop-value instring)
    (if (> (length instring) 3)
	(let ((break-number (get-key-break instring "=")))
	  (if (< break-number 20)
	      (substring instring (+ 1 break-number))))))

  (define (get-desktop-group instring)
    (substring instring 1 (- (length instring) 2)))

  (define (parse-desktop-file-line infile)
    (if (setq this-line (read-line infile))
	(if (not (desktop-skip-line-p this-line))
	    (cons
	     (if (desktop-group-p this-line)
		 (get-desktop-group this-line)
	       (if (not (desktop-group-p this-line))
		   (cons (get-desktop-key this-line)
			 (get-desktop-value this-line))))
	     (parse-desktop-file-line infile))
	  (parse-desktop-file-line infile))))

  (define (parse-desktop-file infile)
    (let ((d-file (open-file infile 'read)))
      (parse-desktop-file-line d-file)))

  ;; generic functions

  (define (map-desk-files in-desk-files in-directory)
    (if in-desk-files
	(cons (expand-file-name (car in-desk-files) in-directory)
	      (map-desk-files (cdr in-desk-files) in-directory))))

  (define (map-dir-files directories)
    (if directories
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

  (define (trim-end string)
    (cond
     ((string= (aref string (- (length string) 3)) 37)
      (substring string 0 (- (length string) 4)))
     (string
      (substring string 0 (- (length string) 1)))))

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

  (defvar menu-cat-alist
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
    (if cat-list
	(let ((cat-val (car cat-list)))
	  (if (assoc cat-val loc-list)
	      (cons (cdr (assoc cat-val loc-list))
		    (fix-sub-cats cat-list (remove (assoc cat-val loc-list)
						   loc-list)))
	    (fix-sub-cats (cdr cat-list) loc-list)))))

  ;; Associate values from the Master Category list with sub-categories
  ;; from file
  (define (fix-cats cat-list)
    (if cat-list
	(let ((cat-val (car (car cat-list)))
	      (c-list (fix-sub-cats (car cat-list) local-menu)))
	  (if (car c-list)
	      (cons (cons cat-val c-list) (fix-cats (cdr cat-list)))
	    (fix-cats (cdr cat-list))))))

  ;; Convert a Categories key value from ; delineated records to a
  ;; list
  (define (build-cat-list line)
    (if (> (length line) 1)
	(let ((this-cat (prin1-to-string (read-from-string line))))
	  (cons this-cat
		(if (< (length this-cat) (length line))
		    (build-cat-list
		     (substring line (+ 1 (length this-cat)))))))))

  ;; Helper for (parse-desk-line)
  ;; Determine best category to use... :|
  (define (parse-cat-list cat-list)
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
  (define (alphabetize-entries saw-menu)
    (if saw-menu
	(cons (cons (car (car saw-menu))
		    (sort (cdr (car saw-menu)) string<))
	      (alphabetize-entries (cdr saw-menu)))))

  (define (fdo-exile fdo-list)
    "Exile `fdo-list' -- i.e., mark it as an invalid or garbled
.desktop file."
    (let ((exile-comment
	   (cons "fdo-Comment" "This .desktop file was exiled, use \
with caution, file may be corrupt.\n"))
	  (exile-cmd
	   (cons "Exec" "sawfish-client -c 'display-errors'\n")))
      (setq fdo-list
	    (append fdo-list (list exile-comment)))
      (if (assoc "NoDisplay" fdo-list)
	  (rplacd (assoc "NoDisplay" fdo-list) "true\n")
	(setq fdo-list (append fdo-list (cons (cons "NoDisplay"
						    "true\n")))))
      (when (not (assoc "Exec" fdo-list))
	(setq fdo-list (append fdo-list (list exile-cmd))))
      (when (and (not (assoc "Name" fdo-list))
		 (not (assoc (concat name-string apps-menu-lang "]")
			     fdo-list)))
	(setq fdo-list (append fdo-list (cons (cons "Name"
						    "Unknown\n")))))
      (if (assoc "Categories" fdo-list)
	  (rplacd (assoc "Categories" fdo-list) "Exile\n")
	(setq fdo-list (append fdo-list (cons (cons "Categories"
						    "Exile\n")))))
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
.desktop file `desk-file'."
    (if (and (not (file-directory-p desk-file))
	     (desktop-file-p desk-file))
	(let ((fdo-list (fdo-check-exile (parse-desktop-file desk-file))))
	  (if apps-menu-ignore-no-display
	      (let ((a (assoc "NoDisplay" fdo-list)))
		(if a (rplacd a "false\n")
		  (setq fdo-list (cons (cons "NoDisplay" "false\n")
				       fdo-list)))))
	  (if (not (string= (cdr (assoc "NoDisplay" fdo-list)) "true\n"))
	      (list
	       (parse-cat-list (build-cat-list
				(trim-end (cdr (assoc "Categories"
						      fdo-list)))))
	       (trim-end (cdr (find-lang-in-desktop-file fdo-list)))
	       (if (string= (cdr (assoc "Terminal" fdo-list))
			    "true\n")
		   (list 'system
			 (concat xterm-program " -e "
				 (trim-end (cdr (assoc "Exec" fdo-list)))
				 " &"))
		 (list 'system
		       (concat (trim-end (cdr (assoc "Exec" fdo-list)))
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
	  (alphabetize-entries (fix-cats menu-cat-alist))
	(fix-cats menu-cat-alist))))

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
