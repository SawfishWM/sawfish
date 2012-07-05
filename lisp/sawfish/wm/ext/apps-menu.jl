;; apps-menu.jl -- generate applications menu from *.desktop files

;; (c) 2009 - 2011 Matthew Love

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

;;; Description:
;;
;; Generate applications menu from *.desktop files in the directory
;; /usr/share/applications .

;; "Desktop entry specification", *.desktop files spec, is defined in:
;; http://standards.freedesktop.org/desktop-entry-spec/latest/

;; 'fdo' in some names stands for "freedesktop.org".

;;; Todo:

;;; Notes: we don't handle non-utf8 encoding.

;;; Code:

(define-structure sawfish.wm.ext.apps-menu

    (export generate-apps-menu
	    init-apps-menu
	    update-apps-menu
	    parse-fdo-file
	    fdo-filter-record
	    fdo-toplevel-filter
	    fdo-nodisplay-filter
	    fdo-hidden-filter
	    fdo-onlyshowin-filter
	    fdo-notshowin-filter
	    fdo-default-filter
	    fdo-some-filter)

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

  (defvar apps-menu-filter 'default
    "The filter to use while generating the `apps-menu'. The default filters
include `fdo-toplevel-filter' `fdo-nodisplay-filter' `fdo-hidden-filter'
`fdo-onlyshowin-filter' and `fdo-notshowin-filter'.  Can also be set with
'default or 'some, both of which are combinations of the default filters,
'default uses them all and 'some only uses `fdo-notshowin-filter' and
`fdo-onlyshowin-filter'. This can be set to 'nil or '() to perform no
filtering on the `apps-menu'.")

  (defvar apps-menu-associate-categories t
    "Associate desktop entry categories with the category-master-list")

  (defvar desktop-directory '("/usr/share/applications")
    "List of directories to look for *.desktop files.")

  (defvar apps-menu-alphabetize t
    "Sort the apps menu alphabetically.")

  (defvar apps-menu-lang nil
    "Human language for applications menu, in string. Default is set from locale.")

  ;; The Master Category List

  (defvar desktop-cat-alist
    '(("Top-Level" . ("Application" "Applications" "GNOME" "KDE" "X-Xfce-Toplevel"
		      "GTK" "Qt"))
      ("Desktop" . ("X-Desktop" "X-DesktopApplets" "X-DesktopCountry"))
      ("Office" . ("Office" "WordProcessor" "Presentation" "X-Document"
		   "TextEditor" "SpreadSheet" "Calculator" "X-Calculate"
		   "Chart" "FlowChart" "Finance" "Calendar" "ContactManagement"
		   "X-Personal" "X-PersonalUtility" "Dictionary"))
      ("Internet" . ("Telephony" "Network" "Dialup" "VideoConference"
		     "RemoteAccess" "News" "HamRadio" "FileTransfer"
		     "X-Internet" "P2P" "Email" "WebBrowser" "IRCClient"
		     "Chat" "InstantMessaging" "Chat" "WebDevelopment"))
      ("Games" . ("Game" "ActionGame" "AdventureGame" "ArcadeGame"
		  "BoardGame" "BlocksGame" "CardGame" "KidsGame"
		  "LogicGame" "RolePlaying"))
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
		    "Math" "Education" "Geography" "Simulation"))
      ("Development" . ("GUIDesigner" "IDE" "Profiling" "RevisionControl"
			"ProjectManagement" "Translation" "Java"
			"Development" "Documentation" "Editors"))
      ("Utility" . ("X-SystemMemory" "Utility" "X-SetupEntry"
		    "X-SetupUtility" "X-SystemMemory" "TextTools"
		    "TelephonyTools" "Accessibility" "Clock" "ConsoleOnly"))
      ("Filesystem" . ("X-FileSystemFind" "X-FileSystemUtility" "Archiving"
		       "FileManager" "X-FileSystemMount" "Compression"))
      ("System" . ("X-SystemSchedule" "System" "X-SystemMemory" "Emulator"
		   "TerminalEmulator" "Printing" "Monitor" "Security"))
      ("Settings" . ("Settings" "HardwareSettings" "PackageManager"
		     "X-GNOME-PersonalSettings" "DesktopSettings"))
      ("Exiles" . ("Exile"))))

  (define this-line nil)

  (define name-string "Name[")

  ;; fdo-file-parsing

  (define (fdo-skip-line-p instring)
    "Return `t' if `instring' should be skipped."
    (or (eq (aref instring 0) ?#)
	(eq (aref instring 0) ?\n)))

  (define (check-if-desktop-stream instream)
    "Check for the `[Desktop Entry]' line in `instream'"
    (let ((line (read-line instream)))
      (when line
	(if (string= line "[Desktop Entry]\n")
	    't
	  (when (fdo-skip-line-p line)
	    (check-if-desktop-stream instream))))))

  (define (desktop-file-p directory-file)
    "Quickly check if `directory-file' is a `*.desktop' file."
    (condition-case nil
	(let ((this-file (open-file directory-file 'read)))
	  (check-if-desktop-stream this-file))
      ;; unreadable -> return nil
      (file-error)))

  (define (get-key-value-pair instring)
    "Split a `*.desktop' file line into its key-value pair.
Returns (key . value)"
    ;; Sorry, \\s doesn't work. Why??
    (if (string-match "^([^ \t=]+)[ \t]*=[ \t]*([^\n]+)" instring)
	(cons (expand-last-match "\\1") (expand-last-match "\\2"))
      ;; Ususally, it doesn't reach here.
      (cons "" "")))

  (define (fdo-group-p instring)
    (eq (aref instring 0) ?\[))

  (define (get-fdo-group instring)
    (substring instring 1 (- (length instring) 2)))

  (define (parse-fdo-file-line infile)
    "Parse a `*.desktop' file list.
Returns (group1 (key1 . value1) ... group2 (keyA . valueA) ...)"
    (when (setq this-line (read-line infile))
      (if (not (fdo-skip-line-p this-line))
	  (cons
	   (if (fdo-group-p this-line)
	       (get-fdo-group this-line)
	     (get-key-value-pair this-line))
	   (parse-fdo-file-line infile))
	(parse-fdo-file-line infile))))

  (define (parse-fdo-file infile)
    "Parse a `*.desktop' file and return an alist."
    (when (desktop-file-p infile)
      (let ((d-file (open-file infile 'read)))
	(parse-fdo-file-line d-file))))

  ;; desktop-file mapping

  (define (map-desk-files in-desk-files in-directory #!optional (extension "."))
    "Given a list of filenames and a directory, will expand those
filenames to include the full path."
    (when in-desk-files
      (if (string-match extension (car in-desk-files))
	  (cons (expand-file-name (car in-desk-files) in-directory)
		(map-desk-files (cdr in-desk-files) in-directory extension))
	(map-desk-files (cdr in-desk-files) in-directory extension))))

  (define (map-dir-files directories #!optional (extension "."))
    "Given a list of directory paths, will return a list of
files in those direcories with their full pathnames.  Optionally
`extension' may be set to show only files that match the regexp."
    (when directories
      (if (file-directory-p (car directories))
	  (let ((desk0 (directory-files (car directories))))
	    (cons (map-desk-files desk0 (car directories) extension)
		  (map-dir-files (cdr directories) extension)))
	(map-dir-files (cdr directories) extension))))

  (define (flatten input)
    (cond ((null input) nil)
	  ((atom input) (list input))
	  (t (append (flatten (car input))
		     (flatten (cdr input))))))

  ;; language functions

  (defmacro simplify-mlang (mlang mlevel)
    `(and
      ,(cond
	((or (= 0 mlevel) (not mlevel))
	 `(or (string-looking-at "([a-z]*)(_?)([A-Z]*?)(@)([A-Z]*[a-z]*)?" ,mlang)
	      (string-looking-at "([a-z]*)(_..)|([a-z]*)?" ,mlang)
	      (string-looking-at "([a-z]*)?" ,mlang)))
	((= 1 mlevel)
	 `(string-looking-at "([a-z]*)(_?)([A-Z]*?)(@)([A-Z]*[a-z]*)?" ,mlang))
	((= 2 mlevel)
	 `(string-looking-at "([a-z]*)(_..)|([a-z]*)?" ,mlang))
	((= 3 mlevel)
	 `(string-looking-at "([a-z]*)?" ,mlang)))
      (expand-last-match "\&")))

  (define (find-lang-string)
    (let loop ((lang-vars '("LC_ALL" "LC_MESSAGES" "LANG")))
      (and lang-vars
	   (let ((mlang (getenv (car lang-vars))))
	     (if mlang (simplify-mlang mlang 0)
	       (loop (cdr lang-vars)))))))

  ;; Functions for categories

  (define (remove-duplicates input)
    "Remove duplicate entries from `input'"
    (do ((a '() (if (member (car input) a) a (cons (car input) a)))
	 (input input (cdr input)))
	((null input) (reverse a))))

  (define (merge-list input delimiter)
    "Merge a cons list `input' into a string separated by `delimiter'"
    (when input
      (concat (car input) delimiter
	      (merge-list (cdr input) delimiter))))

  (define (associate-categories fdol)
    "Associate the `Categories' value(s) with the category
master list, `desktop-cat-alist'.  Returns a modified desktop-file entry."
    (when fdol
      (let* ((these-categories
	      (delete "" (string-split ";" (cdr (assoc "Categories" fdol)))))
	     (category-list '()))
	(let loop ((this-category these-categories))
	  (if (null this-category)
	      (let ((cat-string (merge-list (remove-duplicates category-list) ";")))
		(rplacd (assoc "Categories" fdol)
			cat-string)
		fdol)
	    (progn (mapc (lambda (ent)
			   (if (member (car this-category) ent)
			       (setq category-list
				     (append category-list (list (car ent))))))
			 desktop-cat-alist)
		   (loop (cdr this-category))))))))

  (define (grab-category input cat)
    "Remove duplicate categories from a generated apps-menu list by
category name."
    (when input
      (let ((cat-list '()))
	(setq cat-list (append cat-list (list cat)))
	(let loop ((this-line input))
	  (if (not this-line) cat-list
	    (progn (if (string= (caar this-line) cat)
		       (setq cat-list (append cat-list (list (cdr (car this-line))))))
		   (loop (cdr this-line))))))))

  (define (make-category-list input)
    "Return a list of the categories to be used in the menu."
    (when input
      (cons (caar input)
	    (make-category-list (cdr input)))))

  (define (consolidate-menu input)
    "Reduce the menu down so that each menu entry is inside a
single category."
    (when input
      (let ((cat-list (remove-duplicates (make-category-list input)))
	    (out-menu nil))
	(mapc (lambda (x)
		(setq out-menu
		      (append out-menu
			      (list (remove-duplicates (grab-category input x))))))
	      cat-list)
	out-menu)))

  ;; In fact, %% means "escaped %". Let's forget :/
  (define (trim-percent string)
    "Cut the string before % sign if present."
    (if (string-match "%" string)
	(substring string 0 (match-start))
      string))

  (define (alphabetize-entries saw-menu)
    "Alphabetize the entries in the category menus."
    (if saw-menu
	(cons (cons (car (car saw-menu))
		    (sort (cdr (car saw-menu))
			  (lambda (a b)
			    (string< (string-downcase (car a)) (string-downcase (car b))))))
	      (alphabetize-entries (cdr saw-menu)))))

  (define (fdo-exile fdo-list)
    "Exile `fdo-list' -- i.e., mark it as an invalid or garbled
desktop file."
    (let ((exile-comment
	   (cons "fdo-Comment" "This .desktop file was exiled, use \
with caution, file may be corrupt.\n"))
	  (exile-cmd
	   (cons "Exec" "sawfish-client -c 'display-errors'\n")))
      ;; Set the fdo-Comment key, mentioning the exile.
      (setq fdo-list (append fdo-list (list exile-comment)))
      ;; Set the NoDisplay key to 'true'
      (if (assoc "NoDisplay" fdo-list)
	  (rplacd (assoc "NoDisplay" fdo-list) "true")
	(setq fdo-list (append fdo-list (cons (cons "NoDisplay" "true")))))
      ;; Set the Categories & Category keys to 'Exile'
      (if (assoc "Categories" fdo-list)
	  (rplacd (assoc "Categories" fdo-list) "Exile")
	(setq fdo-list (append fdo-list (cons (cons "Categories" "Exile")))))
      (if (assoc "Category" fdo-list)
	  (rplacd (assoc "Category" fdo-list) "Exile")
	(setq fdo-list (append fdo-list (cons (cons "Category" "Exile")))))
      ;; Set the Exec key if it does not exist
      (when (not (assoc "Exec" fdo-list))
	(setq fdo-list (append fdo-list (list exile-cmd))))
      ;; Set the Name key if it does not exist
      (when (and (not (assoc "Name" fdo-list))
		 (not (assoc (concat name-string apps-menu-lang "]") fdo-list)))
	(setq fdo-list (append fdo-list (cons (cons "Name" "Unknown")))))
      fdo-list))

  (define (fdo-check-exile fdo-list)
    "If `fdo-list' doesn't have a Categories, Exec, or Name field,
exile it."
    (when fdo-list
      (if (or (and (not (assoc "Categories" fdo-list))
		   (not (stringp (cdr (assoc "Categories" fdo-list)))))
	      (not (assoc "Exec" fdo-list))
	      (and (not (assoc "Name" fdo-list))
		   (not (assoc (concat name-string
				       apps-menu-lang "]")
			       fdo-list))))
	  (fdo-exile fdo-list)
	fdo-list)))

  (define (fdo-double-check-category fdo-list)
    "Make sure the Category key is present and correctly asigned."
    (when fdo-list
      (if (assoc "Category" fdo-list)
	  (if (or (not (stringp (cdr (assoc "Category" fdo-list))))
		  (equal "" (cdr (assoc "Category" fdo-list)))
		  (not (stringp (cdr (assoc "Category" fdo-list)))))
	      (rplacd (assoc "Category" fdo-list) "Exile"))
	(append fdo-list (cons (cons "Category" "Exile")))))
    fdo-list)

  (define (determine-desktop-name fdo-list)
    "Get the correct Name[*] entry based on language settings."
    (or (when apps-menu-lang
	  (let ((mlang-1 (concat name-string (simplify-mlang apps-menu-lang 1) "]"))
		(mlang-2 (concat name-string (simplify-mlang apps-menu-lang 2) "]"))
		(mlang-3 (concat name-string (simplify-mlang apps-menu-lang 3) "]")))
	    (or (cdr (assoc mlang-1 fdo-list))
		(cdr (assoc mlang-2 fdo-list))
		(cdr (assoc mlang-3 fdo-list)))))
	(cdr (assoc "Name" fdo-list))))

  (define (determine-desktop-exec fdo-list)
    "Determine the correct `(system exec)' function from the given fdo alist"
    (if (assoc "Terminal" fdo-list)
	(if (string-match "[Tt]" (cdr (assoc "Terminal" fdo-list)))
	    (list 'system
		  (concat xterm-program " -e "
			  (trim-percent (cdr (assoc "Exec" fdo-list)))
			  " &"))
	  (list 'system
		(concat (trim-percent (cdr (assoc "Exec" fdo-list)))
			" &")))
      (list 'system
	    (concat (trim-percent (cdr (assoc "Exec" fdo-list)))
		    " &"))))

  ;; Apps-Menu Filtering

  (define (fdo-nodisplay-filter fdol)
    "Return the desktop-file-list if NoDisplay is False, or if NoDisplay is
not present in the desktop-file-list"
    (if (assoc "NoDisplay" fdol)
	(if (string-match "[Ff]" (cdr (assoc "NoDisplay" fdol)))
	    fdol)
      fdol))

  (define (fdo-hidden-filter fdol)
    "Return the desktop-file-list if Hidden is False, or if Hidden is
not present in the desktop-file-list"
    (if (assoc "Hidden" fdol)
	(if (string-match "[Ff]" (string-downcase (cdr (assoc "OnlyShowIn" fdol))))
	    fdol)
      fdol))

  (define (fdo-onlyshowin-filter fdol)
    "Return the desktop-file-list if OnlyShowIn matches `desktop-environment',
or if OnlyShowIn is not present in the desktop-file-list"
    (if (assoc "OnlyShowIn" fdol)
	(if (string-match desktop-environment (string-downcase (cdr (assoc "OnlyShowIn" fdol))))
	    fdol)
      fdol))

  (define (fdo-notshowin-filter fdol)
    "Return the desktop-file-list if NotShowIn does not match `desktop-environment',
or if NotShowIn is not present in the desktop-file-list"
    (if (assoc "NotShowIn" fdol)
	(if (not (string-match desktop-environment (string-downcase (cdr (assoc "NotShowIn" fdol)))))
	    fdol)
      fdol))

  (define (fdo-associate-categories-filter fdol)
    "If `apps-menu-associate-categories' is true, filter the
desktop-entry through `fdo-associate-categories'."
    (when fdol
      (if apps-menu-associate-categories
	  (associate-categories fdol)
	fdol)))

  (define (fdo-toplevel-filter fdol)
    "Return the desktop-file-list if the `Category' is of the
Top-Level variety."
    (when fdol
      (if (not (equal "Top-Level" (cdr (assoc "Category" fdol))))
	  fdol)))

  (define (fdo-default-filter fdol)
    "The default fdo-filter, combines the above."
    (fdo-toplevel-filter
     (fdo-hidden-filter
      (fdo-notshowin-filter
       (fdo-onlyshowin-filter
	(fdo-nodisplay-filter fdol))))))

  (define (fdo-some-filter fdol)
    "The 'some fdo-filter, will only respect
the NotShowIn and OnlyShowIn keys."
    (fdo-toplevel-filter
     (fdo-notshowin-filter
      (fdo-onlyshowin-filter fdol))))

  (define (fdo-filter-record fdol filter)
"Let `filter' process `fdol', a desktop file entry, and return the result.
`filter' can be a function, or a symbol 'default or 'some. If it isn't set,
return `fdol' as-is."
    (if (not filter) fdol
      (condition-case nil
	  (let loop ((fdo-entry fdol))
	    (when (consp fdo-entry)
	      (cons
	       ;; Check if entry is valid
	       (fdo-double-check-category
		(fdo-check-exile
		 ((cond
		   ;; default filter is chosen
		   ((equal filter 'default)
		    fdo-default-filter)
		   ;; some flter is chosen
		   ((equal filter 'some)
		    fdo-some-filter)
		   ;; user filter is chosen
		   ((closurep filter)
		    filter))
		  (car fdo-entry))))
	       (loop (cdr fdo-entry)))))
	(error fdol))))

  (define (split-desktop-entry fdol)
    "Split a desktop entry into several entries, each containing one
of the categories of the original."
    (when fdol
      (let ((new-fdol fdol)
	    (category-string (cdr (assoc "Categories" fdol))))
	(when (stringp category-string)
	  (let loop ((categories
		      (delete "" (string-split ";" category-string))))
	    (when categories
	      (append (list
		       (append new-fdol (list (cons "Category" (car categories)))))
		      (loop (cdr categories)))))))))

  ;; Sawfish-menu generation

  (define (fdo-menu-entry fdol)
    "Return menu-entry list from a fdo-list."
    ;; Generate the menu-entry list
    (generate-menu-entry
     ;; Filter entry by pre-made or user function
     (delete nil
	     (fdo-filter-record
	      ;; Split the desktop-entry by category
	      (split-desktop-entry
	       ;; Rename 'Categories' key based on category-list
	       (fdo-associate-categories-filter
		;; Check if entry is valid
		(fdo-check-exile fdol)))
	      apps-menu-filter))))

  (define (generate-menu-entry fdo-list)
    "Generate a menu entry to run the program specified in the the
desktop file `desk-file'."
    (when (car fdo-list)
      (cons (list (cdr (assoc "Category" (car fdo-list)))
		  (determine-desktop-name (car fdo-list))
		  (determine-desktop-exec (car fdo-list)))
	    (generate-menu-entry (cdr fdo-list)))))

  (define (generate-apps-menu)
    "Returns the list of applications menu which can be used for `apps-menu'."
    (unless apps-menu-lang
      (setq apps-menu-lang (find-lang-string)))
    (let ((desk-files (flatten (map-dir-files desktop-directory ".desktop")))
	  (local-menu nil))
      (mapc
       (lambda (x)
	 (setq local-menu
	       (append local-menu
		       (fdo-menu-entry
			(parse-fdo-file x)))))
       desk-files)
      (if apps-menu-alphabetize
	  (alphabetize-entries (consolidate-menu (sort (delete nil local-menu) string<)))
	(consolidate-menu (sort (delete nil local-menu) string<)))))

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
