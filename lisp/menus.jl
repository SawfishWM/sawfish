;; menus.jl -- popup menus
;; $Id$

(provide 'menus)

(defvar menu-program "sawmill-ui")
(defvar menu-program-stays-running nil)

(defvar menu-process nil)
(defvar menu-pending nil)
(defvar menu-active nil)

(defvar window-ops-menu
  '(("Move" move-window-interactively)
    ("Resize" resize-window-interactively)
    ("Raise" raise-window)
    ("Lower" lower-window)
    ("Iconify" iconify-window)
    ("Delete" delete-window)
    ("Destroy" destroy-window)
    ("Send left" send-to-previous-workspace)
    ("Send right" send-to-next-workspace)))

(defvar root-menu
  '(("Workspaces" . workspace-menu)
    ("Windows" . window-menu)
    (apps-menu)
    ()
    ("Restart" restart)
    ("Quit" quit)))

(defvar apps-menu
  '("Applications"
    ("xterm" (lambda () (system "xterm &")))
    ("Emacs" (lambda () (system "emacs &")))
    ("Netscape" (lambda () (system "netscape &")))
    ("The GIMP" (lambda () (system "gimp &")))
    ("XFIG" (lambda () (system "xfig &")))
    ("GV" (lambda () (system "gv &")))
    ("xcalc" (lambda () (system "xcalc &")))))

(defun menu-start-process ()
  (unless (and menu-process (process-in-use-p menu-process))
    (when menu-process
      (kill-process menu-process))
    (setq menu-process (make-process 'menu-filter 'menu-sentinel))
    (or (start-process menu-process menu-program)
	(error "Can't start menu backend: %s" menu-program))))

(defun menu-stop-process ()
  (when (and menu-process (not menu-program-stays-running))
    (kill-process menu-process)
    (setq menu-process nil)))

(defun menu-filter (output)
  (setq output (concat menu-pending output))
  (setq menu-pending nil)
  (condition-case nil
      (let
	  ((result (read-from-string output)))
	(throw 'menu-done result))
    (end-of-stream
     (setq menu-pending output))))

(defun menu-sentinel (process)
  (when (and menu-process (not (process-in-use-p menu-process)))
    (setq menu-process nil)))

(defun menu-preprocessor (cell)
  (when (and cell (symbolp (car cell)) (not (functionp cell)))
    (setq cell (symbol-value (car cell))))
  (when cell
    (let
	((label (car cell)))
      (if (functionp (cdr cell))
	  (setq cell (funcall (cdr cell)))
	(setq cell (cdr cell)))
      (when (and (consp (car cell)) (not (functionp (car cell))))
	(setq cell (mapcar 'menu-preprocessor cell)))
      (cons label cell))))

;;;###autoload
(defun popup-menu (spec)
  (if menu-active
      (error "Menu already active")
    (setq menu-active)
    (menu-start-process)
    ;; This function is probably called from a ButtonPress event,
    ;; so cancel the implicit pointer grab (to allow the menu's grab
    ;; to succeed)
    (ungrab-pointer)
    (sync-server)
    (unwind-protect
	(let
	    ((result (catch 'menu-done
		       (format menu-process "(popup-menu %S)\n"
			       (mapcar 'menu-preprocessor spec))
		       (while t
			 (accept-process-output 10)))))
	  (setq menu-active nil)
	  (when result
	    (cond ((commandp result)
		   (call-command result))
		  ((functionp result)
		   (funcall result))
		  (t
		   result))))
      (menu-stop-process))))

;;;###autoload
(defun popup-window-menu ()
  (interactive)
  (if window-ops-menu
      (popup-menu window-ops-menu)
    (beep)))

;;;###autoload
(defun popup-root-menu ()
  (interactive)
  (if root-menu
      (popup-menu root-menu)
    (beep)))
