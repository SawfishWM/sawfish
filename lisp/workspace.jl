;; workspace.jl -- similar to virtual desktops
;; $Id$

(provide 'workspace)

(defvar ws-workspaces nil)

(defvar ws-current-workspace nil)

(defvar cycle-through-workspaces nil
  "When non-nil, moving through workspaces is cyclical, instead of stopping
when the first or last has been reached.")

(defvar delete-workspaces-when-empty t
  "When non-nil, workspaces are immediately deleted once they contain no
windows.")

(defvar uniconify-to-current-workspace t
  "When non-nil, windows that are uniconified appear on the current
workspace.")

(defvar static-workspace-menus
  '(("Next" next-workspace)
    ("Previous" previous-workspace)))


;; Low level functions

;; window shouldn't be in any workspace
(defun ws-add-window-to-space (w space)
  (rplacd space (nconc (cdr space) (list w)))
  (window-put w 'workspace space)
  (when (and ws-current-workspace
	     (eq space ws-current-workspace)
	     (not (window-get w 'iconified)))
    (show-window w)))

(defun ws-add-window (w)
  (unless (window-get w 'sticky)
    (if (null ws-current-workspace)
	(progn
	  ;; initialisation
	  (setq ws-current-workspace (list 'workspace w))
	  (setq ws-workspaces (list ws-current-workspace)))
      (rplacd ws-current-workspace
	      (nconc (delq w (cdr ws-current-workspace)) (list w))))
    (window-put w 'workspace ws-current-workspace)
    (unless (window-visible-p w)
      (show-window w))))

(defun ws-remove-window (w)
  (let
      ((space (window-get w 'workspace)))
    (when space
      (rplacd space (delq w (cdr space)))
      (when (and delete-workspaces-when-empty (null (cdr space)))
	;; workspace is now empty
	(when (eq ws-current-workspace space)
	  (ws-switch-workspace (or (nth 1 (memq space ws-workspaces))
				   (car ws-workspaces))))
	(setq ws-workspaces (delq space ws-workspaces)))
      (window-put w 'workspace nil)
      (when (windowp w)
	(hide-window w)))))

(defun ws-clean-lists ()
  (mapc #'(lambda (space)
	    (rplacd space (delete-if-not 'windowp (cdr space))))
	ws-workspaces))

(defun ws-add-workspace (at-end)
  (let
      ((space (list 'workspace)))
    (if at-end
	(setq ws-workspaces (nconc ws-workspaces (list space)))
      (setq ws-workspaces (cons space ws-workspaces)))
    (call-hook 'add-workspace-hook (list space))
    space))

(defun ws-find-next-workspace (&optional cycle)
  (when (cdr ws-workspaces)
    (let
	((tem (nth 1 (memq ws-current-workspace ws-workspaces))))
      (or tem (and cycle (car ws-workspaces))))))

(defun ws-find-previous-workspace (&optional cycle)
  (when (cdr ws-workspaces)
    (let
	((tem ws-workspaces))
      (while (and (cdr tem) (not (eq (nth 1 tem) ws-current-workspace)))
	(setq tem (cdr tem)))
      (and (or cycle (cdr tem))
	   (car tem)))))

(defun ws-switch-workspace (space)
  (unless (eq ws-current-workspace space)
    (when ws-current-workspace
      (mapc 'hide-window (cdr ws-current-workspace))
      (call-hook 'leave-workspace-hook (list ws-current-workspace)))
    (setq ws-current-workspace space)
    (when ws-current-workspace
      (mapc #'(lambda (w)
		(unless (window-get w 'iconified)
		  (show-window w))) (cdr ws-current-workspace))
      (call-hook 'enter-workspace-hook (list ws-current-workspace)))))


;; menu constructors

(defun workspace-menu ()
  (let
      ((tem ws-workspaces)
       (i 0)
       menu)
    (while tem
      (setq menu (cons (list (format nil "space %d%s"
				     (1+ i)
				     (if (eq (car tem) ws-current-workspace)
					 " *" ""))
			     `(lambda ()
				(ws-switch-workspace (nth ,i ws-workspaces))))
		       menu))
      (setq tem (cdr tem))
      (setq i (1+ i)))
    (nconc (nreverse menu) (list nil) static-workspace-menus)))

(defun window-menu ()
  (let
      (menu space win name)
    (setq space ws-workspaces)
    (while space
      (setq win (cdr (car space)))
      (while win
	(when (window-mapped-p (car win))
	  (setq name (window-name (car win)))
	  (setq menu (cons (list (concat
				  (and (window-get (car win) 'iconified) ?\[)
				  (if (> (length name) 20)
				      (concat (substring name 0 20) "...")
				    name)
				  (and (window-get (car win) 'iconified)  ?\])
				  (and (eq (input-focus) (car win)) " *"))
				 `(lambda ()
				    (display-window
				     (get-window-by-id
				      ,(window-id (car win))))))
			   menu)))
	(setq win (cdr win)))
      (setq space (cdr space))
      (when space
	(setq menu (cons nil menu))))
    (nreverse menu)))


;; Commands

(defun next-workspace ()
  "Display the next workspace."
  (interactive)
  (let
      ((space (ws-find-next-workspace cycle-through-workspaces)))
    (when space
      (ws-switch-workspace space))))

(defun send-to-next-workspace (window)
  "Move WINDOW to the next workspace. If no next workspace exists, one will be
created."
  (interactive "f")
  (let
      ((space (or (ws-find-next-workspace)
		  (ws-add-workspace t))))
    (ws-remove-window window)
    (ws-add-window-to-space window space)))

(defun previous-workspace ()
  "Display the previous workspace."
  (interactive)
  (let
      ((space (ws-find-previous-workspace cycle-through-workspaces)))
    (when space
      (ws-switch-workspace space))))

(defun send-to-previous-workspace (window)
  "Move WINDOW to the previous workspace. If no such workspace exists, one
will be created."
  (interactive "f")
  (let
      ((space (or (ws-find-previous-workspace)
		  (ws-add-workspace nil))))
    (ws-remove-window window)
    (ws-add-window-to-space window space)))


;; iconification (but without icons)

(defun iconify-window (w)
  (interactive "f")
  (unless (window-get w 'iconified)
    (window-put w 'iconified t)
    (when (window-visible-p w)
      (hide-window w))
    (call-hook 'iconify-window-hook (list w))))

(defun uniconify-window (w)
  (interactive "f")
  (when (window-get w 'iconified)
    (window-put w 'iconified nil)
    (cond ((eq (window-get w 'workspace) ws-current-workspace)
	   (show-window w))
	  (uniconify-to-current-workspace
	   (ws-remove-window w)
	   (ws-add-window w)))
    (call-hook 'uniconify-window-hook (list w))))

(defun display-window (w)
  "Display the workspace containing window W."
  (interactive "f")
  (when w
    (if (and (window-get w 'iconified) uniconify-to-current-workspace)
	(uniconify-window w)
      (let
	  ((space (window-get w 'workspace)))
	(when (and space (not (eq space ws-current-workspace)))
	  (ws-switch-workspace space))
	(uniconify-window w)
	(warp-cursor-to-window w)))))


;; initialisation

(unless (memq 'ws-add-window add-window-hook)
  (add-hook 'add-window-hook 'ws-add-window t)
  (add-hook 'destroy-notify-hook 'ws-remove-window t)
  (mapc 'ws-add-window (managed-windows)))
