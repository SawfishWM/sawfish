;; workspace.jl -- similar to virtual desktops

(provide 'workspace)

(defvar ws-workspaces nil)

(defvar ws-current-workspace nil)


;; Low level functions

;; window shouldn't be in any workspace
(defun ws-add-window-to-space (w space)
  (rplacd space (nconc (cdr space) (list w)))
  (window-put w 'workspace space)
  (when (and ws-current-workspace (eq space ws-current-workspace))
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
    (window-put w 'workspace ws-current-workspace)))

(defun ws-remove-window (w)
  (let
      ((space (window-get w 'workspace)))
    (when space
      (rplacd space (delq w (cdr space)))
      (when (null (cdr space))
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
  (interactive)
  (when (cdr ws-workspaces)
    (let
	((tem ws-workspaces))
      (while (and (cdr tem) (not (eq (nth 1 tem) ws-current-workspace)))
	(setq tem (cdr tem)))
      (and (or cycle (cdr tem))
	   (car tem)))))

(defun ws-switch-workspace (space)
  (when ws-current-workspace
    (mapc 'hide-window (cdr ws-current-workspace))
    (call-hook 'leave-workspace-hook (list ws-current-workspace)))
  (setq ws-current-workspace space)
  (when ws-current-workspace
    (mapc 'show-window (cdr ws-current-workspace))
    (call-hook 'enter-workspace-hook (list ws-current-workspace))))


;; Commands

(defun next-workspace ()
  (interactive)
  (let
      ((space (ws-find-next-workspace t)))
    (when space
      (ws-switch-workspace space))))

(defun send-to-next-workspace (window)
  (interactive "f")
  (let
      ((space (or (ws-find-next-workspace)
		  (ws-add-workspace t))))
    (ws-remove-window window)
    (ws-add-window-to-space window space)))

(defun previous-workspace ()
  (interactive)
  (let
      ((space (ws-find-previous-workspace t)))
    (when space
      (ws-switch-workspace space))))

(defun send-to-previous-workspace (window)
  (interactive "f")
  (let
      ((space (or (ws-find-previous-workspace)
		  (ws-add-workspace nil))))
    (ws-remove-window window)
    (ws-add-window-to-space window space)))


;; initialisation

(add-hook 'add-window-hook 'ws-add-window t)
(add-hook 'destroy-notify-hook 'ws-remove-window t)
(mapc 'ws-add-window (managed-windows))
