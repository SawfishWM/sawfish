;; select-window.jl -- click on a window, any window..
;; $Id$

(defvar select-window-map (bind-keys (make-sparse-keymap)
			    "Any-Click1" 'select-window-finished))

;;;###autoload
(defun select-window ()
  (grab-pointer (or (input-focus) (car (managed-windows)))
		(get-cursor 'crosshair))
  (unwind-protect
      (progn
	(let
	    ((override-keymap select-window-map))
	  (catch 'select-window
	    (recursive-edit))))
    (ungrab-pointer)))

(defun select-window-finished ()
  (interactive)
  (throw 'select-window (query-pointer-window)))
