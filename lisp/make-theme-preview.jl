;; make-theme-preview.jl --
;; $Id$

(require 'make-theme)
(provide 'make-theme-preview)

(defvar make-theme-preview-window nil)
(defvar make-theme-preview-type 'default)
(defvar make-theme-preview-theme nil)

(add-hook 'before-add-window-hook
	  (lambda (w)
	    (let
		((class (get-x-text-property w 'WM_CLASS)))
	      (when (and (>= (length class) 2)
			 (string= (aref class 1) "SawmillThemer")
			 (string= (aref class 0) "preview"))
		(setq make-theme-preview-window w)
		(when make-theme-preview-theme
		  (set-window-frame
		   w (make-theme-preview-theme w make-theme-preview-type)))))))

(add-hook 'destroy-notify-hook
	  (lambda (w)
	    (when (eq w make-theme-preview-window)
	      (setq make-theme-preview-window nil))))

(defun make-theme-preview (patterns frames mappings)
  (let
      ((theme (make-theme patterns frames mappings)))
    (setq make-theme-preview-theme theme)
    (when make-theme-preview-window
      (set-window-frame make-theme-preview-window
			(theme make-theme-preview-window
			       make-theme-preview-type)))))