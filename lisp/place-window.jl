;; place-window.jl -- decide where to initially place a window
;; $Id$

(provide 'place-window)

(defvar place-window-mode 'random)

;; called from the place-window-hook
(defun place-window (w)
  (if (window-transient-p w)
      nil
    (let
	((mode (or (window-get w 'place-mode) place-window-mode)))
      (cond ((eq mode 'smart)
	     ;; XXX implement this..
	     (setq mode 'random))
;	    ((eq mode 'interactive)
;	     ;; XXX this doesn't work; why not?
;	     (let
;		 ((move-outline-mode nil))
;	       (move-window-interactively w)))
	    ((eq mode 'random)
	     (move-window-to
	      w
	      (random (max 0 (- (screen-width)
				(car (window-dimensions w)))))
	      (random (max 0 (- (screen-height)
				(cdr (window-dimensions w))))))))
      t)))

(add-hook 'place-window-hook 'place-window t)
