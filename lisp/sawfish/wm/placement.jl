;; place-window.jl -- decide where to initially place a window
;; $Id$

(provide 'place-window)

(defvar place-window-mode 'random)

;; XXX the main problem with this code is that it doesn't respect
;; XXX pre-specified positions, until then it's more trouble than
;; XXX it's worth..

;; called from the add-window-hook
(defun place-window (w)
  (cond ((eq place-window-mode 'by-hand)
	 ;; XXX this doesn't work; why not?
	 (move-window-interactively w))
	((eq place-window-mode 'smart)
	 ;; XXX implement this..
	 (setq place-window-mode 'random))
	((eq place-window-mode 'random)
	 (move-window-to
	  w
	  (random (max 0 (- (screen-width) (car (window-dimensions w)))))
	  (random (max 0 (- (screen-height) (cdr (window-dimensions w)))))))))

(add-hook 'add-window-hook 'place-window)
