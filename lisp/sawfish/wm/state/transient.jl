;; transient.jl -- support transient windows
;; $Id$

(provide 'transient)

(defvar transient-frame nil-frame
  "Frame used to decorate transient windows.")

(defun transient-add-window (w)
  (when (window-transient-p w)
    (set-window-frame w transient-frame)))

(add-hook 'add-window-hook 'transient-add-window t)
