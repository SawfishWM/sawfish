;; focus.jl -- implement standard focus behaviour
;; $Id$

(provide 'focus)

(defvar sloppy-focus nil
  "When non-nil, and the usual focus-follows-mouse behaviour is in use, the
focus is only changed when a top-level window is entered, never when the
root window is entered.")

(defun focus-enter-fun (w)
  (if (eq w 'root)
      (unless sloppy-focus
	(set-input-focus nil))
    (set-input-focus w)))

(defun focus-leave-fun (w)
  (unless sloppy-focus
    (set-input-focus nil)))

(add-hook 'enter-notify-hook 'focus-enter-fun t)
(add-hook 'leave-notify-hook 'focus-leave-fun t)
