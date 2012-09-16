;; -*- indent-tabs-mode: nil -*-
;;
;; File: undo-viewport.jl
;; Version: $Revision: 0.1 $
;; Description: 
;;     Remembers changing viewports, allows to switch back and forth.
;;
;; Installation:
;;     Install this file, and add the following lines to your .sawmillrc:
;;
;; (setq load-path (cons "/directory/containing/this/file/" load-path))
;; (require 'undo-viewport)
;;
;;     And then bind the "viewport-switch-undo" and "viewport-switch-redo" commands to two handy keys.
;;
;; Author:
;;     Janek Kozicki <cosurgi@gmail.com>
;;     Terry Weissman <terry@weissman.org> - original author of undo.jl which
;;     was shamelessly modified here
;;


(require 'timers)

(defvar l "")

(defun undo-viewport-eval (cmd)
  (eval cmd)
)

(defun undo-viewport-record (l)
  (setq viewport-undo-cmd-list (cons l viewport-undo-cmd-list)))

(defun viewport-switch-undo ()
  "Undo last viewport switch"
  (interactive)
  (let ((cmd (car viewport-undo-list))
        (x (cdr viewport-undo-list))
        (r viewport-undo-redo-list))
    (setq viewport-undo-list ())
    (setq viewport-undo-cmd-list ())
    (undo-viewport-eval cmd)
    (viewport-undo-transfer-to-viewport-undo-list)
    (if viewport-undo-list
        (setq r (cons (car viewport-undo-list) r)))
    (setq viewport-undo-redo-list r)
    (setq viewport-undo-list x)
    (setq viewport-undo-cmd-list ())))


(defun viewport-switch-redo ()
  "Redo last viewport switch"
  (interactive)
  (let ((cmd (car viewport-undo-redo-list))
        (r (cdr viewport-undo-redo-list)))
    (undo-viewport-eval cmd)
    (make-timer (lambda () (setq viewport-undo-redo-list r)) 0 1)))
 

(defun undo-viewport-record-workspace ()
  (undo-viewport-record (list 'set-screen-viewport viewport-last-coords-x viewport-last-coords-y ))
  (setq viewport-last-coords-x (car (screen-viewport)))
  (setq viewport-last-coords-y (cdr (screen-viewport)))
  (viewport-undo-transfer-to-viewport-undo-list)
  )

(defun viewport-undo-transfer-to-viewport-undo-list ()
  (if viewport-undo-check-stacking
      (progn
	(setq viewport-undo-check-stacking nil)))
  (if viewport-undo-cmd-list
      (progn
        (setq viewport-undo-list (cons (cons 'progn viewport-undo-cmd-list) viewport-undo-list))
        (setq viewport-undo-cmd-list nil)
        (setq viewport-undo-redo-list nil)
        (while (> (length viewport-undo-list) 50)
          (setq viewport-undo-list (nreverse (cdr (nreverse viewport-undo-list))))))))

;;;;; Initialize global variables.

(defvar viewport-undo-list ())

(defvar viewport-undo-redo-list ())

(defvar viewport-undo-cmd-list ())

(defvar viewport-last-coords-x 0)
(defvar viewport-last-coords-y 0)

(defvar viewport-undo-cur-stack nil)

(defvar viewport-undo-check-stacking nil)

;;;;; Set hooks for operations that we want to be able to undo.

(unless (in-hook-p 'viewport-moved-hook undo-viewport-record-workspace)
  (add-hook 'viewport-moved-hook undo-viewport-record-workspace))

