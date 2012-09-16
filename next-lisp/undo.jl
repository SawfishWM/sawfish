;; -*- indent-tabs-mode: nil -*-
;;
;; File: undo.jl
;; Version: $Revision: 1.4 $
;; Description: 
;;     Remembers window activity (moving, resizing, iconifying, shading,
;;     workspace switching) and lets you undo it.
;;
;;     Do you ever accidentally move a window, and then have to tediously
;;     replace it where it used to be?  Ever hit a "clean up this cluttered
;;     desktop" command, and decide that the results are worse than what you
;;     started with?
;;
;; Installation:
;;     Install this file, and add the following lines to your .sawmillrc:
;;
;; (setq load-path (cons "/directory/containing/this/file/" load-path))
;; (require 'undo)
;;
;;     And then bind the "undo" and "undo-redo" commands to two handy keys.
;;
;; Author:
;;     Terry Weissman <terry@weissman.org>
;;     Latest version can be found at http://www.weissman.org/sawfish
;;
;; Changelog:
;;    $Log: undo.jl,v $
;;    Revision 1.4  2000/09/11 15:34:50  weissman
;;    Added missing 'require' line.
;;
;;    Revision 1.3  2000/05/31 21:29:05  terry
;;    Added support for the repel package.  Also, put in some not-yet-working
;;    stuff for undoing stacking order changes.
;;
;;    Revision 1.2  2000/05/23 16:47:24  terry
;;    "redo" should be a no-op if the last relevant operation was not an "undo".
;;    Also, fixed compiler warnings.
;;


(require 'timers)

(defvar l "")

(defun undo-eval (cmd)
;  (grab-server)
  (eval cmd)
;  (ungrab-server)
;  (display-message (format nil "%S" cmd))
)

(defun undo-record (l)
  (setq undo-curcmd-list (cons l undo-curcmd-list)))

(defun undo ()
  "Undo last window move or resize"
  (interactive)
  (let ((cmd (car undo-list))
        (x (cdr undo-list))
        (r undo-redo-list))
    (setq undo-list ())
    (setq undo-curcmd-list ())
    (undo-eval cmd)
    (undo-transfer-to-undo-list)
    (if undo-list
        (setq r (cons (car undo-list) r)))
    (setq undo-redo-list r)
    (setq undo-list x)
    (setq undo-curcmd-list ())))


(defun undo-redo ()
  "Redo last window move or resize"
  (interactive)
  (let ((cmd (car undo-redo-list))
        (r (cdr undo-redo-list)))
    (undo-eval cmd)
    (make-timer (lambda () (setq undo-redo-list r)) 0 1)))


(defun undo-record-move (w)
  (let ((oldloc (window-get w 'undo-loc))
        (newloc (window-position w)))
    (if oldloc
        (unless (equal oldloc newloc)
          (undo-record (list 'move-window-to w (car oldloc) (cdr oldloc)))))
    (window-put w 'undo-loc newloc)))
  
    
(defun undo-record-resize (w)
  (let ((oldsize (window-get w 'undo-size))
        (newsize (window-dimensions w)))
    (if oldsize
        (unless (equal oldsize newsize)
          (undo-record (list 'resize-window-to w (car oldsize) (cdr oldsize)))))
    (window-put w 'undo-size newsize)))
  
    
(defun undo-record-stacking-for-real ()
;  (undo-pre-command)
  (setq l (concat l "s"))
  (let ((newstack (stacking-order)))
    (unless (equal undo-cur-stack newstack)
      (undo-record `(restack-windows (quote ,undo-cur-stack)))
      (setq undo-cur-stack newstack)))
;  (undo-post-command)
)

(defun undo-record-stacking ()
  ; Doing the real stuff now seems to cause weird event-droppings or something.
  ; So, we will just do it soon.
 ; (setq undo-check-stacking t)
  (undo-record-stacking-for-real))

        
       

(defun undo-record-workspace (w)
  (undo-record (list 'select-workspace w)))


(defun undo-record-iconify (w)
  (undo-record (list 'uniconify-window w)))

(defun undo-record-uniconify (w)
  (undo-record (list 'iconify-window w)))

(defun undo-record-shade (w)
  (undo-record (list 'unshade-window w)))

(defun undo-record-unshade (w)
  (undo-record (list 'shade-window w)))


(defun undo-transfer-to-undo-list ()
  (if undo-check-stacking
      (progn
	(undo-record-stacking-for-real)
	(setq undo-check-stacking nil)))
  (if undo-curcmd-list
      (progn
        (setq undo-list (cons (cons 'progn undo-curcmd-list) undo-list))
        (setq undo-curcmd-list nil)
        (setq undo-redo-list nil)
        (while (> (length undo-list) 50)
          (setq undo-list (nreverse (cdr (nreverse undo-list))))))))

  
(defun undo-transfer-if-done ()
  (unless (or undo-in-command undo-in-interactive (< 0 undo-group-depth))
    (undo-transfer-to-undo-list)))


(defun undo-pre-command ()
  (setq undo-in-command t))

(defun undo-post-command ()
  (setq undo-in-command nil)
 ( undo-transfer-if-done))

(defun undo-pre-interactive ()
  (setq undo-in-interactive t))

(defun undo-post-interactive (w)
  (setq undo-in-interactive nil)
  (undo-record-resize w)                ;These two lines shouldn't be
  (undo-record-move w)                  ;necessary, but they don't hurt...
  (undo-transfer-if-done))

(defun undo-start-group ()
  (setq undo-group-depth (+ undo-group-depth 1)))

(defun undo-end-group ()
  (setq undo-group-depth (- undo-group-depth 1))
  (undo-transfer-if-done))


(defun undo-record-all-locs ()
  (setq undo-cur-stack (stacking-order))
  (map-windows (lambda (w)
                 (window-put w 'undo-loc (window-position w))
                 (window-put w 'undo-size (window-dimensions w)))))



;;;;; Initialize global variables.

(defvar undo-list ())

(defvar undo-redo-list ())

(defvar undo-curcmd-list ())

(defvar undo-in-command nil)

(defvar undo-in-interactive nil)

(defvar undo-cur-stack nil)

(defvar undo-check-stacking nil)

(defvar undo-group-depth 0)



;;;;; Set hooks for operations that we want to be able to undo.


(unless (in-hook-p 'window-moved-hook undo-record-move)
  (add-hook 'window-moved-hook undo-record-move))

(unless (in-hook-p 'window-resized-hook undo-record-resize)
  (add-hook 'window-resized-hook undo-record-resize))

(unless (in-hook-p 'leave-workspace-hook undo-record-workspace)
  (add-hook 'leave-workspace-hook undo-record-workspace))

(unless (in-hook-p 'iconify-window-hook undo-record-iconify)
  (add-hook 'iconify-window-hook undo-record-iconify))

(unless (in-hook-p 'uniconify-window-hook undo-record-uniconify)
  (add-hook 'uniconify-window-hook undo-record-uniconify))

(unless (in-hook-p 'shade-window-hook undo-record-shade)
  (add-hook 'shade-window-hook undo-record-shade))

(unless (in-hook-p 'unshade-window-hook undo-record-unshade)
  (add-hook 'unshade-window-hook undo-record-unshade))

; (unless (in-hook-p 'after-restacking-hook undo-record-stacking)
;   (add-hook 'after-restacking-hook undo-record-stacking))

; (unless (in-hook-p 'window-depth-changed-hook undo-record-stacking)
;   (add-hook 'window-depth-changed-hook undo-record-stacking))

; (unless (in-hook-p 'visibilty-notify-hook undo-record-stacking)
;   (add-hook 'visibilty-notify-hook undo-record-stacking))



;;;;; Set hooks for operations that tend to group un-do-able operations.

(unless (in-hook-p 'pre-command-hook undo-pre-command)
  (add-hook 'pre-command-hook undo-pre-command))

(unless (in-hook-p 'post-command-hook undo-post-command)
  (add-hook 'post-command-hook undo-post-command))

(unless (in-hook-p 'before-move-hook undo-pre-interactive)
  (add-hook 'before-move-hook undo-pre-interactive))

(unless (in-hook-p 'after-move-hook undo-post-interactive)
  (add-hook 'after-move-hook undo-post-interactive))

(unless (in-hook-p 'before-resize-hook undo-pre-interactive)
  (add-hook 'before-resize-hook undo-pre-interactive))

(unless (in-hook-p 'after-resize-hook undo-post-interactive)
  (add-hook 'after-resize-hook undo-post-interactive))



;;; Remember the initial size and locations of all windows, so that if they
;;; change we will know what their old values were.

(make-timer undo-record-all-locs 1 0)


; (setq log "")
; (add-hook 'window-moved-hook (lambda () (setq log (concat log "*"))))
; (add-hook 'before-move-hook (lambda () (setq log (concat log "m"))))
; (add-hook 'after-move-hook (lambda () (setq log (concat log "M"))))
; (add-hook 'pre-command-hook (lambda () (setq log (concat log "c"))))
; (add-hook 'post-command-hook (lambda () (setq log (concat log "C"))))


