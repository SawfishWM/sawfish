;; tabgroup.jl - tab main
;;
;; Copyright (C) Yann Hodique <Yann.Hodique@lifl.fr>
;;
;; This file is an official accepted contribution into sawfish.
;;
;; This script is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 22, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.tabs.tabgroup

    (export window-tabbed-p
            adjustment-title
            tab-refresh-group
            tab-release-window
            tab-raise-left-window
            tab-raise-right-window
            tab-find-window
            tab-rank
            tab-group-window-list
            tab-group-windows-index
            tab-group-window)
    
    (open rep
          rep.system
          rep.data.records
          sawfish.wm.gaol
          sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.windows
          sawfish.wm.frames
          sawfish.wm.state.iconify
          sawfish.wm.state.shading
          sawfish.wm.commands.move-resize
          sawfish.wm.stacking
          sawfish.wm.util.groups
          sawfish.wm.commands.groups
          sawfish.wm.workspace)
  
  (define-structure-alias tabgroup sawfish.wm.tabs.tabgroup)
  
    (defcustom tab-raise-on-hover nil
    "Raise tabs while hovering them."
    :group focus
    :type boolean)

  (define tab-groups nil)
  (define tab-refresh-lock t)
  (define tab-move-lock t)

  (define (window-tabbed-p w)
    (window-get w 'tabbed))

  (define-record-type :tab-group
    (tab-build-group p d wl)
    tab-group?
    (p tab-group-position)
    (d tab-group-dimensions)
    (wl tab-group-window-list))

  (define (adjustment-title w)
    (call-window-hook 'window-state-change-hook w (list '(title-position))))

  (define (tab-move-resize-frame-window-to win x y w h)
    "Move and resize according to *frame* dimensions."
    (let* ((dim1 (window-dimensions win))
           (dim2 (window-frame-dimensions win))
           (dw (- (car dim2) (car dim1)))
           (dh (- (cdr dim2) (cdr dim1))))
      (move-resize-window-to win x y (- w dw) (- h dh))))

  (define (tab-make-new-group win)
    "Return a new group containing only WIN."
    (let* ((pos (window-position win))
           (dim (window-frame-dimensions win))
           (group (tab-build-group pos dim (list win))))
      (setq tab-groups (append tab-groups (cons group nil)))
      group))

  (define (tab-find-window win)
    "Return the group containing win."
    (let loop ((gr tab-groups))
      (cond
       ((null gr)
        (tab-make-new-group win)
        )
       ((member win (tab-group-window-list (car gr)))
        (car gr))
       (t
        (loop (cdr gr))))))

  (define (tab-window-group-index win)
    "Return the index of the group containing win."
    (let loop ((index 0))
      (cond
       ((eq index (length tab-groups))
        (tab-make-new-group win)
        index)
       ((member win (tab-group-window-list (nth index tab-groups)))
        index)
       (t
        (loop (+ index 1))))))

  (define (tab-group-windows-index win)
    "Return the windows of the group containing win."
    (let* ((index (tab-window-group-index win))
           (wins (tab-group-window-list (nth index tab-groups))))
      wins))

  (define (tab-rank elem list)
    (if (eq elem (car list))
        0
      (+ 1 (tab-rank elem (cdr list)))))
  
  (define (tab-delete-window-from-group win index)
    "Remove WIN from the group at given index."
    (let* ((old (nth index tab-groups))
           (l (remove win (tab-group-window-list old))))
      (if (null l)
          (setq tab-groups (delete old tab-groups))
        (rplaca (nthcdr index tab-groups)
                (tab-build-group (tab-group-position old) (tab-group-dimensions old) l))
        (window-put win 'tabbed nil)
        (tab-refresh-group win 'raise)
        (tab-refresh-group win 'frame)
        (window-put win 'fixed-position nil)
        (if (not (cdr l))
            (window-put (car l) 'tabbed nil))
        (tab-refresh-group (car l) 'frame))))

  (define (tab-delete-window-from-tab-groups w)
    "Find window's group and remove it."
    (when (window-tabbed-p w)
      (tab-delete-window-from-group w (tab-window-group-index w))))

  (define (tab-put-window-in-group win index)
    "Put window in group at given index."
    (let* ((group (nth index tab-groups))
           (dim (tab-group-dimensions group))
           (pos (tab-group-position group)))
      (rplaca (nthcdr index tab-groups)
              (tab-build-group (tab-group-position group)
                               (tab-group-dimensions group)
                               (append (tab-group-window-list group) (list win))))
      (tab-move-resize-frame-window-to win (car pos) (cdr pos) (car dim) (cdr dim))
      (rebuild-frame win)))

  (define (tab-refresh-group win prop)
    "Refresh the entire group containing WIN according to PROP.
PROP can be one of the symbols: raise, frame, reframe, style, move,
resize, title-position, type, depth, shade, unshade, iconify, uniconify,
sticky, unsticky, fixed-position."
    (when tab-refresh-lock
      (setq tab-refresh-lock nil)
      (unwind-protect
          (let* ((index (tab-window-group-index win))
                 (wins (tab-group-window-list (nth index tab-groups)))
                 (focus (tab-group-offset win 0)))
            (adjustment-title win)
            (cond
             ((eq prop 'raise)
              (raise-windows focus wins))
             ((eq prop 'title-position)
              (let ((group-title-position (window-get win 'title-position)))
                (mapcar (lambda (w)
                          (window-put w 'title-position group-title-position)) wins)))
             ((eq prop 'frame)
              (mapcar (lambda (w)
                        (rebuild-frame w)) wins))
             ((eq prop 'reframe)
              (mapcar (lambda (w)
                        (reframe-window w)) wins))
             ((eq prop 'style)
              (let ((group-frame-style (window-get win 'frame-style))
                    (dim (window-frame-dimensions win))
                    (pos (window-position win)))
                (mapcar (lambda (w)
                          (set-frame-style w group-frame-style)
                          (tab-move-resize-frame-window-to w (car pos) (cdr pos) (car dim) (cdr dim))
                          (reframe-window w)) wins)))
             ((or (eq prop 'move) (eq prop 'resize))
              (let ((dim (window-frame-dimensions win))
                    (pos (window-position win)))
                (mapcar (lambda (w)
                          (tab-move-resize-frame-window-to w (car pos) (cdr pos) (car dim) (cdr dim))
                          (rebuild-frame w)) wins)
                (rplaca (nthcdr index tab-groups)
                        (tab-build-group pos dim wins))))
             ((eq prop 'fixed-position)
              (let ((group-frame-fixed-position (window-get win 'fixed-position)))
                (mapcar (lambda (w)
                          (window-put w 'fixed-position group-frame-fixed-position)
                          (rebuild-frame w)) wins)))
             ((eq prop 'type)
              (let ((group-frame-type (window-get win 'type)))
                (mapcar (lambda (w)
                          (window-put w 'type group-frame-type)
                          (rebuild-frame w)) wins)))
             ((eq prop 'depth)
              (let ((group-frame-depth (window-get win 'depth)))
                (mapcar (lambda (w)
                          (window-put w 'depth group-frame-depth)
                          (rebuild-frame w)) wins)))
             ((eq prop 'iconify)
              (mapcar (lambda (w)
                        (iconify-window w)
                        (rebuild-frame w)) wins))
             ((eq prop 'uniconify)
              (mapcar (lambda (w)
                        (uniconify-window w)
                        (rebuild-frame w)) wins))
             ((eq prop 'sticky)
              (mapcar (lambda (w)
                        (make-window-sticky w)
                        (rebuild-frame w)) wins))
             ((eq prop 'unsticky)
              (mapcar (lambda (w)
                        (make-window-unsticky w)
                        (rebuild-frame w)) wins))
             ((eq prop 'shade)
              (mapcar (lambda (w)
                        (shade-window w)
                        (rebuild-frame w)) wins))
             ((eq prop 'unshade)
              (mapcar (lambda (w)
                        (unshade-window w)
                        (rebuild-frame w)) wins))))
        (setq tab-refresh-lock t))))

  ;; Entry points
  (define (tab-group-window w win)
    "Add window W to tabgroup containing WIN."
    ;; don't add a window as tab, if it exists
    ;; on another workspace or viewport
    (when (not (cdr (window-get win 'workspaces)))
      ;; from tabgroup to tabgroup
      (when (window-tabbed-p w)
        (tab-delete-window-from-tab-groups w))
      ;; sort windows/tabs (depth)
      (tab-refresh-group win 'raise)
      (raise-window w)
      (set-input-focus w)
      (let* ((index (tab-window-group-index win))
             (index2 (tab-window-group-index w))
             ;; adopt window settings for the new tab
             (group-frame-style (window-get win 'frame-style))
             (group-frame-type (window-get win 'type))
             (group-frame-sticky (window-get win 'sticky))
             (group-frame-fixed-position (window-get win 'fixed-position))
             (group-frame-title-position (window-get win 'title-position))
             (group-frame-depth (window-get win 'depth)))
        (when (not (eq index index2))
          (if (window-get w 'shaded) (unshade-window w))
          (if (window-get win 'shaded) (unshade-window win))
          (window-put w 'frame-style group-frame-style)
          (window-put w 'type group-frame-type)
          (window-put w 'title-position group-frame-title-position)
          (window-put w 'sticky group-frame-sticky)
          (window-put w 'depth group-frame-depth)
          (window-put w 'fixed-position group-frame-fixed-position)
          ;; reframe w here, tab-refresh-group expectet
          ;; the same frame for w and win
          (reframe-window w)
          (tab-refresh-group win 'move)
          (tab-put-window-in-group w index)
          (tab-delete-window-from-group w index2)
          (tab-refresh-group win 'frame)
          (tab-refresh-group w 'move)
          (if (not (window-tabbed-p win)) (window-put win 'tabbed t))
          (window-put w 'tabbed t)))))
  
  (define (tab-release-window w)
    "Release the window from its group."
    (tab-delete-window-from-tab-groups w)
    (tab-make-new-group w))
  
  (define-command 'tab-release-window tab-release-window #:spec "%f")
  
  (define (tab-group-offset win n)
    "Return the window at position (pos+n) in window's group."
    (let* ((gr (tab-group-window-list (tab-find-window win)))
           (size (length gr))
           (r (tab-rank win gr)))
      (nth (modulo (+ r n) size) gr)))
  
  (define (tab-same-group-p w1 w2)
    "Predicate : true <=> w1 and w2 are grouped together."
    (member w1 (tab-group-window-list (tab-find-window w2))))

  (define (tab-raise-left-window)
    "Raise left window in current tab group."
    (let ((win (tab-group-offset (input-focus) -1)))
      (raise-window win)
      (set-input-focus win)))

  (define-command 'tab-raise-left-window tab-raise-left-window)

  (define (tab-raise-right-window)
    "Raise right window in current tab group."
    (let ((win (tab-group-offset (input-focus) 1)))
      (raise-window win)
      (set-input-focus win)))

  (define-command 'tab-raise-right-window tab-raise-right-window)

  (define (map-other-grouped-windows win func)
    ""
    (mapcar func
            (delete-if
             (lambda (w) (eq w win))
             (tab-group-window-list (tab-find-window win)))) )

  (define (tab-group-raise win)
    (if (window-tabbed-p win)
        (tab-refresh-group win 'raise)))

  (define (tab-group-sticky win)
    (if (window-get win 'sticky)
        (tab-refresh-group win 'sticky)
      (tab-refresh-group win 'unsticky)))

  (unless batch-mode
    (add-hook 'window-state-change-hook
              (lambda (win args)
                (when (window-tabbed-p win)
                  (setq args (car args))
                  (cond ((eq 'sticky args)
                         (tab-group-sticky win))
                        ((eq 'fixed-position args)
                         (tab-refresh-group win 'fixed-position))
                        ((eq 'frame-style args)
                         (tab-refresh-group win 'style)
                         (tab-refresh-group win 'move))
                        ((eq 'type args)
                         (tab-refresh-group win 'type))
                        ((eq 'stacking args)
                         (tab-refresh-group win 'depth))))))
    
    (add-hook 'focus-in-hook 
              (lambda (win) 
                (if tab-raise-on-hover
                    (tab-group-raise win))))
    (add-hook 'after-move-hook (lambda (win)  (if (window-tabbed-p win) (tab-refresh-group win 'move))))
    (add-hook 'after-resize-hook (lambda (win)  (if (window-tabbed-p win) (tab-refresh-group win 'resize))))
    ;; only update tabs by move if opaque move mode (opaque = slow)
    ;;
    (when (eq move-outline-mode 'opaque)
      (add-hook 'while-moving-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'move))))
      )
    (add-hook 'window-resized-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'resize))))
    (add-hook 'shade-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'shade))))
    (add-hook 'unshade-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'unshade))))
    (add-hook 'iconify-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'iconify))))
    (add-hook 'uniconify-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'uniconify))))
    (add-hook 'add-to-workspace-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'frame))))
    (add-hook 'destroy-notify-hook tab-delete-window-from-tab-groups))

  (gaol-add tab-refresh-group tab-group-windows-index)
  )
