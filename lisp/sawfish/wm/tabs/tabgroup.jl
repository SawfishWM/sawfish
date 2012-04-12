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
            adjust-title
            tab-refresh-group
            tab-release-window
            tab-raise-left-window
            tab-raise-right-window
            tab-find-window
            tab-rank
            tab-group-window-list
            tab-group-window-index
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
  
  (define current-win nil)
  (define all-wins nil)
  (define oldgroup nil)
  (define tab-groups nil)
  (define tab-refresh-lock t)
  (define release-window t)
  (define last-unmap-id nil)
  (define in-tab-group-name nil)

  (define (window-tabbed-p w)
    (window-get w 'tabbed))

  (define-record-type :tab-group
    (tab-build-group p d wl)
    tab-group?
    (p tab-group-position)
    (d tab-group-dimensions)
    (wl tab-group-window-list))

  (define (adjust-title w)
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

  (define (tab-group-window-index win)
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
        (if (not (cdr l))
            (window-put (car l) 'tabbed nil))
        (setq oldgroup (car l)))))

  (define (tab-delete-window-from-tab-groups w)
    "Find window's group and remove it."
    (if release-window
        (remove-from-tab-group w))
    (setq release-window t)
    (when (window-tabbed-p w)
      (tab-delete-window-from-group w (tab-window-group-index w))
      (window-put w 'fixed-position nil)
      (tab-refresh-group oldgroup 'frame)
      (rebuild-frame w 'frame)))

  (define (tab-put-window-in-group win index)
    "Put window in group at given index."
    (let* ((group (nth index tab-groups)))
      (rplaca (nthcdr index tab-groups)
              (tab-build-group (tab-group-position group)
                               (tab-group-dimensions group)
                               (append (tab-group-window-list group) (list win))))))

  (define (tab-refresh-group win prop)
    "Refresh the entire group containing WIN according to PROP.
PROP can be one of the symbols: raise, frame, reframe, reframe-all, style, move,
resize, title-position, type, depth, shade, unshade, iconify, uniconify, maximized,
sticky, unsticky, fixed-position."
    (when tab-refresh-lock
      (setq tab-refresh-lock nil)
      (unwind-protect
          (let* ((index (tab-window-group-index win))
                 (wins (tab-group-window-list (nth index tab-groups)))
                 (focus (tab-group-offset win 0))
                 (unfocus (remove focus wins)))
            (adjust-title win)
            (cond
             ((eq prop 'raise)
              (raise-windows focus wins))
             ((eq prop 'title-position)
              (let ((group-title-position (window-get win 'title-position)))
                (mapcar (lambda (w)
                          (window-put w 'title-position group-title-position)) unfocus)
               (window-put focus 'title-position group-title-position)))
             ((eq prop 'frame)
              (mapcar (lambda (w)
                        (rebuild-frame w)) unfocus)
              (rebuild-frame focus))
             ((eq prop 'reframe)
              (mapcar (lambda (w)
                        (reframe-window w)) unfocus))
             ((eq prop 'reframe-all)
              (mapcar (lambda (w)
                        (reframe-window w)) unfocus)
              (reframe-window focus))
             ((or (eq prop 'move) (eq prop 'resize))
              (let ((dim (window-frame-dimensions win))
                    (pos (window-position win)))
                (mapcar (lambda (w)
                          (tab-move-resize-frame-window-to w (car pos) (cdr pos) (car dim) (cdr dim))) unfocus)))
             ((eq prop 'style)
              (let ((group-frame-style (window-get win 'frame-style))
                    (dim (window-frame-dimensions win))
                    (pos (window-position win)))
                (mapcar (lambda (w)
                          (set-frame-style w group-frame-style)
                          (tab-move-resize-frame-window-to w (car pos) (cdr pos) (car dim) (cdr dim))) unfocus)))
             ((eq prop 'fixed-position)
              (let ((group-frame-fixed-position (window-get win 'fixed-position)))
                (mapcar (lambda (w)
                          (window-put w 'fixed-position group-frame-fixed-position)) unfocus)))
             ((eq prop 'type)
              (let ((group-frame-type (window-get win 'type)))
                (mapcar (lambda (w)
                          (window-put w 'type group-frame-type)) unfocus)))
             ((eq prop 'depth)
              (let ((group-frame-depth (window-get win 'depth)))
                (mapcar (lambda (w)
                          (window-put w 'depth group-frame-depth)) unfocus)
                (raise-window win)
                (set-input-focus win)))
             ((eq prop 'iconify)
              (mapcar (lambda (w)
                        (iconify-window w)) unfocus))
             ((eq prop 'uniconify)
              (mapcar (lambda (w)
                        (uniconify-window w)) unfocus))
             ((eq prop 'maximized)
              (let ((dim (window-frame-dimensions win))
                    (pos (window-position win))
                    (group-frame-maximized-vertically (window-get win 'maximized-vertically))
                    (group-frame-maximized-horizontally (window-get win 'maximized-horizontally))
                    (group-frame-maximized-fullscreen (window-get win 'maximized-fullscreen))
                    (group-frame-unmaximized-type (window-get win 'unmaximized-type))
                    (group-frame-unmaximized-geometry (window-get win 'unmaximized-geometry)))
                (mapcar (lambda (w)
                          (window-put w 'maximized-vertically group-frame-maximized-vertically)
                          (window-put w 'maximized-horizontally group-frame-maximized-horizontally)
                          (window-put w 'maximized-fullscreen group-frame-maximized-fullscreen)
                          (window-put w 'unmaximized-type group-frame-unmaximized-type)
                          (window-put w 'unmaximized-geometry group-frame-unmaximized-geometry)
                          (tab-move-resize-frame-window-to w (car pos) (cdr pos) (car dim) (cdr dim))) unfocus)))
             ((eq prop 'sticky)
              (mapcar (lambda (w)
                        (make-window-sticky w)) unfocus))
             ((eq prop 'unsticky)
              (mapcar (lambda (w)
                        (make-window-unsticky w)) unfocus))
             ((eq prop 'shade)
              (mapcar (lambda (w)
                        (shade-window w)) unfocus)) 
             ((eq prop 'unshade)
              (mapcar (lambda (w)
                        (unshade-window w)) unfocus))))
        (setq tab-refresh-lock t))))

  ;; Entry points
  (define (tab-group-window w win)
    "Add window W to tabgroup containing WIN."
    ;; don't add a window as tab, if it already
    ;; exists on another workspace or window type 
    ;; is not a "normal" window (e.g. dock panel ...)
    (when (and (not (cdr (window-get win 'workspaces)))
               (equal (aref (nth 2 (get-x-property w '_NET_WM_WINDOW_TYPE)) 0) '_NET_WM_WINDOW_TYPE_NORMAL)
               (equal (aref (nth 2 (get-x-property win '_NET_WM_WINDOW_TYPE)) 0) '_NET_WM_WINDOW_TYPE_NORMAL))
      (let* ((index (tab-window-group-index win))
             (index2 (tab-window-group-index w))
             (pos (window-position win))
             (dim (window-dimensions win))
             ;; adopt window settings for the new tab
             (group-frame-style (window-get win 'frame-style))
             (group-frame-type (window-get win 'type))
             (group-frame-focus-mode (window-get win 'focus-mode))
             (group-frame-gravity (window-get win 'gravity))
             (group-frame-sticky (window-get win 'sticky))
             (group-frame-sticky-viewport (window-get win 'sticky-viewport))
             (group-frame-never-iconify (window-get win 'never-iconify))
             (group-frame-fixed-position (window-get win 'fixed-position))
             (group-frame-title-position (window-get win 'title-position))
             (group-frame-depth (window-get win 'depth))
             (group-frame-never-maximize (window-get win 'never-maximize))
             (group-frame-maximized-vertically (window-get win 'maximized-vertically))
             (group-frame-maximized-horizontally (window-get win 'maximized-horizontally))
             (group-frame-maximized-fullscreen (window-get win 'maximized-fullscreen))
             (group-frame-unmaximized-type (window-get win 'unmaximized-type))
             (group-frame-unmaximized-geometry (window-get win 'unmaximized-geometry)))
        (when (not (eq index index2))
          ;; tabgroup to tabgroup
          (when (window-tabbed-p w)
            (setq release-window nil)
            (tab-delete-window-from-tab-groups w))
          (if (window-get win 'iconified) (uniconify-window win))
          (let ((group-frame-to-workspaces (car (window-workspaces win))))
            (if (window-get win 'shaded) (unshade-window win))
            (setq tab-refresh-lock nil)
            (if (window-get w 'iconified) (uniconify-window w))
            (let ((group-frame-from-workspaces (car (window-workspaces w))))
              (if (window-get w 'shaded) (unshade-window w))
              (window-put w 'frame-style group-frame-style)
              (window-put w 'type group-frame-type)
              (window-put w 'focus-mode group-frame-focus-mode)
              (window-put w 'gravity group-frame-gravity)
              (window-put w 'title-position group-frame-title-position)
              (window-put w 'sticky group-frame-sticky)
              (window-put w 'sticky-viewport group-frame-sticky-viewport)
              (window-put w 'never-iconify group-frame-never-iconify)
              (window-put w 'depth group-frame-depth)
              (window-put w 'fixed-position group-frame-fixed-position)
              (window-put w 'never-maximize group-frame-never-maximize)
              (window-put w 'maximized-vertically group-frame-maximized-vertically)
              (window-put w 'maximized-horizontally group-frame-maximized-horizontally)
              (window-put w 'maximized-fullscreen group-frame-maximized-fullscreen)
              (window-put w 'unmaximized-type group-frame-unmaximized-type)
              (window-put w 'unmaximized-geometry group-frame-unmaximized-geometry)
              ;; reframe w here, tab-refresh-group expectet
              ;; the same frame for w and win
              (reframe-window w)
              (tab-put-window-in-group w index)
              (tab-delete-window-from-group w index2)
              (resize-window-to w (car dim) (cdr dim))
              (move-window-to w (car pos) (cdr pos))
              (when (and group-frame-to-workspaces group-frame-from-workspaces
                         (not (eq group-frame-to-workspaces group-frame-from-workspaces)))
                (move-window-to-workspace w group-frame-from-workspaces group-frame-to-workspaces))
              (setq tab-refresh-lock t)
              (tab-refresh-group w 'frame)
              (set-input-focus w)
              (if (not (window-tabbed-p win)) (window-put win 'tabbed t))
              (window-put w 'tabbed t)))))))
  
  (define (tab-release-window w)
    "Release the window from its group."
    (setq release-window nil)
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

  (define (before-move-resize win)
    "Releas win from the tabgroup and iconify the rest from the group."
    (when (not (window-get win 'fixed-position))
      (when (eq all-wins nil)
        (let* ((index (tab-window-group-index win))
               (wins (tab-group-window-list (nth index tab-groups)))
               (tabs (remove win (tab-group-window-list (nth index tab-groups))))
               (default-window-animator 'none))
          (tab-delete-window-from-group win index)
          (rebuild-frame win)
          (setq tab-refresh-lock nil)
          (mapcar (lambda (w)
                    (when (window-get w 'never-iconify)
                      (window-put w 'never-iconify nil)
                      (window-put w 'never-iconify-opaque t))
                    (iconify-window w)) tabs)
          (setq all-wins wins)
          (setq current-win win))
        (setq tab-refresh-lock t))))

  (define (after-move-resize win)
    "Add all tabs to the tabgroup from win. (Releas and iconify by before-move-resize)"
    (setq tab-refresh-lock nil)
    (let* ((wins all-wins)
	   (default-window-animator 'none)
           (pos (window-position win))
           (dim (window-dimensions win))
           index index-old)
      (when (and (cdr wins)
                 (eq win current-win))
        (setq index (tab-window-group-index (car wins)))
        (mapcar (lambda (w)
                  (when (not (eq index (tab-window-group-index w)))
                    (setq index-old (tab-window-group-index w))
                    (tab-put-window-in-group w index)
                    (tab-delete-window-from-group w index-old))
                  (move-window-to w (car pos) (cdr pos))
                  (resize-window-to w (car dim) (cdr dim))
                  (uniconify-window w)
                  (when (window-get w 'never-iconify-opaque)
                    (window-put w 'never-iconify-opaque nil)
                    (window-put w 'never-iconify t))
                  (window-put w 'tabbed t)) wins)
        (raise-window win)
        (setq all-wins nil)
        (setq current-win nil))
      (setq tab-refresh-lock t)
      (when (window-tabbed-p win) 
        (tab-refresh-group win 'move)
        (tab-refresh-group win 'frame))))

  (define (unmap-id win)
    (setq last-unmap-id (window-id win)))

  (define (in-tab-group win)
    "Add a new window as tab if have one (the first created if more as one) 
of the windows the same 'tab-group property"
    (when (window-get win 'tab-group)
      (setq in-tab-group-name (append in-tab-group-name (cons (cons (window-id win) (window-get win 'tab-group)))))
      (let ((open-win-tabgroup (get-window-by-id (car (rassoc (window-get win 'tab-group) in-tab-group-name)))))
        (if (and open-win-tabgroup
                 (not (eq win open-win-tabgroup)))
            (tab-group-window win open-win-tabgroup)))))

  (define (remove-from-tab-group win)
    "Remove window from in-tab-group-name alist if it have a 'tab-group property"
    (when (window-get win 'tab-group)
      (setq in-tab-group-name (remove (assoc last-unmap-id in-tab-group-name) in-tab-group-name))))

  (unless batch-mode
    (add-hook 'after-add-window-hook in-tab-group)
    (add-hook 'unmap-notify-hook unmap-id)
    (add-hook 'window-state-change-hook
              (lambda (win args)
                (when (window-tabbed-p win)
                  (setq args (car args))
                  (cond ((eq 'sticky args)
                         (tab-group-sticky win)
                         (tab-refresh-group win 'frame))
                        ((eq 'fixed-position args)
                         (tab-refresh-group win 'fixed-position)
                         (tab-refresh-group win 'frame))
                        ((eq 'frame-style args)
                         (tab-refresh-group win 'style)
                         (tab-refresh-group win 'reframe-all)
                         (tab-refresh-group win 'move))
                        ((eq 'type args)
                         (tab-refresh-group win 'type)
                         (tab-refresh-group win 'reframe))
                        ((eq 'stacking args)
                         (tab-refresh-group win 'depth)
                         (tab-refresh-group win 'frame))))))
    
    (add-hook 'focus-in-hook (lambda (win) (tab-group-raise win)))
    (when (eq move-outline-mode 'opaque)
      (add-hook 'before-move-hook (lambda (win) (if (window-tabbed-p win) (before-move-resize win)))))
    (add-hook 'after-move-hook  (lambda (win) (after-move-resize win)))
    (when (eq resize-outline-mode 'opaque)
      (add-hook 'before-resize-hook (lambda (win) (if (window-tabbed-p win) (before-move-resize win)))))
    (add-hook 'after-resize-hook  (lambda (win) (after-move-resize win)))
    (add-hook 'shade-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'shade))))
    (add-hook 'unshade-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'unshade))))
    (add-hook 'iconify-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'iconify))))
    (add-hook 'uniconify-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'uniconify))))
    (add-hook 'window-maximized-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'maximized))))
    (add-hook 'window-unmaximized-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'maximized))))
    (add-hook 'add-to-workspace-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'frame))))
    (add-hook 'destroy-notify-hook tab-delete-window-from-tab-groups))

  (gaol-add tab-refresh-group tab-group-window-index)
  )
