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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.tabs.tabgroup

    (export window-tabbed-p
            frame-style-tabs-support-p
            net-wm-window-type-normal-p
            tab-refresh-group
            tab-release-window
            tab-raise-left-window
            tab-raise-right-window
            tab-move-to-beginning
            tab-move-to-end
            tab-move-to-right
            tab-move-to-left
            tab-window-group-index
            tab-find-window
            tab-rank
            tab-group-window-list
            tab-group-windows
            tab-group-windows-stacking-order
            tab-group-window
            tab-move-resize-lock)

    (open rep
          rep.system
          rep.io.timers
          rep.data.records
          sawfish.wm.gaol
          sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.windows
          sawfish.wm.frames
          sawfish.wm.tabs.tab
          sawfish.wm.events
          sawfish.wm.state.iconify
          sawfish.wm.state.shading
          sawfish.wm.commands.move-resize
          sawfish.wm.stacking
          sawfish.wm.util.groups
          sawfish.wm.util.window-order
          sawfish.wm.commands.groups
          sawfish.wm.ext.auto-raise
          sawfish.wm.ext.shade-hover
          sawfish.wm.workspace)

  (define-structure-alias tabgroup sawfish.wm.tabs.tabgroup)

  (define all-wins nil)
  (define windows-stacking-order nil)
  (define oldgroup nil)
  (define tab-groups nil)
  (define tab-refresh-lock t)
  (define tab-move-resize-lock nil)
  (define destroy nil)
  (define release-window t)
  (define last-unmap-id nil)
  (define in-tab-group-name nil)
  (define tab-theme-name)
  (define tab-theme-tabbars)
  (define timer-raise nil)

  (defvar tab-group-windows-hook '()
    "Tab-group-windows-hook called when changing or creating a tabgroup.
Returning all windows in the current tabgroup")

  (define (set-tab-theme-name #!key frame-style-supported-tabs)
    (setq tab-theme-name frame-style-supported-tabs))

  (define (frame-style-tabs-support-p w)
    "Returns t if the framestyle from W supports tabs.
Also need the currect settings in the theme.jl from the theme."
    (setq tab-theme-name nil)
    (call-window-hook 'window-state-change-hook w (list '(tab-theme-name)))
    (eq (window-get w 'current-frame-style) tab-theme-name))

  (define (set-tab-theme-tabbars #!key frame-style-supported-tabbars)
    (setq tab-theme-tabbars frame-style-supported-tabbars))

  (define (frame-style-tabbars-support w)
    "Returns a list with theme name and tabbars position if the framestyle from W 
has multiple tabbars. Also need the currect settings in the theme.jl from the theme."
    (setq tab-theme-tabbars nil)
    (call-window-hook 'window-state-change-hook w (list '(tab-theme-tabbars)))
    tab-theme-tabbars)
  
  (define (net-wm-window-type-normal-p w)
    "Returns t if _NET_WM_WINDOW_TYPE by W is true or W has window-property 'force-tab"
    (or (window-get w 'force-tab)
        (if (get-x-property w '_NET_WM_WINDOW_TYPE)
            (equal (aref (nth 2 (get-x-property w '_NET_WM_WINDOW_TYPE)) 0) '_NET_WM_WINDOW_TYPE_NORMAL))))

  (define (window-tabbed-p w)
        (window-get w 'tabbed))

  (define-record-type :tab-group
    (tab-build-group p d wl)
    tab-group?
    (p tab-group-position)
    (d tab-group-dimensions)
    (wl tab-group-window-list))

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
    "Return the group containing WIN."
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
    "Return the index of the group containing WIN."
    (let loop ((index 0))
      (cond
       ((eq index (length tab-groups))
        (tab-make-new-group win)
        index)
       ((member win (tab-group-window-list (nth index tab-groups)))
        index)
       (t
        (loop (+ index 1))))))

  (define (tab-group-windows win)
    "Return the windows of the group containing WIN."
    (let* ((index (tab-window-group-index win))
           (wins (tab-group-window-list (nth index tab-groups))))
      wins))

  (define (tab-group-windows-stacking-order win)
    "Return the windows of the group containing WIN sort by stacking order."
    (let* ((tabs (tab-group-windows win))
           (all-wins (stacking-order))
           order)
      (mapcar (lambda (w)
                (if (member w tabs)
                    (setq order (append order (list w))))) all-wins)
      order))

  (define (tab-rank elem list)
    "Returns the nth position from elem (tab) in a list (tabbar)"
    (if (eq elem (car list))
        0
      (+ 1 (tab-rank elem (cdr list)))))

  (define (destroy-hook w)
    (setq destroy 't)
    (tab-delete-window-from-tab-groups w))

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
      (let ((wins (list (remove w (tab-group-windows w)))))
        (tab-delete-window-from-group w (tab-window-group-index w))
        (window-put w 'fixed-position nil)
        (tab-refresh-group oldgroup 'frame)
        (unshade-window (nth 0 (tab-group-windows-stacking-order oldgroup)))
        (window-order-push (nth 0 (tab-group-windows-stacking-order oldgroup)))
        (when (eq destroy 't)
          (set-input-focus (nth 0 (tab-group-windows-stacking-order oldgroup)))
          (setq destroy nil))
        (call-hook 'tab-group-windows-hook wins)
        (reframe-window w))))

  (define (tab-put-window-in-group win index)
    "Put window in group at given index."
    (let* ((group (nth index tab-groups)))
      (rplaca (nthcdr index tab-groups)
              (tab-build-group (tab-group-position group)
                               (tab-group-dimensions group)
                               (append (tab-group-window-list group) (list win))))
      (window-order-push win)))

  (define (tab-refresh-group win prop)
    "Refresh the entire group containing WIN according to PROP.
PROP can be one of the symbols: raise, frame, reframe, reframe-all, style, move,
resize, title-position, type, depth, shade, unshade, iconify, uniconify, maximized,
sticky, unsticky, fixed-position fixed-size."
    (when tab-refresh-lock
      (setq tab-refresh-lock nil)
      (unwind-protect
          (let* ((index (tab-window-group-index win))
                 (wins (tab-group-window-list (nth index tab-groups)))
                 (focus (tab-group-offset win 0))
                 (unfocus (remove focus wins))
                 not-shaded)
            (mapcar (lambda (w)
                      (if (not (window-get w 'shaded)) (setq not-shaded 't))) wins)
            (if not-shaded
                (mapcar (lambda (w)
                          (unshade-window w)) wins))
            (cond
             ((eq prop 'raise)
              (raise-windows win (remove win (tab-group-windows-stacking-order win))))
             ((eq prop 'title-position)
              (let ((group-title-position (window-get win 'title-position)))
                (mapcar (lambda (w)
                          (window-put w 'title-position group-title-position)) unfocus)
               (window-put focus 'title-position group-title-position)))
             ((eq prop 'frame)
              (mapcar (lambda (w)
                        (reframe-window w)) unfocus)
              (reframe-window focus))
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
             ((eq prop 'fixed-size)
              (let ((group-frame-fixed-size (window-get win 'fixed-size)))
                (mapcar (lambda (w)
                          (window-put w 'fixed-size group-frame-fixed-size)) unfocus)))
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
              (let ((workspace-sticky (window-sticky-p/workspace win))
                    (viewport-sticky (window-sticky-p/viewport win))
                    tab-workspace-sticky tab-viewport-sticky)
                (mapcar (lambda (w)
                          (setq tab-workspace-sticky (window-sticky-p/workspace w))
                          (setq tab-viewport-sticky (window-sticky-p/viewport w))
                          (if (not (eq workspace-sticky tab-workspace-sticky))
                              (if workspace-sticky
                                  (make-window-sticky/workspace w)
                                (make-window-unsticky/workspace w)))
                          (if (not (eq viewport-sticky tab-viewport-sticky))
                              (if viewport-sticky
                                  (make-window-sticky/viewport w)
                                (make-window-unsticky/viewport w)))) unfocus))))
            (when (cdr (tab-group-windows win))
              (if not-shaded (unshade-window win))
              (mapcar (lambda (w)
                        (shade-window w)) (remove win (tab-group-windows win)))))
        (setq tab-refresh-lock t))))

  ;; Entry points
  (define (tab-group-window w win)
    "Add window W to tabgroup containing WIN."
    ;; don't add a window as tab, if it already
    ;; exists on another workspace or window type
    ;; is not a "normal" window (e.g. dock panel ...)
    ;; and framestyle supported tabs
    (when (and (not (cdr (window-get win 'workspaces)))
               (net-wm-window-type-normal-p w)
               (net-wm-window-type-normal-p win)
               (frame-style-tabs-support-p win))
      (let* ((index (tab-window-group-index win))
             (index2 (tab-window-group-index w))
             (pos (window-position win))
             (dim (window-dimensions win))
             ;; adopt window settings for the new tab
             (group-frame-style (window-get win 'frame-style))
             (group-frame-type (window-get win 'type))
             (group-frame-shade-hover (window-get win 'shade-hover))
             (group-frame-focus-mode (window-get win 'focus-mode))
             (group-frame-gravity (window-get win 'gravity))
             (group-frame-never-iconify (window-get win 'never-iconify))
             (group-frame-fixed-position (window-get win 'fixed-position))
             (group-frame-fixed-size (window-get win 'fixed-size))
             (group-frame-title-position (window-get win 'title-position))
             (group-frame-depth (window-get win 'depth))
             (group-frame-never-maximize (window-get win 'never-maximize))
             (group-frame-maximized-vertically (window-get win 'maximized-vertically))
             (group-frame-maximized-horizontally (window-get win 'maximized-horizontally))
             (group-frame-maximized-fullscreen (window-get win 'maximized-fullscreen))
             (group-frame-unmaximized-type (window-get win 'unmaximized-type))
             (group-frame-unmaximized-geometry (window-get win 'unmaximized-geometry))
             group-frame-to-workspaces group-frame-from-workspaces is-sticky-w is-sticky-v)
        (when (not (eq index index2))
          ;; tabgroup to tabgroup
          (when (window-tabbed-p w)
            (setq release-window nil)
            (tab-delete-window-from-tab-groups w)
            (setq index2 (tab-window-group-index w)))
          (if (window-get win 'iconified) (uniconify-window win))
          (if (window-get win 'shaded) (unshade-window win))
          (setq tab-refresh-lock nil)
          (if (window-sticky-p/workspace win) (setq is-sticky-w 't))
          (if (window-sticky-p/viewport win) (setq is-sticky-v 't))
          (if (window-sticky-p win) (make-window-unsticky win))
          (setq group-frame-to-workspaces (car (window-workspaces win)))
          (if (window-sticky-p w) (make-window-unsticky w))
          (setq group-frame-from-workspaces (car (window-workspaces w)))
          (if (window-get w 'iconified) (uniconify-window w))
          (if (window-get w 'shaded) (unshade-window w))
          (window-put w 'frame-style group-frame-style)
          (window-put w 'type group-frame-type)
          (window-put w 'shade-hover group-frame-shade-hover)
          (window-put w 'focus-mode group-frame-focus-mode)
          (window-put w 'gravity group-frame-gravity)
          (window-put w 'title-position group-frame-title-position)
          (window-put w 'never-iconify group-frame-never-iconify)
          (window-put w 'depth group-frame-depth)
          (window-put w 'fixed-position group-frame-fixed-position)
          (window-put w 'fixed-size group-frame-fixed-size)
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
          (if (and is-sticky-w is-sticky-v)
              (progn 
                (make-window-sticky win)
                (make-window-sticky w))
            (when is-sticky-w 
              (make-window-sticky/workspace win)
              (make-window-sticky/workspace w))
            (when is-sticky-v 
              (make-window-sticky/viewport win)
              (make-window-sticky/viewport w)))
          (setq tab-refresh-lock t)
          (tab-refresh-group w 'frame)
          (set-input-focus w)
          (call-hook 'tab-group-windows-hook (list (tab-group-windows w)))
          (if (not (window-tabbed-p win)) (window-put win 'tabbed t))
          (window-put w 'tabbed t)
          (raise-windows w (remove w (tab-group-windows-stacking-order w)))))))
  
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

  (define (move-tab w pos)
    "Move tab W to pos"
    (when (window-tabbed-p w)
      (let* ((wins (tab-group-windows w))
            (rank (tab-rank w wins))
            (list-end (nthcdr rank wins))
            (list-start wins)
            right left current new-list)
        (mapcar (lambda (w)
                  (setq list-start (remove w list-start))) list-end)
        (when (eq pos 'next)
          (if (not (nth (+ rank 1) wins))
              (tab-move-to-beginning w)
            (setq list-end (remove w list-end))
            (setq right (nth (+ rank 1) wins))
            (setq list-end (remove (nth (+ rank 1) wins) list-end))         
            (setq current (list w))
            (if (consp list-start)
                (setq new-list (append new-list list-start)))
            (if right
                (setq new-list (append new-list (list right))))
            (if (consp current)
                (setq new-list (append new-list current)))
            (if (consp list-end)
                (setq new-list (append new-list list-end)))
            (setq all-wins (nthcdr 0 new-list))
            (after-move-resize w)))
        (when (eq pos 'prev)
          (setq left (last list-start))  
          (if (not left)
              (tab-move-to-end w)
          (setq list-start (remove left list-start))
            (setq list-end (nthcdr (+ rank 1) wins))
            (setq current (list w))
            (if (consp list-start)
                (setq new-list (append new-list list-start)))
            (if (consp current)
                (setq new-list (append new-list current)))
            (if left
                (setq new-list (append new-list (list left))))
            (if (consp list-end)
                (setq new-list (append new-list list-end)))
            (setq all-wins (nthcdr 0 new-list))
            (after-move-resize w))))))

  (define clicked-frame nil)
  (define (tab-move-to-right w)
    "Move tab to right in the tabbar."
    (setq clicked-frame (clicked-frame-part))
    (move-tab w 'next))

  (define-command 'tab-move-to-right tab-move-to-right #:spec "%f")

  (define (tab-move-to-left w)
    "Move tab to left in the tabbar."
    (setq clicked-frame (clicked-frame-part))
    (move-tab w 'prev))
  
  (define-command 'tab-move-to-left tab-move-to-left #:spec "%f")

  (define (move-tab-edge w pos)
    "Move tab W to pos"
    (when (window-tabbed-p w)
      (let ((tabs (remove w (tab-group-windows w))))
        (if (eq pos 'end)
            (setq all-wins (append tabs (cons w nil))))
        (if (eq pos 'beg)
            (setq all-wins (append (cons w nil) tabs)))
        (after-move-resize w))))

  (define (tab-move-to-end w)
    "Move tab to the end in the tabbar."
    (setq clicked-frame (clicked-frame-part))
    (move-tab-edge w 'end))

  (define-command 'tab-move-to-end tab-move-to-end #:spec "%f")

  (define (tab-move-to-beginning w)
    "Move tab to the beginning in the tabbar."
    (setq clicked-frame (clicked-frame-part))
    (move-tab-edge w 'beg))

  (define-command 'tab-move-to-beginning tab-move-to-beginning #:spec "%f")

  (define (map-other-grouped-windows win func)
    ""
    (mapcar func
            (delete-if
             (lambda (w) (eq w win))
             (tab-group-window-list (tab-find-window win)))) )

  (define (before-move win)
    (when (not (window-get win 'fixed-position))
      (before-move-resize win)))

  (define (before-move-resize win)
    "Releas WIN from the tabgroup and iconify the rest from the group."
    (let* ((default-window-animator 'none)
           (index (tab-window-group-index win))
           (wins (tab-group-windows win))
           (order (tab-group-windows-stacking-order win))
           (tabs (remove win (tab-group-windows win))))
      (setq tab-move-resize-lock 't)
      (tab-delete-window-from-group win index)
      (reframe-window win)
      (setq tab-refresh-lock nil)
      (mapcar (lambda (w)
                (when (window-get w 'never-iconify)
                  (window-put w 'never-iconify nil)
                  (window-put w 'never-iconify-opaque t))
                (iconify-window w)) tabs)
      (setq windows-stacking-order order)
      (setq all-wins wins))
    (setq tab-refresh-lock t))

  (define (after-move-resize win)
    "Add all tabs to the tabgroup from WIN. (Releas and iconify by before-move-resize)"
    (setq tab-refresh-lock nil)
    (let* ((default-window-animator 'none)
           (wins all-wins)
           (pos (window-position win))
           (dim (window-dimensions win))
           index index-old)
      (when (cdr wins)
        (setq index (tab-window-group-index (car wins)))
        (mapcar (lambda (w)
                  (setq index-old (tab-window-group-index w))
                  (tab-put-window-in-group w index)
                  (tab-delete-window-from-group w index-old)
                  (setq index (tab-window-group-index w))
                  (move-window-to w (car pos) (cdr pos))
                  (resize-window-to w (car dim) (cdr dim))
                  (uniconify-window w)
                  (when (window-get w 'never-iconify-opaque)
                    (window-put w 'never-iconify-opaque nil)
                    (window-put w 'never-iconify t))
                  (window-put w 'tabbed t)) wins)
        (call-hook 'tab-group-windows-hook (list (tab-group-windows win)))
        (raise-windows win (remove win windows-stacking-order))
        (tab-refresh-group win 'raise)
        (setq all-wins nil))
      (setq tab-refresh-lock t)
      (when (window-tabbed-p win)
        (tab-refresh-group win 'move)
        (tab-refresh-group win 'frame)
        (set-input-focus (nth 0 (tab-group-windows-stacking-order win)))
        (when clicked-frame
          (move-cursor-in-tabbar (input-focus))
          (setq clicked-frame nil)))
      (setq tab-move-resize-lock nil)))

  (define (move-cursor-in-tabbar win)
    (let* ((group (tab-find-window win))
           (tabnum (tab-rank win (tab-group-window-list group)))
           (tab-pos-list (tab-pos group tabnum win))
           (start-right
            (if (or (eq (window-get win 'type) 'transient)
                    (eq (window-get win 'type) 'shaped-transient))
                tabbar-left-margin-transient
              tabbar-left-margin)))
      (if (not (eq (window-get win 'current-frame-style) (nth 0 (frame-style-tabbars-support win))))
          (warp-cursor (+ (car (window-position win)) start-right (nth 4 tab-pos-list) (quotient (nth 6 tab-pos-list) 2))
                       (+ (quotient (- (cdr (window-frame-dimensions win)) (cdr (window-dimensions win)) 4) 2) (cdr (window-position win))))
        (let ((current-pos 
               (if (window-get win 'title-position)
                   (window-get win 'title-position)
                 (nth 1 (frame-style-tabbars-support win)))))
          (case current-pos
                ((top) (warp-cursor (+ (car (window-position win)) start-right (nth 4 tab-pos-list) (quotient (nth 6 tab-pos-list) 2))
                                    (+ (quotient (- (cdr (window-frame-dimensions win)) (cdr (window-dimensions win)) 4) 2) (cdr (window-position win)))))
                ((bottom) (warp-cursor (+ (car (window-position win)) start-right (nth 4 tab-pos-list) (quotient (nth 6 tab-pos-list) 2))
                                       (+ (quotient (- (cdr (window-frame-dimensions win)) (cdr (window-dimensions win))) 2) 
                                          (cdr (window-position win)) (cdr (window-dimensions win)))))
                ((left) (warp-cursor (+ (quotient (- (car (window-frame-dimensions win)) (car (window-dimensions win)) 4) 2) (car (window-position win)))
                                     (- (+ (cdr (window-position win)) (cdr (window-dimensions win))) start-right (nth 7 tab-pos-list) 
                                        (quotient (nth 9 tab-pos-list) 2))))
                ((right) (warp-cursor (+ (quotient (- (car (window-frame-dimensions win)) (car (window-dimensions win))) 2) 
                                         (car (window-position win)) (car (window-dimensions win)) 2) 
                                      (- (+ (cdr (window-position win)) (cdr (window-dimensions win))) start-right (nth 7 tab-pos-list) 
                                         (quotient (nth 9 tab-pos-list) 2)))))))))

  (define (unmap-id win)
    (setq last-unmap-id (window-id win)))

  (define (in-tab-group win)
    "Add a new window WIN as tab if have one (the first created if more as one)
of the windows the same 'tab-group property"
     (when (window-get win 'tab-group)
       (setq in-tab-group-name (append in-tab-group-name (cons (cons (window-id win) (window-get win 'tab-group)))))
       (let ((open-win-tabgroup (get-window-by-id (car (rassoc (window-get win 'tab-group) in-tab-group-name)))))
        ;; unmap-notify-hook gets not always a window-id for all
        ;; windows e.g. gimp (it will close more as one window and
        ;; also not all call the unmap-notify-hook and/or we get the window-id).
        ;; This next "if" will clean the list and remove the "ghosts".
        (if (not (eq open-win-tabgroup nil))
            (if (not (eq win open-win-tabgroup))
                (tab-group-window win open-win-tabgroup))
          (setq in-tab-group-name (remove (rassoc (window-get win 'tab-group) in-tab-group-name) in-tab-group-name))
          (in-tab-group win)))))

  (define (remove-from-tab-group win)
    "Remove WIN from in-tab-group-name alist if it have a 'tab-group property"
    (when (window-get win 'tab-group)
      (setq in-tab-group-name (remove (assoc last-unmap-id in-tab-group-name) in-tab-group-name))))

  (define (focus-in-tab win)
    (let ((timer-wait (if raise-windows-on-focus raise-window-timeout '1)))
      (if (or (eq focus-mode 'click)
              (eq (window-get win 'focus-mode) 'click))
          (setq timer-wait '1))
      (setq timer-raise
            (make-timer (lambda ()
                          (if (or shade-hover-mode (window-get win 'shade-hover))
                              (raise-windows win (remove win (tab-group-windows-stacking-order win)))
                            (tab-refresh-group win 'raise)))
                        (quotient timer-wait 1000) (mod timer-wait 1000)))))

  (define (focus-out-tab)
    (when timer-raise
      (delete-timer timer-raise)
      (setq timer-raise nil)))

  (define (unshade-tab win)
    (let ((unshade-nil))
      (when (window-get win 'shade-hover-unshaded)
        (mapcar (lambda (w)
                  (if (not (window-get w 'shaded)) (setq unshade-nil 't))) (remove win (tab-group-windows win)))
        (when unshade-nil
          (window-put win 'shade-hover-unshaded nil)
          (clean-up)
          (setq unshade-nil nil)
          (mapcar (lambda (w)
                    (shade-window w)) (remove win (tab-group-windows win))))
        (set-input-focus win))))

  (unless batch-mode
    (add-hook 'after-add-window-hook in-tab-group)
    (add-hook 'unmap-notify-hook unmap-id)
    (add-hook 'window-state-change-hook
              (lambda (win args)
                (when (window-tabbed-p win)
                  (setq args (car args))
                  (cond ((eq 'sticky args)
                         (tab-refresh-group win 'sticky)
                         (tab-refresh-group win 'frame))
                        ((eq 'fixed-position args)
                         (tab-refresh-group win 'fixed-position)
                         (tab-refresh-group win 'frame))
                        ((eq 'fixed-size args)
                         (tab-refresh-group win 'fixed-size)
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

    (when (eq move-outline-mode 'opaque)
      (add-hook 'before-move-hook (lambda (win) (if (window-tabbed-p win) (before-move win)))))
    (add-hook 'after-move-hook  (lambda (win) (after-move-resize win)))
    (when (eq resize-outline-mode 'opaque)
      (add-hook 'before-resize-hook (lambda (win) (if (window-tabbed-p win) (before-move-resize win)))))
    (add-hook 'after-resize-hook  (lambda (win) (after-move-resize win)))
    (add-hook 'focus-in-hook (lambda (win) (if (window-tabbed-p win) (focus-in-tab win))))
    (add-hook 'focus-out-hook (lambda (win) (if (window-tabbed-p win) (focus-out-tab))))
    (add-hook 'unshade-window-hook (lambda (win) (if (window-tabbed-p win) (unshade-tab win))))
    (add-hook 'iconify-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'iconify))))
    (add-hook 'uniconify-window-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'uniconify))))
    (add-hook 'window-maximized-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'maximized))))
    (add-hook 'window-unmaximized-hook (lambda (win) (if (window-tabbed-p win) (tab-refresh-group win 'maximized))))
    (add-hook 'destroy-notify-hook destroy-hook))

  (gaol-add set-tab-theme-name set-tab-theme-tabbars tab-refresh-group tab-group-windows))
