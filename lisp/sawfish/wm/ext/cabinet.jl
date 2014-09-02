;; cabinet.jl

;; Copyright (C) 2011, 2012 hqwrong <hq.wrong@gmail.com>

;; This file is part of sawfish.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.


;; XXX Without this, this key-commands not run in cabinet, but i have no idea why.
;; XXX
;; XXX maximize-window-toggle maximize-window-fullscreen-toggle
;; XXX maximize-window-horizontally-toggle maximize-window-vertically-toggle
;; XXX maximize-window-fullxinerama-toggle maximize-fill-window
;; XXX maximize-fill-window-horizontally maximize-fill-window-vertically
;; XXX tab-release-window tab-move-to-end tab-move-to-beginning
;; XXX tab-move-to-right tab-move-to-left
;; XXX window-history-save-position window-history-save-dimensions
;; XXX window-history-save-attributes window-history-forget
(require 'sawfish.wm.heads)
(require 'sawfish.wm.state.maximize)
(require 'sawfish.wm.tabs.tabgroup)
(require 'sawfish.wm.ext.window-history)

(define-structure sawfish.wm.ext.cabinet
  
    (export cabinet-switch
            ;; 
            cabinet-keymap-complete
            cabinet-item-text
            ;; you'll need following procedures only when
            ;; you want to extend cabinet-keymap
            with-update-on-workspace
            with-selected
            with-marked-wl
            with-exit-and-clear-mark
            with-update-on-input
            with-new-wlist
            with-draw)
    
    (open rep
          rep.data
          rep.system
          rep.regexp
          rep.lang.math
          rep.io.timers
          sawfish.wm.util.events
          sawfish.wm.prg.compton
          sawfish.wm.tabs.tabgroup
          sawfish.wm.state.shading
          sawfish.wm.state.iconify
          sawfish.wm.stacking
          sawfish.wm.util.workarea
          sawfish.wm.viewport
          sawfish.wm.state.maximize
          sawfish.wm.util.display-window
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.colors
          sawfish.wm.events
          sawfish.wm.fonts
          sawfish.wm.images
          sawfish.wm.misc
          sawfish.wm.util.x
          sawfish.wm.windows
          sawfish.wm.workspace)


  (defgroup cabinet "Cabinet"
    :group focus
    :require sawfish.wm.ext.cabinet)

  (defgroup filter "Filter"
    :group (focus cabinet))
  
  (defgroup bindings "Keybindings"
    :group (focus cabinet))

  (defcustom cabinet:margin (cons 10 10)
    "Margin to the border of cabinet."
    :group (focus cabinet)
    :type (pair (number 1 50 10 1) (number 1 50 10 1)))
  
  (defcustom cabinet:item-margin 6
    "Margin between items."
    :group (focus cabinet)
    :range (1 . 50)
    :type number)
  
  (defcustom cabinet:input-line-margin 25
    "Margin between input line and the first item."
    :group (focus cabinet)
    :range (1 . 100)
    :type number)
  
  (defcustom cabinet:icon-size 24
    "Icon size."
    :group (focus cabinet)
    :range (8 . 128)
    :type number)
  
  (defcustom cabinet:workspace-mode-on-initial nil
    "Display only windows from current workspace."
    :group (focus cabinet)
    :type boolean)
  
  (defcustom cabinet:display-cabinet-on-active nil
    "Display cabinet on active monitor."
    :group (focus cabinet)
    :type boolean)

  (defcustom cabinet:cycle-as-start nil
    "Cycle overhead when start cabinet-switch, like x-cycle."
    :group (focus cabinet)
    :type boolean)
  
  (defcustom cabinet:manipulation-on-selected 'Raise 
    "Action if browse across cabinet."
    :group (focus cabinet)
    :type (choice Raise Focus None))
  
  (defcustom cabinet:raise-window-timeout 200
    "Delay in milliseconds until a window gets raised if browse across cabinet. \\bottom"
    :group (focus cabinet)
    :type (range (1 . 1000)))

  (defcustom cabinet:command-repeat-timeout 100
    "Delay in milliseconds until a window keybinding gets repeats. \\bottom"
    :group (focus cabinet)
    :type (range (100 . 1000)))

  (defcustom cabinet:default-item-forground (get-color "#FFFFFF")
    "Font color."
    :group (focus cabinet)
    :type color)
  
  (defcustom cabinet:background (get-color "#000000")
    "Background color."
    :group (focus cabinet)
    :type color)
  
  (defcustom cabinet:forground-input (get-color "#FFFF00")
    "Input text color."
    :group (focus cabinet)
    :type color)
  
  (defcustom cabinet:split-line-forground (get-color "#B22222")
    "Split line color."
    :group (focus cabinet)
    :type color)
  
  (defcustom cabinet:maximized-item-forground (get-color "#FFA500")
    "Maximized windows color."
    :group (focus cabinet)
    :type color)
  
  (defcustom cabinet:iconified-item-forground (get-color "#6D6D6D")
    "Iconified windows color."
    :group (focus cabinet)
    :type color)
  
  (defcustom cabinet:shaded-item-forground (get-color "#0000FF")
    "Shaded windows color."
    :group (focus cabinet)
    :type color)
  
  (defcustom cabinet:filter-out-for-manipulation (list "^Conky$" "^Gkrellm$")
    nil
    :group (focus cabinet filter)
    :widget-flags (expand-horizontally expand-vertically)
    :type* `(list string ,(_ "Windows displayed below the split line.")))
  
  (defcustom cabinet:filter-out-from-display (list "^Gdesklets-daemon$" "^Xfce4-panel$" "^Mate-panel$"
                                                   "^Xfdesktop$" "^x-caja-desktop$" "^plasma-desktop$"
						   "^Pancake$" "^lxpanel" "Sawfishpager")
    nil
    :group (focus cabinet filter)
    :widget-flags (expand-horizontally expand-vertically)
    :type* `(list string ,(_ "Windows removed from cabinet.")))
  
  (defcustom cabinet:cycle "Super-TAB"
    "Cycle through windows."
    :type event
    :after-set (lambda () (search-bad-key))
    :group (focus cabinet bindings))

  (defcustom cabinet:reversely-cycle "C-TAB"
    "Reversely cycle through windows."
    :type event
    :after-set (lambda () (search-bad-key))
    :group (focus cabinet bindings))

  (defcustom cabinet:restore-order "ESC"
    "Exit with restore stack order."
    :type event
    :after-set (lambda () (search-bad-key))
    :group (focus cabinet bindings))

  (defcustom cabinet:toggle-workspace-mode "Super-a"
    "Toggle windows from current or all workspaces."
    :type event
    :after-set (lambda () (search-bad-key))
    :group (focus cabinet bindings))

  (defcustom cabinet:exchange-manipulation "Super-q"
    "Toggle windows for display and for manipulation."
    :type event
    :after-set (lambda () (search-bad-key))
    :group (focus cabinet bindings))

  (defcustom cabinet:cycle-workspaces "Super-x"
    "Cycle through workspaces."
    :type event
    :after-set (lambda () (search-bad-key))
    :group (focus cabinet bindings))

  (defcustom cabinet:cycle-reversely-workspaces "Super-y"
    "Reversely cycle through workspaces."
    :type event
    :after-set (lambda () (search-bad-key))
    :group (focus cabinet bindings))

  (defcustom cabinet:advance-user-keybindings nil
    "Advance user keymap."
    :group (focus cabinet bindings)
    :after-set (lambda () (search-bad-key))
    :type boolean)

  (defcustom cabinet:mark-selected "Super-SPC"
    "Marked window as selected."
    :type event
    :after-set (lambda () (search-bad-key))
    :depends cabinet:advance-user-keybindings
    :group (focus cabinet bindings))

  (defcustom cabinet:advance-keybindings '(("Super-d" delete-window t none)
                                           ("Super-i" toggle-window-iconified t none)
                                           ("Super-m" maximize-window-toggle t none)
                                           ("Super-s" toggle-window-shaded t none)
                                           ("Super-f" maximize-window-fullscreen-toggle t none)
                                           ("Super-t" tab-add-maked-windows t none)
                                           ("C-d" delete-window t close)
                                           ("C-i" toggle-window-iconified t close)
                                           ("C-m" maximize-window-toggle t close)
                                           ("C-s" toggle-window-shaded t close)
                                           ("C-f" maximize-window-fullscreen-toggle t close)
                                           ("C-t" tab-add-maked-windows t close)
                                           ("M-d" delete-window t go-to-next)
                                           ("M-i" toggle-window-iconified t go-to-next)
                                           ("M-m" maximize-window-toggle t go-to-next)
                                           ("M-s" toggle-window-shaded t go-to-next)
                                           ("M-f" maximize-window-fullscreen-toggle t go-to-next)
                                           ("M-t" tab-add-maked-windows t go-to-next))
    nil
    :type* `(alist (event ,(_ "Key:"))
                   ((h-and (symbol 
                            delete-window maximize-window-toggle toggle-window-iconified toggle-window-shaded toggle-window-sticky
                            maximize-window-fullscreen-toggle maximize-window-horizontally-toggle maximize-window-vertically-toggle maximize-window-fullxinerama-toggle
                            tab-add-maked-windows tab-release-window tab-move-to-end tab-move-to-beginning tab-move-to-right tab-move-to-left
                            move-window-to-center move-window-to-top-left move-window-to-next-workspace move-window-to-previous-workspace
                            toggle-window-fixed-position toggle-window-fixed-size  toggle-window-never-focus toggle-window-ignored toggle-task-list-skip toggle-window-cycle-skip
                            toggle-window-list-skip toggle-desktop send-window-to-next-head send-window-to-previous-head
                            raise-window lower-window raise-window-depth lower-window-depth raise-lower-window
                            raise-window-and-transients raise-lower-window-and-transients delete-window-safely destroy-window
                            window-history-save-position window-history-save-dimensions window-history-save-attributes window-history-forget  
                            iconify-window uniconify-window shade-window unshade-window make-window-sticky make-window-unsticky
                            make-window-ignored make-window-not-ignored               
                            maximize-fill-window maximize-fill-window-horizontally maximize-fill-window-vertically 
                            ;; tab/titelbar-toggle tab/titelbar-to-top tab/titelbar-to-bottom tab/titelbar-to-right tab/titelbar-to-left 
                            ;; move-window-center move-window-up move-window-down 
                            ;; move-window-right move-window-left move-window-next move-window-previous 
                            ;; shrink-window-up shrink-window-down shrink-window-left shrink-window-right 
                            ;; yank-window-up yank-window-down yank-window-left yank-window-right
                            ;; size-window-add-column size-window-subtract-column size-window-add-row size-window-subtract-row 
                            ;; grow-window-up grow-window-down grow-window-left grow-window-right
                            ;; pack-window-up pack-window-down pack-window-right pack-window-left 
                            ;; double-window-size halve-window-size
                            ;; send-to-next-workspace send-to-previous-workspace send-to-workspace-left
                            ;; send-to-workspace-right send-to-workspace-up send-to-workspace-down
                            ;; maximize-window-fullscreen maximize-window-fullxinerama  
                            )
                           (label "After executed cabinet:")
                           (choice none close go-to-next)) ,(_ "Event")))
    :widget-flags (expand-vertically)
    :after-set (lambda () (search-bad-key))
    :depends cabinet:advance-user-keybindings
    :group (focus cabinet bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end defcustom

  (define (do-when-tabbed w do)
    "Action when W is tabbed."
    (when (window-tabbed-p w)
      (tab-refresh-group w do)
      (window-opacity w)))

  (define (window-tabbed-foreground-p w)
    "Returns t if W is a foreground tab"
    (if (window-tabbed-p w)
        (eq w (nth 0 (tab-group-windows-stacking-order w)))))

  (define (tabbgroup-is-shaded-p w)
    "Returns t if tabbgroup from W is shaded"
    (let ((shaded 't))
      (when (window-tabbed-p w)
        (mapcar (lambda (w)
                  (if (not (window-get w 'shaded)) (setq shaded nil))) (tab-group-windows w))
        shaded)))

  (define (cabinet-item-text w)
    "The text part shown on cabinet item"
    ;; if we remove a window we temporary need this if
    (if (not (numberp (window-id w)))
        "Window is gone"
      (concat (when (window-get w 'marked)
                " »»")
              (when (window-tabbed-p w)
                (if (window-tabbed-foreground-p w)
                    (if (eq cabinet:manipulation-on-selected 'None)
                        (format nil " [%s%s]" (tab-window-group-index w) '★)
                      (format nil " [%s]" (tab-window-group-index w)))
                  (format nil " [%s]" (tab-window-group-index w))))
              "  "
              (window-name w)
              "  ["
              (window-class-name w 1)
              "] "
              (when (or (> (car (window-viewport w)) 0)
                        (> (cdr (window-viewport w)) 0))
                (format nil " %s" (window-viewport w)))
              (when (window-get w 'workspaces)
                (format nil " %s" (window-get w 'workspaces)))
              ;;(when (window-sticky-p w)
              ;;  (format nil " ∘"))
              (if (and (window-maximized-vertically-p w)
                       (not (window-maximized-horizontally-p w)))
                  (format nil " ↕"))
              (if (and (window-maximized-horizontally-p w)
                       (not (window-maximized-vertically-p w)))
                  (format nil " ↔"))
              (when (window-maximized-p w)
                (if (or (tabbgroup-is-shaded-p w)
                        (and (not (window-tabbed-p w))
                             (window-shaded-p w)))
                    (format nil " ▾")))
              (when (window-get w 'marked)
                " ««"))))

  (define input-line-height (font-height default-font))
  (define cabinet-font-height input-line-height)
  (define wlist-for-manipulation nil)
  (define wlist-for-display nil)
  (define wlist-for-cabinet nil)
  (define cabinet-window nil)
  (define cabinet-orin-wlist)
  (define cabinet-orin-focus)
  (define cabinet-input "")
  (define cabinet-orin-workspace)
  (define workspace-mode)
  (define cabinet-filter-for-display-func)
  (define cabinet-filter-for-manipulation-func)
  (define cabinet-raise-timer nil)
  (define command-timer nil)
  (define bad-key nil)
  (define focus-by-tab nil)
  (define split-line-length nil)

  ;; item-format package
  (define (cabinet-item-text-format w)
    (let ((split-length (quotient (length (cabinet-item-text w)) 2)))
      (if (> split-length split-line-length)
          (if (> split-length 40)
              (setq split-line-length (- split-length 5))
            (setq split-line-length split-length))))
    (cons (cabinet-item-text w)
          (cond
           ((window-iconified-p w) cabinet:iconified-item-forground)
           ((window-maximized-p w) cabinet:maximized-item-forground)
           ((if (or (tabbgroup-is-shaded-p w)
                    (and (not (window-tabbed-p w))
                         (window-shaded-p w)))
                cabinet:shaded-item-forground))
           (t cabinet:default-item-forground))))

  (define (cabinet-item-format w)
    (let ((item (cons (if (window-icon-image w)
                          (window-icon-image w)
                        (require 'rep.io.files)
                        (if (file-exists-p (concat (car (cdr image-load-path)) "/" "cabinet-missing.png"))
                            (make-image (concat (cdr image-load-path) "cabinet-missing.png"))))
                      (cabinet-item-text-format w))))
      (lambda (tag)
        (cond
         ((eq tag 'icon) (car item))
         ((eq tag 'text) (cadr item))
         ((eq tag 'forground) (cdr (cdr item)))))))

  (define (make-item icon text forground)
    (lambda (tag)
      (cond
       ((eq tag 'icon) icon)
       ((eq tag 'text) text)
       ((eq tag 'forground) forground))))
  (define (get-item-icon item) (item 'icon))
  (define (get-item-text item) (item 'text))
  (define (get-item-forground item) (item 'forground))
  ;; end @ item format package 

  (define (cabinet-filter filter-list)
    (lambda (window)
      (let loop ((fow-nrl filter-list))
        (cond
         ((null fow-nrl) t)
         ((or (string-match (car fow-nrl) 
                            (window-name window)
                            0 nil)
              (string-match (car fow-nrl) 
                            (window-class-name window 1)
                            0 nil))
          nil)
         (t (loop (cdr fow-nrl)))))))

  (define (cabinet-split-line)
    (let ((lenght '0)
          line enil mode)
      (while (< lenght split-line-length) 
        (setq line (concat line "»")) 
        (setq enil (concat enil "«")) 
        (setq lenght (1+ lenght)))
      (setq mode (if workspace-mode
                     (format nil "%s/%s"
                             current-workspace
                             (cdr (workspace-limits)))
                   "ALL"))
      (make-item nil (format nil (concat line mode enil))
                 cabinet:split-line-forground)))

  (define (draw-items wl input-line)
    (when (x-window-p cabinet-window)
      (x-destroy-window cabinet-window)
      (setq cabinet-window nil))
    (let* ((wl-size (length wl))
           (cabinet-items wl)
           (item-height (max cabinet:icon-size
                             cabinet-font-height))
           (wx (+ (* 2 (car cabinet:margin))
                  cabinet:icon-size
                  (apply max
                         (mapcar text-width
                                 (cons input-line
                                       (mapcar get-item-text
                                               cabinet-items))))))
           (wy (+ (* 2 (cdr cabinet:margin))
                  (* wl-size item-height)
                  (* (- wl-size 1) cabinet:item-margin)
                  cabinet:input-line-margin
                  input-line-height))
           (win-size (cons wx wy)))
      (define (event-handler type xw)
        (define (cabinet-draw-text str pos foreground)
          (let ((gc (x-create-gc xw
                                 `((foreground . ,foreground)))))
            (x-draw-string xw gc pos str)
            ;; (x-destroy-gc gc)
            ))
        (x-clear-window wx type)
        ;; draw input line
        (cabinet-draw-text input-line
                           (cons (car cabinet:margin) (+ (cdr cabinet:margin)
                                                         cabinet-font-height))
                           cabinet:forground-input)
        ;; draw item text
        (let ((item-count 0)
              (item-height-with-margin (+ item-height
                                          cabinet:item-margin))
              (input-height-with-margin (+ input-line-height
                                           cabinet:input-line-margin)))
          (mapc (lambda (item)
                  (let* ((icon (get-item-icon item))
                         (text (get-item-text item))
                         (item-forground (get-item-forground item))
                         (icon-x (+ (car cabinet:margin)))
                         (icon-y (+ (cdr cabinet:margin)
                                    input-height-with-margin
                                    (* item-count
                                       item-height-with-margin)))
                         (text-x (+ icon-x
                                    cabinet:icon-size))
                         (text-y (+ icon-y
                                    (quotient cabinet-font-height 2)
                                    (quotient cabinet:icon-size
                                              3))))
                    (when icon
                      (x-draw-image icon xw
                                    (cons icon-x icon-y)
                                    (cons cabinet:icon-size cabinet:icon-size)))
                    (cabinet-draw-text text
                                       (cons text-x text-y)
                                       item-forground))
                  (setq item-count (+ 1 item-count)))
                cabinet-items))) ;; end of event-handler
      (setq cabinet-window
            (let ((head (if cabinet:display-cabinet-on-active (current-head) '0)))
              (x-create-window `(,(+ (car (head-offset head))
                                     (- (quotient (car (head-dimensions head)) 2)
                                        (quotient wx 2)))
                                 .
                                 ,(if (> (cdr win-size) (cdr (head-dimensions head)))
                                      '0
                                    (+ (cdr (head-offset head))
                                       (- (quotient (cdr (head-dimensions head)) 2)
                                          (quotient wy 2)))))
                               win-size
                               1
                               `((background . ,cabinet:background))
                               event-handler)))
      (x-map-window cabinet-window)
      cabinet-window))

  (define (display-bad-key)
    (display-message (concat "Cabinet:" "\n" "\n" bad-key "\n" "\nIs duplicate." "\nDisable cabinet.")
                     `((background . ,(get-color "Red")))))

  (define (search-bad-key)
    (setq bad-key nil)
    (let ((key-list (list cabinet:cycle cabinet:reversely-cycle cabinet:restore-order cabinet:toggle-workspace-mode
                          cabinet:exchange-manipulation cabinet:cycle-workspaces cabinet:cycle-reversely-workspaces)) key rest-list)
      (when cabinet:advance-user-keybindings
        (setq key-list (append key-list (list cabinet:mark-selected)))
        (setq key-list (append key-list 
                               (mapcar (lambda (w)
                                         (car w))
                                       cabinet:advance-keybindings))))
      (mapcar (lambda ()
                (setq rest-list (cdr key-list))
                (setq key (car key-list))
                (when (not (x-keysym-name (car (lookup-event key))))
                  (setq bad-key key)
                  (display-message (concat "Cabinet:" "\n" "\n" bad-key "\n" "\nIs not a valid x-keysym-name." "\nDisable cabinets.")
                                   `((background . ,(get-color "Red")))))
                (when (member key rest-list)
                  (setq bad-key key)
                  (display-bad-key))
                (setq key-list (cdr key-list)))
              key-list)))
  
  ;; keymap package
  (define (command-delay-timer)
    (setq command-timer
          (make-timer (lambda ()
                        (setq command-timer nil))
                      (quotient cabinet:command-repeat-timeout 1000) (mod cabinet:command-repeat-timeout 1000))))

  (define (update-cabinet-interface-add)
    (when (x-window-p cabinet-window)
      (setq wlist-for-manipulation
            (filter cabinet-filter-for-manipulation-func
                    (filter cabinet-filter-for-display-func
                            (stacking-order))))
      (setq wlist-for-display (filter (lambda (win)
                                        (not (cabinet-filter-for-manipulation-func win)))
                                      (stacking-order)))
      (if workspace-mode
          (update-on-workspace))
      (draw-cabinet)))

  (define (update-cabinet-interface-remove)
    (when (x-window-p cabinet-window)
      (let ((old-manipulation wlist-for-manipulation)
            (old-display wlist-for-display)
            new-manipulation new-display)
        (setq new-manipulation
              (filter cabinet-filter-for-manipulation-func
                      (filter cabinet-filter-for-display-func
                              (stacking-order))))
        (setq new-display (filter (lambda (win)
                                    (not (cabinet-filter-for-manipulation-func win)))
                                  (stacking-order)))
        (mapcar (lambda (w)
                  (if (not (member w new-manipulation))
                      (setq wlist-for-manipulation (remove w old-manipulation))))
                old-manipulation)
        (mapcar (lambda (w)
                  (if (not (member w new-display))
                      (setq wlist-for-manipulation (remove w old-display))))
                old-display)
        (if workspace-mode
            (update-on-workspace))
        (draw-cabinet))))

  (define (update-cabinet-interface #!optional next)
    (when (x-window-p cabinet-window)
      (when (not (null wlist-for-manipulation))
        (if (member next wlist-for-manipulation)
            (cabinet-switch-to-next)))
      (if workspace-mode
          (update-on-workspace))
      (draw-cabinet)))

  (define (cabinet-switch-to-next)
    (when cabinet-raise-timer
      (delete-timer cabinet-raise-timer)
      (setq cabinet-raise-timer nil))
    (when (not (null wlist-for-manipulation))
      (setq wlist-for-manipulation
            (append (cdr wlist-for-manipulation)
                    (list (car wlist-for-manipulation))))))
  
  (define (cabinet-switch-to-previous)
    (when cabinet-raise-timer
      (delete-timer cabinet-raise-timer)
      (setq cabinet-raise-timer nil))
    (when (not (null wlist-for-manipulation))
      (setq wlist-for-manipulation
            (let ((rl (reverse wlist-for-manipulation)))
              (list* (car rl) (reverse (cdr rl)))))))
  
  (define (erase-input)
    (setq cabinet-input ""))
  
  (define (with-draw cmd)  
    "If your cmd will change Cabinet's
apperances,then add it."
    (lambda ()
      (cmd)
      (draw-cabinet)))
  
  (define (with-new-wlist cmd)
    "If your cmd the number of items 
grow, then add it"
    (lambda ()
      (cmd)
      (setq wlist-for-manipulation
            (filter cabinet-filter-for-manipulation-func
                    (filter cabinet-filter-for-display-func
                            (stacking-order))))
      (setq wlist-for-display (filter (lambda (win)
                                  (not (cabinet-filter-for-manipulation-func win)))
                                (stacking-order)))))
  
  (define (with-update-on-input cmd)
    " It will filter items on input after
execution of cmd, add it when you feel proper."
    (lambda ()  
      (cmd)
      (cabinet-update-on-input)))
  
  (define (cabinet-update-on-input)
    "Same with `with-update-on-input',except that it's
a procedure with no argument."
    (setq wlist-for-manipulation
          (if (not (null wlist-for-manipulation))
              (filter (lambda (w)
                        (string-match cabinet-input
                                      (concat
                                       (window-name w)
                                       (window-class-name w))
                                      0 t))
                      wlist-for-manipulation))))
  
  (define (with-exit-and-clear-mark cmd)
    " Use it, when you decide to quit Cabinet after
your cmd's execution."
    (lambda ()
      (cmd)
      (mapc (lambda (w)
              (when (window-get w 'marked)
                (window-remprop w 'marked)
                (call-marked w)))
            cabinet-orin-wlist)
      (event-exit-wait)))

  (define (with-marked-wl cmd)
    "The marked windows will be passed to cmd as a list."
    (lambda ()
      (cmd (filter-windows (lambda (w)
                             (window-get w
                                         'marked))))))
  
  (define (with-selected cmd)
    "The selected window will be passed to cmd as an argument."
    (lambda ()
      (cmd (car wlist-for-manipulation))))
  
  (define (workspace-prev)
    (cycle-workspace -1))
  
  (define (workspace-next)
    (cycle-workspace 1))
  
  (define (cycle-workspace count)
    (with-draw
     (lambda ()
       ((with-update-on-input (with-new-wlist (lambda () t))))
       (if workspace-mode
           (next-workspace count)
         (setq workspace-mode t)
         (next-workspace count))
       (update-on-workspace))))
  
  (define (toggle-workspace-mode)
    (with-draw
     (lambda ()
       ((with-update-on-input (with-new-wlist (lambda () t))))
       (if workspace-mode
           (setq workspace-mode (not workspace-mode))
         (setq workspace-mode t)
         (update-on-workspace)))))

  (define (with-update-on-workspace cmd)
    "It filters items for current workspace windows.
add it, when your command changed workspace"
    (lambda ()
      (cmd)
      (update-on-workspace)))
  
  (define (update-on-workspace)
    " Same with `with-update-on-workspace',except that
this is a procedure with no argument."
    (setq wlist-for-manipulation
          (filter (lambda (w)
                    (let ((ww (window-get w 'workspaces)))
                      (or (memq current-workspace ww)
                          (eq ww nil)))) 
                  wlist-for-manipulation)))

  (define (update-on-bs cmd)
    "Update cabinets interface after backspce"
    (lambda ()
      (cmd)
      (if workspace-mode
          (update-on-workspace)
        (setq wlist-for-manipulation
              (filter cabinet-filter-for-manipulation-func
                      (filter cabinet-filter-for-display-func
                              (stacking-order)))))))

  (define (call-marked cmd)
    "Call marked hook if window marked"
    (call-window-hook 'window-state-change-hook cmd (list '(marked))))

  (define (get-last-workspace)
    (aref (nth 2 (get-x-property 'root '_NET_NUMBER_OF_DESKTOPS)) 0))
  
  (defvar move-window-to-next-workspace
    (lambda (w)
      (unless (window-sticky-p w)
        (let ((nw (+ (car (window-get w 'workspaces)) 1)))
          (if (< nw (get-last-workspace))
              (send-window-to-workspace-from-first w nw))))))
  
  (defvar move-window-to-previous-workspace
    (lambda (w)
      (unless (window-sticky-p w)
        (let ((nw (- (car (window-get w 'workspaces)) 1)))
          (if (> nw '-1)
              (send-window-to-workspace-from-first w nw))))))


  (defvar move-window-to-top-left
    (lambda (w)
    (let ((dims (window-frame-dimensions w))
          (h-off (cons 0 0))
          (screen (calculate-workarea #:window w #:head '0)))
      (move-window-to w
                      (clamp* (car h-off)
                              (car dims) (nth 0 screen) (nth 2 screen))
                      (clamp* (cdr h-off)
                              (cdr dims) (nth 1 screen) (nth 3 screen)))
      (call-window-hook 'after-move-hook w))))

  (defvar move-window-to-center
    (lambda (w)
    (let ((dims (window-frame-dimensions w))
          (h-dims (current-head-dimensions w))
          (h-off (current-head-offset w))
          (screen (calculate-workarea #:window w #:head (current-head w))))
      (move-window-to w
                      (clamp* (+ (car h-off)
                                 (quotient (- (car h-dims) (car dims)) 2))
                              (car dims) (nth 0 screen) (nth 2 screen))
                      (clamp* (+ (cdr h-off)
                                 (quotient (- (cdr h-dims) (cdr dims)) 2))
                              (cdr dims) (nth 1 screen) (nth 3 screen)))
      (call-window-hook 'after-move-hook w))))
  
  (defvar tab-add-maked-windows
    (lambda ()
      (let* ((win (car wlist-for-manipulation))
             (tabs (remove win (filter-windows (lambda (w) (window-get w 'marked))))))
        (when (car tabs)
          (require 'sawfish.wm.tabs.tab)
          (window-remprop win 'marked)
          (mapcar (lambda (w)
                    (window-remprop w 'marked)
                    (tab-window-add-to-tabgroup w)
                    (tab-window-add-to-tabgroup win))
                  tabs)
            (set-input-focus nil)))))

  (define (run-cmd-and-do cmd do)
    (when (not command-timer)
      (command-delay-timer)
      (let ((win (car wlist-for-manipulation))
            (next (car (cdr wlist-for-manipulation))))
        (if (eq cmd 'tab-add-maked-windows)
            (tab-add-maked-windows)
          (if (and (filter-windows (lambda (w) (window-get w 'marked)))
                   cabinet:advance-user-keybindings)
              (mapcar (lambda (x)
                        (cmd x))
                      (filter-windows (lambda (w) (window-get w 'marked))))
            (cmd win)))
        (when (string= do "close")
          (mapc (lambda (w)
                  (when (window-get w 'marked)
                    (window-remprop w 'marked)
                    (call-marked w)))
                cabinet-orin-wlist)
          (event-exit-wait)
          (x-destroy-window cabinet-window)
          (display-window win))
        
        (when (string= do "none")
          (mapc (lambda (w)
                  (when (window-get w 'marked)
                    (window-remprop w 'marked)
                    (call-marked w)))
                cabinet-orin-wlist)
          (update-cabinet-interface))
        
        (when (string= do "go-to-next")
          (mapc (lambda (w)
                  (when (window-get w 'marked)
                    (window-remprop w 'marked)
                    (call-marked w)))
                cabinet-orin-wlist)
          (update-cabinet-interface next)))))

  (define (cabinet-keymap-complete)
    (let ((keymap-complete))
      (if bad-key
          (bind-keys (make-keymap) nil)
        (if (and cabinet:advance-user-keybindings
                 (car cabinet:advance-keybindings))
            (setq keymap-complete (append (cabinet-keymap-base) (cabinet-keymap-advance)))
          (setq keymap-complete (cabinet-keymap-base)))
        keymap-complete)))

  (define (cabinet-keymap-advance)
    (let ((keybind-list cabinet:advance-keybindings)
          keymap-list tmp)
      (mapcar (lambda (w)
                (setq tmp (list (car w) (list run-cmd-and-do (nth 1 w) (prin1-to-string (nth 3 w)))))
                (setq keymap-list (append keymap-list (cdr (bind-keys (make-keymap) (nth 0 tmp) (nth 1 tmp))))))
              keybind-list)
      keymap-list))

  (define (cabinet-keymap-base)
    (bind-keys (make-keymap)
               "RET" (with-exit-and-clear-mark
                      (with-selected
                       (lambda (w)
                         (display-window w))))
               "KP_Enter" (with-exit-and-clear-mark
                           (with-selected
                            (lambda (w)
                              (display-window w))))
               "SPC" (with-draw
                      (with-update-on-input
                       (lambda ()
                         (setq cabinet-input
                               (concat cabinet-input " ")))))
               "BS" (with-draw
                     (with-update-on-input
                      (update-on-bs
                       (with-new-wlist
                        (lambda ()
                          (when (> (length cabinet-input) 0)
                            (setq cabinet-input
                                  (substring cabinet-input
                                             0
                                             (1- (length cabinet-input))))))))))
               cabinet:cycle (with-draw cabinet-switch-to-next)
               cabinet:reversely-cycle (with-draw cabinet-switch-to-previous)
               cabinet:restore-order (with-exit-and-clear-mark 
                                      (lambda ()
                                        (select-workspace cabinet-orin-workspace)
                                        (restack-windows cabinet-orin-wlist)
                                        (set-input-focus cabinet-orin-focus)
                                        (when (and focus-by-tab 
                                                   (numberp (window-id focus-by-tab)))
                                          (do-when-tabbed focus-by-tab 'focus)
                                          (setq focus-by-tab nil))))
               cabinet:exchange-manipulation (with-draw
                                              (with-update-on-input
                                               (lambda ()
                                                 (let ((tmp wlist-for-manipulation))
                                                   (setq wlist-for-manipulation wlist-for-display
                                                         wlist-for-display tmp)))))
               cabinet:toggle-workspace-mode (toggle-workspace-mode)
               cabinet:cycle-workspaces (workspace-next)
               cabinet:cycle-reversely-workspaces (workspace-prev)
               cabinet:mark-selected (when cabinet:advance-user-keybindings
                                       (with-draw
                                        (with-selected
                                         (lambda (w)
                                           (window-put w 'marked
                                                       (not (window-get w 'marked)))
                                           (call-marked w)))))))
  ;; end @ keymap package
  (define (draw-cabinet)
    (let ((w (car wlist-for-manipulation)))
      (when (not (null wlist-for-manipulation))
        (when (eq cabinet:manipulation-on-selected 'Raise)
          (setq cabinet-raise-timer
                (make-timer (lambda ()
                              (raise-window w)
                              (do-when-tabbed w 'raise))
                            (quotient cabinet:raise-window-timeout 1000) (mod cabinet:raise-window-timeout 1000))))
        (when (eq cabinet:manipulation-on-selected 'Focus)
          (setq cabinet-raise-timer
                (make-timer (lambda ()
                              (raise-window w)
                              (set-input-focus w))
                            (quotient cabinet:raise-window-timeout 1000) (mod cabinet:raise-window-timeout 1000))))
        (when (eq cabinet:manipulation-on-selected 'None) 
          (set-input-focus nil)))
      (setq split-line-length nil)
      (draw-items (append (mapcar cabinet-item-format wlist-for-manipulation)
                          (list (cabinet-split-line))
                          (mapcar cabinet-item-format wlist-for-display))
                  (concat cabinet-input "_"))))
  
  (defvar cabinet-handler
    `((,(lambda (key) (= 1 (length key)))
       .
       ,(lambda (key)
          (setq cabinet-input (concat cabinet-input key))
          (cabinet-update-on-input)
          (draw-cabinet)))))

  (define (unbound-check)
    (if bad-key
        (progn (display-bad-key)
               nil)
      (display-message nil)
      't))

  (define (cabinet-switch)
    "Start cabinet switch."
    (if bad-key
        (display-bad-key)
      (when (and (input-focus) 
                 (window-tabbed-p (input-focus))
                 (eq cabinet:manipulation-on-selected 'Raise))
          (set-input-focus nil)
          (setq focus-by-tab (input-focus)))
      (setq cabinet-orin-wlist (stacking-order)
            cabinet-orin-focus (input-focus)
            cabinet-input ""
            cabinet-orin-workspace current-workspace
            workspace-mode cabinet:workspace-mode-on-initial
            cabinet-filter-for-display-func
            (cabinet-filter cabinet:filter-out-from-display)
            cabinet-filter-for-manipulation-func
            (cabinet-filter cabinet:filter-out-for-manipulation)
            wlist-for-cabinet (filter cabinet-filter-for-display-func
                                      (stacking-order))
            wlist-for-manipulation (filter cabinet-filter-for-manipulation-func
                                           wlist-for-cabinet)
            wlist-for-display (filter (lambda (win)
                                        (not (cabinet-filter-for-manipulation-func win)))
                                      wlist-for-cabinet))
      (when workspace-mode
        (update-on-workspace))
      (when cabinet:cycle-as-start
        (cabinet-switch-to-next))
      (draw-cabinet)
      (event-wait-for #:keymap (cabinet-keymap-complete)
                      #:handler cabinet-handler
                      #:loop-on-unbound (unbound-check))
      (x-destroy-window cabinet-window)))

  (search-bad-key)

  (unless batch-mode
    (add-hook 'tab-group-windows-hook update-cabinet-interface)
    (add-hook 'before-add-window-hook update-cabinet-interface-add)
    (add-hook 'destroy-notify-hook update-cabinet-interface-remove))

  (define-command 'cabinet-switch cabinet-switch))
