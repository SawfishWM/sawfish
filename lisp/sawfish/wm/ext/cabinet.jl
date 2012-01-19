;; cabinet.jl 1.1 -- A tool for window display,search and manipulation 
;; Time-stamp: <2012-01-16 12:51:02 hqwrong>


;; Copyright (C) 2011, 2012 hqwrong <hq.wrong@gmail.com>


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

;;
;;; Commentary:

;; Note:
;;  The words "window" and "application" mean same
;;  thing,they are used interchangeablly. The word "item"
;;  means the item displayed on Cabinet, which corresponding
;;  to an application. The words "selected item" means the
;;  first item on Cabinet. The words "selected window" means
;;  the window corresponding to the selected item.

;; ** Start up
;;   Cabinet required `hqw-util.jl', it should have been published along
;; with Cabinet. To separate them apart, because hqw-util.jl's
;; independence, you may find it useful at other occasions.

;;   Put `hqw-util.jl' and `cabinet.jl' into your load directory,and 

;;   (require 'cabinet)

;;   Use command `cabinet-switch' to start Cabinet.
;; To bind it to "W-s",copy the following line to your rc file.

;; (bind-keys global-keymap "W-s" 'cabinet-switch)

;; ** Cabinet Features.
;;   Cabinet has a similar interface with iswitch,it also adds
;; some own features, like:

;; 1) Support window icons to be shown
;;    Actually,this is my main motive to write Cabinet. For
;; People are apt to distinguish from images instead of
;; text.

;; 2) Support cycling among workspaces
;;   Most times the reason you create another workspace is
;; to make a bunch of applications distinguished from
;; others. It's a bad idear to mix them together to choose
;; from. The better one is only let you have applications of
;; current workspace,and enable you to cycle among
;; workspaces.
;;   Though, Cabinet afford you a choice: To toggle between
;; workspace-mode -- to just show windows of current workspace --
;; and all-mode -- to show all the windows. When in
;; workspace mode, the tag on split line is something like
;; 2/3, which means the largest workspace id is 3, and now
;; you are in workspace of id 2. When in all mode, the tag
;; should be "ALL".
;; Notice to make you cycle through workspaces,you should
;; set workspace boundary to 'wrap-around,put this in your
;; rc file:

;; (setq workspace-boundary-mode 'wrap-around)

;; 3) Distinguish windows for display from windows for
;; manipulation.
;;   Some applications, most time you are not inclined to
;; select them via Cabinet, for you already have a hot key
;; for them or other reasons, but somehow you just want to
;; see them there: being displayed on Cabinet. You may see a
;; split line on Cabinet, the items below it are windows
;; that you just want to display. This feature makes Cabinet
;; could somehow functions like a panel.

;; 4) Use forground color instead of tag to indicate window
;; status.
;;   Because the same reason for enabling icons. Because it
;; saves a charater's space.

;; 5) Enable users to manipulate a bunch of windows.
;; Use "Super-Spc" to mark the selected window, and use
;; procedure `with-marked-wl', you can manipulate
;; them. The marks will be cleared out after you quit
;; from Cabinet. Cabinet affords a procedure to tile marked
;; windows horizontally .

;; 6) Item format
;;   Item format is  like this :
;;       icon + window name + <window class> + viewport id + workspace id
;;   Variable `cabinet-item-text' controls what to append to icon.
;;   You are welcomed to reset it.

;; ** Cabinet Keymap
;;   I made the default keymap(cabinet-keymap) according to my
;; favour, So it's recommended to create yours. The easiest
;; way to get started will be to copy the default keymap
;; binding from the cabinet.jl, and modify it.
;; Most key bindings are obvious, while some need extra explanation.
;; Here is a list:
;;           "Super-v"    toggle workspace-mode and all-mode
;;           "Super-s"    cycle windows
;;           "Super-r"    reversely cycle windows
;;           "Super-S"     cycle workspaces.(Notice 'S' is capital)
;;           "Super-R"     reversely cycle workspaces.
;;           "Super-z"     toggle iconify property of selected window.
;;           "Super-SPC"   mark selected window
;;           "Super-x"     exchange windows-for-display and windows-for-manipulation
;;           "Super-g"    Same with "RET",but will restore stack order.
;;           "Super-t"   tile horizontally marked windows
;;           "Super-T"   tile vertically marked windows

;; ** For ones who want to extend cabinet keymap
;; You may notice, in keymap definition, there are a bunch of
;; procedures like `with-draw', `with-new-wlist' and so on. Yes,
;; it's comfusing and ugly, but it's flexible, it enables
;; you to extend cabinet-keymap at will. Anyway,I've tried
;; to make doc string clear to alleviate you aversion.:-)

;; ** Version list
;; Cabinet 1.0 2011.11
;; Cabinet 1.1 2011.12.16  fixed a bug

;; Enjoy!!

(define-structure sawfish.wm.ext.cabinet 
  
      (export cabinet-switch
              ;; 
              cabinet-keymap
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
            sawfish.wm.state.shading
            sawfish.wm.state.iconify
            sawfish.wm.stacking
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
            sawfish.wm.workspace
	    sawfish.wm.util.events
	    sawfish.wm.ext.expose)

   ;; (mapc require '( rep
   ;;       rep.system
   ;;       rep.regexp
   ;;       sawfish.wm.state.iconify
   ;;       sawfish.wm.stacking
   ;;       sawfish.wm.viewport
   ;;       sawfish.wm.state.maximize
   ;;       sawfish.wm.custom
   ;;       sawfish.wm.commands
   ;;       sawfish.wm.colors
   ;;       sawfish.wm.events
   ;;       sawfish.wm.fonts
   ;;       sawfish.wm.images
   ;;       sawfish.wm.misc
   ;;       sawfish.wm.util.x
   ;;       sawfish.wm.windows
   ;;       sawfish.wm.workspace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variables welcome to change ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (defgroup cabinet "Cabinet"
      :group focus
      :require cabinet)

   (defcustom cabinet-x-margin 10
      "The horizontal margin of cabinet"
      :type number
      :group (focus cabinet))
   (defcustom cabinet-y-margin 10
      "The vertically margin of cabinet"
      :group (focus cabinet)
      :type number)
   (defcustom cabinet-item-margin 6
      " The margin between item and item."
      :group (focus cabinet)
      :type number)
   (defcustom cycle-as-start
      t
      "whether cycle overhead,when start cabinet-switch.
Just like x-cycle"
      :group (focus cabinet)
      :type boolean)

   (defcustom workspace-mode-on-initial
      t
      "whether to show windows in current workspace.
nil, means to show windows of all workspaces."
      :group (focus cabinet)
      :type boolean)
   
   (defcustom cabinet-input-line-margin
      25
      "The margin between input line and the first item"
      :group (focus cabinet)
      :type number)

   (defvar cabinet-icon-size '(32 . 32)
      "Each icon size of window shown on cabinet"
      )
   (defvar cabinet-input-forground (get-color "yellow")
      "Forground color of your input" )

   (defvar cabinet-split-line-forground (get-color "firebrick"))

   (defvar iconified-item-forground (get-color "gray"))

   (defvar default-item-forground (get-color "white"))

   (defvar maximized-item-forground (get-color "orange"))

   (defvar shaded-item-forground (get-color "blue"))

   (defvar cabinet-background (get-color "black"))

   

   (defvar cabinet-filter-out-for-manipulation-list
      '(
        "^Conky$"
        )
      "The matching windows are only for display.
You can see their items below split line.")

   (defvar cabinet-filter-out-for-display-list
      '(
        "^\.gnome-desktop$"
        "Gnome-panel"
        "^gmc$"
        "^panel$")
   
      "The matching windows are filtered out,they
will not be shown on cabinet.")

   (defvar manipulation-on-selected
      raise-window
      "what to do on selected window, each time draw/refresh
   Cabinet. 
It must be set to a procedure with one argument. ")
   
   (define (cabinet-item-text w)
      "The text part shown on cabinet item"
      (concat (when (window-get w 'cabinet-marked)
                 " **")
              "  "
              (window-name w)
              " <"
              (window-class-name w 1)
              ">"
              (format nil
                      " %s"
                      (window-viewport w)
                      )
              (format nil
                      " %s"
                      (window-get w 'workspaces))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End @ usr variables ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (define input-line-height (font-height default-font))
   (define cabinet-font-height input-line-height)

   (define wlist-for-manipulation          
      nil)

   (define wlist-for-display
      nil)

   (define wlist-for-cabinet
      nil)

   (define cabinet-window nil)

   (define cabinet-orin-wlist)
   (define cabinet-orin-focus)
   (define cabinet-input "")
   (define cabinet-orin-workspace)
   (define workspace-mode)
   (define cabinet-filter-for-display-func)
   (define cabinet-filter-for-manipulation-func)

   ;; item-format package
   (define (cabinet-item-format w)
      (define (cabinet-item-text-format w)
         (cons (cabinet-item-text w)
               (cond
                ((window-iconified-p w) iconified-item-forground)
                ((window-maximized-p w) maximized-item-forground)
                ((window-shaded-p w) shaded-item-forground )
                (t  default-item-forground)))
         )

      (let (
            (item (cons (window-icon-image w)
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
      (make-item nil
                 (format nil 
                         "===================%s========================="
                         (if workspace-mode
                               (format nil
                                       " %s/%s "
                                       current-workspace
                                       (cdr (workspace-limits)))
                            "ALL"))
                 cabinet-split-line-forground))

   (define (draw-items wl input-line)
      (when (x-window-p cabinet-window)
         (x-destroy-window cabinet-window)
         (setq cabinet-window nil))

      (let* (
             (wl-size (length wl))

             (cabinet-items wl)

             (item-height (max (cdr cabinet-icon-size)
                               cabinet-font-height))
             (wx (+ (* 2 cabinet-x-margin)
                    (car cabinet-icon-size)
                    (apply max
                           (mapcar text-width
                                   (cons input-line
                                         (mapcar get-item-text
                                                 cabinet-items))))))

             (wy (+ (* 2 cabinet-y-margin)
                    (* wl-size item-height)
                    (* (- wl-size 1) cabinet-item-margin)
                    cabinet-input-line-margin
                    input-line-height))
             (win-size (cons wx wy)))
         (define (event-handler type xw)
            (define (cabinet-draw-text str pos foreground)
               (let ((gc (x-create-gc xw
                                      `((foreground . ,foreground)))))
                  (x-draw-string xw gc pos str)
                  ;; (x-destroy-gc gc)
                  ))
            (x-clear-window wx)
            ;; draw input line
            (cabinet-draw-text input-line
                               (cons cabinet-x-margin (+ cabinet-y-margin
                                                         cabinet-font-height))
                               cabinet-input-forground)
         
            ;; draw item text
            (let ((item-count 0)
                  (item-height-with-margin (+ item-height
                                              cabinet-item-margin))
                  (input-height-with-margin (+ input-line-height
                                               cabinet-input-line-margin)))
               (mapc (lambda (item)
                     
                        (let* (
                               (icon (get-item-icon item))
                               (text (get-item-text item))
                               (item-forground (get-item-forground item))
                               (icon-x (+ cabinet-x-margin))
                               (icon-y (+ cabinet-y-margin
                                          input-height-with-margin
                                          (* item-count
                                             item-height-with-margin)))
                               (text-x (+ icon-x
                                          (car cabinet-icon-size)))
                               (text-y (+ icon-y
                                          (quotient cabinet-font-height 2)
                                          (quotient (cdr cabinet-icon-size)
                                                    3))))
                           (when icon
                              (x-draw-image icon xw
                                            (cons icon-x icon-y)
                                            cabinet-icon-size))
                           (cabinet-draw-text text
                                              (cons text-x text-y)
                                              item-forground
                                              ))
                        (setq item-count (+ 1 item-count)))
                     cabinet-items))) ;; end of event-handler

         (setq cabinet-window
               (x-create-window `(,(- (quotient (screen-width) 2)
                                      (quotient wx 2))
                                  .
                                  ,(- (quotient (screen-height) 2)
                                      (quotient wy 2)))
                                win-size
                                1
                                `(
                                  (background . ,cabinet-background)
                                  )
                                event-handler
                                ))
         (x-map-window cabinet-window)
         cabinet-window))

   ;; keymap package
   (define (cabinet-switch-to-next)
      (when (not (null wlist-for-manipulation))
         (setq wlist-for-manipulation
               (append (cdr wlist-for-manipulation)
                       (list (car wlist-for-manipulation))))))

   (define (cabinet-switch-to-previous)
      (when (not (null wlist-for-manipulation))
         (setq wlist-for-manipulation
               (let ((rl (reverse wlist-for-manipulation)))
                  (list* (car rl) (reverse (cdr rl)))))))

   (define (toggle-iconify)
      (let ((w (car wlist-for-manipulation)))
         (if (window-get w 'iconified)
               (uniconify-window w)
            (iconify-window w))))

   (define (erase-input)
      (setq cabinet-input ""))

   (define (with-draw cmd)  
      "If your cmd will change Cabinet's
apperances,then add it."
      (lambda ()
         (cmd)
         (draw-cabinet)))

   (define (with-new-wlist cmd)
      " If your cmd makes the number of items 
grow, then add it"
      (lambda ()
         (cmd)
         (setq wlist-for-manipulation
               (filter cabinet-filter-for-manipulation-func
                       (filter cabinet-filter-for-display-func
                               (stacking-order))))
         (setq wlist-for-display
               (filter (lambda (win)
                          (not (cabinet-filter-for-manipulation-func win)))
                       (filter cabinet-filter-for-display-func
                               (stacking-order))))))

   (define (with-update-on-input cmd)
      " It will filter items  on input after
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
                                            (window-class-name w ))
                                           0 t))
                          wlist-for-manipulation))))

   (define (with-exit-and-clear-mark cmd)
      " Use it, when you decide to quit Cabinet after
your cmd's execution."
      (lambda ()
         (cmd)
         (mapc (lambda (w)
                  (window-remprop w 'cabinet-marked))
               cabinet-orin-wlist)
         (event-exit-wait)))

   (define (with-marked-wl cmd)
      "The marked window will be passed to cmd as a list."
      (lambda ()
         (cmd (filter-windows (lambda (w)
                                 (window-get w
                                             'cabinet-marked))))))
   (define (with-selected cmd)
      "The selected window will be passed to cmd as an argument."
      (lambda ()
         (cmd (car wlist-for-manipulation))))

   (define (cycle-workspace count)
      (with-draw
       (lambda ()
          ((with-update-on-input (with-new-wlist (lambda () t))))
          (if workspace-mode
                (next-workspace count)
             (setq workspace-mode t))
          (update-on-workspace))))

   (define (with-update-on-workspace cmd)
      "It filters items for
current workspace windows.
add it, when your command changed workspace"
      (lambda ()
         (cmd)
         (update-on-workspace)))

   (define (update-on-workspace)
      " Same with `with-update-on-workspace',except that
this is a procedure with no argument."
      (when (eq workspace-mode t)
         (setq wlist-for-manipulation
               (filter (lambda (w)
                          (let (
                                (ww (window-get w 'workspaces)))
                             (or (memq current-workspace ww)
                                 (eq ww nil)))) 
                       wlist-for-manipulation))))

   (define cabinet-keymap
      (bind-keys (make-keymap)
                 "C-u" (with-draw
                        (with-update-on-input
                         (with-update-on-workspace
                          (with-new-wlist erase-input))))

                 "SPC" (with-draw
                        (with-update-on-input
                         (lambda ()
                            (setq cabinet-input
                                  (concat cabinet-input " ")))))
                 "BS" (with-draw
                       (with-update-on-input
                        (with-update-on-workspace
                         (with-new-wlist
                          (lambda ()
                             (when (> (length cabinet-input) 0)
                                (setq cabinet-input
                                      (substring cabinet-input
                                                 0
                                                 (1- (length cabinet-input))))
                                ))))))
              
                 "Super-s" (with-draw cabinet-switch-to-next)
                 "C-s" (with-draw cabinet-switch-to-next)
                 "Super-r" (with-draw cabinet-switch-to-previous)
                 "C-r" (with-draw cabinet-switch-to-previous)
                 "Super-w" (with-selected
                            (lambda (w)
                               (delete-window w)
                               (setq wlist-for-manipulation (cdr wlist-for-manipulation))
                               (draw-cabinet)))
                 "Super-z" (with-draw toggle-iconify)
                 "Super-C-z" (with-draw (lambda ()
                                           (toggle-iconify)
                                           (cabinet-switch-to-next)))
                 "RET" (with-exit-and-clear-mark
                        (with-selected
                         (lambda (w)
                           (display-window w))))
                 "Super-g" (with-exit-and-clear-mark (lambda ()
                                                        (select-workspace cabinet-orin-workspace)
                                                        (restack-windows cabinet-orin-wlist)
                                                        (set-input-focus cabinet-orin-focus)))
                 "Super-x" (with-draw
                            (with-update-on-input
                             (lambda ()
                                (let (
                                      (tmp wlist-for-manipulation))
                                   (setq wlist-for-manipulation wlist-for-display
                                         wlist-for-display tmp)))))
                 "Super-SPC" (with-draw
                              (with-selected
                               (lambda (w)
                                  (window-put w
                                              'cabinet-marked
                                              (not
						(window-get w 'cabinet-marked))))))
                 "Super-t" (with-marked-wl
                            (lambda (wl)
                               (expose-windows wl)))
                 "Super-T" (with-marked-wl
                            (lambda (wl)
                               (expose-windows wl t)))

                 "Super-S" (cycle-workspace 1)
                 "Super-R" (cycle-workspace -1)
                 "Super-v" (with-draw
                            (lambda ()
                               ((with-update-on-input
                                 (with-new-wlist
                                  (lambda () t))))
                               (setq workspace-mode (not workspace-mode))))
                ;; "Right" (with-draw
                 ;;          (with-selected
                 ;;           (lambda (w)
                 ;;              (send-to-next-workspace w 1))))
                 ;; "Left" (with-draw
                 ;;         (with-selected
                 ;;          (lambda (w)
                 ;;             (send-to-next-workspace w -1))))
                 ))

   ;; end @ keymap package

   (define (draw-cabinet)
      (let (
            (selected-w (car wlist-for-manipulation)))
         (when (not (null wlist-for-manipulation))
            (manipulation-on-selected selected-w))

         (draw-items (append (mapcar cabinet-item-format wlist-for-manipulation)
                             (list (cabinet-split-line))
                             (mapcar cabinet-item-format wlist-for-display))
                     (concat cabinet-input "_"))
         ))
         
   (defvar cabinet-handler
      `(
        (,(lambda (key) (= 1 (length key)))
         .
         ,(lambda (key)
             (setq cabinet-input (concat cabinet-input key))
             (cabinet-update-on-input)
             (draw-cabinet)))))

   (define (cabinet-switch)
      "The Main function."

      (setq cabinet-orin-wlist (stacking-order)
            cabinet-orin-focus (input-focus)

            cabinet-input ""
            cabinet-orin-workspace current-workspace
            workspace-mode workspace-mode-on-initial
            cabinet-filter-for-display-func
            (cabinet-filter cabinet-filter-out-for-display-list)
            cabinet-filter-for-manipulation-func
            (cabinet-filter cabinet-filter-out-for-manipulation-list)
            wlist-for-cabinet (filter cabinet-filter-for-display-func
                                      (stacking-order))

            wlist-for-manipulation (filter cabinet-filter-for-manipulation-func
                                           wlist-for-cabinet)
            wlist-for-display (filter (lambda (win)
                                         (not (cabinet-filter-for-manipulation-func win)))
                                      wlist-for-cabinet)
            )
    
  
      (when workspace-mode
         (update-on-workspace))
   
      (when cycle-as-start
         (cabinet-switch-to-next))
  
      (draw-cabinet)

      (event-wait-for #:keymap cabinet-keymap
                    #:handler cabinet-handler
                    #:loop-on-unbound t)
      (x-destroy-window cabinet-window))
   (define-command 'cabinet-switch cabinet-switch)
   )
