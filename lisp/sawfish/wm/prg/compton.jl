;; compton.jl -- compton integration

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.


(define-structure sawfish.wm.prg.compton

    (export window-opacity start-compton stop-compton)

    (open rep
          rep.system
          rep.io.processes
          rep.io.timers
          rep.util.misc
	  rep.regexp
          sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.windows
          sawfish.wm.state.maximize
          sawfish.wm.ext.match-window)

  (define-structure-alias compton sawfish.wm.prg.compton)

  (define %compton-proc nil)
  (define switch-opacity nil)
  (define update-opacity nil)
  (define stop-compton nil)
  (define timer-load-compton nil)

  (defgroup window-effects "Window Effects"
    :group appearance)

  (if (not (program-exists-p "compton"))
      (defcustom opacity-introduction nil
        "!!! Install compton and restart sawfish to use transparency in sawfish. !!!"
        :type (label " ")
        :group (appearance window-effects))

    (defcustom opacity-enable nil "Enable opacity."
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom shadows-enable t "Enabled client-side shadows on windows."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom shadows-main-win nil "Only draw shadows on normal windows and dialogs."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom shadows-disable-shaped t "Don't draw shadows on shaped windows."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom shadows-disable-dad nil "Don't draw shadows on drag-and-drop windows."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom shadows-disable-menu nil "Don't draw shadows on menus."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom force-tabbed t "Force shadows by tabbed windows."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom shadows-crop-maximized nil "Crop shadows by maximized Windows (xinerama support)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom top-offset -15 "The top offset for shadows (Default -15)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (25 20 15 10 5 0 -05 -10 -15 -20 -25 -30 -35 -40 -45 -50)
      :after-set (lambda () (switch-opacity)))

    (defcustom left-offset -15 "The left offset for shadows (Default -15)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (25 20 15 10 5 0 -05 -10 -15 -20 -25 -30 -35 -40 -45 -50)
      :after-set (lambda () (switch-opacity)))

    (defcustom translucency 60 "The translucency for shadows (Default 80)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (10 20 30 40 50 60 70 80 90 100)
      :after-set (lambda () (switch-opacity)))

    (defcustom blur-radius 12 "The blur radius for shadows (Default 12)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)
      :after-set (lambda () (switch-opacity)))

    (defcustom shadow-red 0 "Red color value of shadow (Default 0)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (0 1 2 3 4 5 6 7 8 9 10)
      :after-set (lambda () (switch-opacity)))

    (defcustom shadow-green 0 "Green color value of shadow (Default 0)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (0 1 2 3 4 5 6 7 8 9 10)
      :after-set (lambda () (switch-opacity)))

    (defcustom shadow-blue 0 "Blue color value of shadow (Default 0)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (0 1 2 3 4 5 6 7 8 9 10)
      :after-set (lambda () (switch-opacity)))

    (defcustom avoid-shadows t "Avoid drawing shadows on dock/panel windows."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom fade-enable t "Fade windows in/out when opening/closing and when opacity changes."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom fade-in 28 "Opacity change between steps while fading in (Default 28)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (1 7 14 21 28 35 42 49 56)
      :after-set (lambda () (switch-opacity)))

    (defcustom fade-out 28 "Opacity change between steps while fading out (Default 28)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (1 7 14 21 28 35 42 49 56)
      :after-set (lambda () (switch-opacity)))

    (defcustom fade-time 10 "The time between steps in a fade in milliseconds (Default 10)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (0 2 4 6 8 10 12 14 16 18 20 25 30 35 40 45 50)
      :after-set (lambda () (switch-opacity)))

    (defcustom zero-mask t "Zero the part of the shadow's mask behind the window (experimental)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type boolean
      :after-set (lambda () (switch-opacity)))

    (defcustom menu-opacity 10 "The opacity for menus (Default 10)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type symbol
      :options (5 6 7 8 9 10)
      :after-set (lambda () (switch-opacity)))

    (defcustom opacity-by-move 75 "Opacity by move (Default 75)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 75 1))

    (defcustom opacity-by-resize 75 "Opacity by resize (Default 75)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 75 1))

    (defcustom opacity-normal-i 90 "Opacity normal windows inactive (Default 90)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 90 1)
      :after-set (lambda () (update-opacity 'normal)))

    (defcustom opacity-dialog-i 85 "Opacity dialog windows inactive (Default 85)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 85 1)
      :after-set (lambda () (update-opacity 'dialog)))

    (defcustom opacity-tabbed-i 95 "Opacity tabbed windows inactive (Default 95)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 95 1)
      :after-set (lambda () (update-opacity 'tabbed)))

    (defcustom opacity-desktop-i 75 "Opacity desktop windows inactive (Default 75)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 75 1)
      :after-set (lambda () (update-opacity 'desktop)))

    (defcustom opacity-dock-a 100 "Opacity dock windows active (Default 100)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 50 100 100 1)
      :after-set (lambda () (update-opacity 'dock)))

    (defcustom opacity-dock-i 80 "Opacity dock windows inactive (Default 80)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 80 1)
      :after-set (lambda () (update-opacity 'dock)))

    (defcustom opacity-notify-i 85 "Opacity notify (Default 85)."
      :depends opacity-enable
      :group (appearance window-effects)
      :type (number 0 100 85 1)
      :after-set (lambda () (update-opacity 'notify)))

    (defcustom compton-extra-args "" "Extra arguments to compton."
      :depends opacity-enable
      :group (appearance window-effects)
      :type string)

    ;; windows custom-settings entry
    (define-match-window-property 'opacity 'appearance '(number 0 100 100))
    (define-match-window-property 'no-shadows 'appearance 'boolean)

    ;; info: compton crash with empty values 
    (define (shadows) (if shadows-enable '-c '-e1))
    (define (fade) (if fade-enable '-f '-e1))
    (define (avoid) (if avoid-shadows '-C '-e1))
    (define (zero) (if zero-mask '-z '-e1))
    (define (dad) (if shadows-disable-dad '-G '-e1))
    (define (smenu) (if shadows-disable-menu (concat "window_type *= 'menu'") (concat "window_type *= 'nil'")))

    (define (trans) (/ (+ 0.00 translucency) 100))
    (define (fade-i) (/ (+ 0.00 fade-in) 1000))
    (define (fade-o) (/ (+ 0.00 fade-out) 1000))
    (define (menu-o) (/ (+ 0.00 menu-opacity) 10))
    (define (red) (/ (+ 0.00 shadow-red) 10))
    (define (green) (/ (+ 0.00 shadow-green) 10))
    (define (blue) (/ (+ 0.00 shadow-blue) 10))

    (define (start-compton #!key
                           (c-shadows (shadows))
                           (c-fade (fade))
                           (c-avoid (avoid))
                           (c-zero (zero))
                           (c-smenu (smenu))
                           (c-dad (dad))
                           (c-trans (trans))
                           (c-fade-i (fade-i))
                           (c-fade-o (fade-o))
                           (c-menu-o (menu-o))
                           (c-red (red))
                           (c-green (green))
                           (c-blue (blue)))
      "Start compton. If a compton process already exists, it's beeing killed."
      (when (program-exists-p "compton")
        (stop-compton)
        (setq %compton-proc (make-process))
        (apply start-process %compton-proc "compton" (format nil "%s" c-shadows) (format nil "%s" c-fade) (format nil "%s" c-avoid) (format nil "%s" c-zero)
                       "-r" (number->string blur-radius) "-o" (format nil "%s" c-trans) "-l" (number->string left-offset)
                       "-t" (number->string top-offset) "-I" (format nil "%s" c-fade-i) "-O" (format nil "%s" c-fade-o) "-D" (number->string fade-time)
                       "-m" (format nil "%s" c-menu-o) "--shadow-red" (format nil "%s" c-red) "--shadow-green" (format nil "%s" c-green)
                       "--shadow-blue" (format nil "%s" c-blue) (format nil "%s" c-dad) "--detect-rounded-corners" "--shadow-exclude" (concat c-smenu)
		       "--shadow-exclude" "_COMPTON_SHADOW:32c = 0"
		       (unless (= (length compton-extra-args) 0) (string-split "\\s+" compton-extra-args)))))

    (define (stop-compton)
      "Stop compton, if running."
      (when %compton-proc (kill-process %compton-proc)
            (setq %compton-proc nil)))

    ;;0xffffffff opacity 0%
    ;;4294967294.00 opacity 0%
    ;;0x00000000 opacity 100%
    (define (get-opacity w)
      (* (/ 4294967294.00 100) w))

    (define (get-type w)
      (if (get-x-property w '_NET_WM_WINDOW_TYPE)
          (aref (nth 2 (get-x-property w '_NET_WM_WINDOW_TYPE)) 0)))

    (define (window-shadow w)
      "Set _COMPTON_SHADOW to exclude shadow for window w"
      (if (or (and (window-get w 'tabbed)
                   (not force-tabbed))
              (and (window-get w 'shaded)
                   shadows-disable-shaped)
              (and (not shadows-enable))
              (window-get w 'no-shadows)
              (and shadows-crop-maximized
                   (window-maximized-vertically-p w)
                   (window-maximized-horizontally-p w))
              (and (if shadows-main-win
                       (and (not (eq (get-type w) '_NET_WM_WINDOW_TYPE_DIALOG))
                            (not (eq (get-type w) '_NET_WM_WINDOW_TYPE_NORMAL))))))
          (set-x-property (window-frame-id w) '_COMPTON_SHADOW (make-vector 1 0) 'CARDINAL 32)
        (delete-x-property (window-frame-id w) '_COMPTON_SHADOW))
      (sync-server))

    (define (dim-window w opacity)
      "Set _NET_WM_WINDOW_OPACITY to opacity for window w"
      (window-shadow w)
      (if (eq opacity '0)
          (delete-x-property (window-frame-id w) '_NET_WM_WINDOW_OPACITY)
        (set-x-property (window-frame-id w) '_NET_WM_WINDOW_OPACITY (make-vector 1 opacity) 'CARDINAL 32))
      (sync-server))

    (define (window-opacity w)
      (if (window-get w 'opacity)
          (if (eq (input-focus) w)
              (dim-window w (get-opacity '100))
            (dim-window w (get-opacity (window-get w 'opacity))))
        (if (window-get w 'tabbed)
            (if (eq (input-focus) w)
                (dim-window w (get-opacity '100))
              (dim-window w (get-opacity opacity-tabbed-i)))
          (let ((type-is (get-type w)))
            (if (or (eq type-is '_NET_WM_WINDOW_TYPE_DIALOG)
                    (eq type-is '_NET_WM_WINDOW_TYPE_NOTIFY)
                    (eq type-is '_NET_WM_WINDOW_TYPE_DOCK)
                    (eq type-is '_NET_WM_WINDOW_TYPE_DESKTOP))
                (progn
                  (if (eq type-is '_NET_WM_WINDOW_TYPE_DIALOG)
                      (if (eq (input-focus) w)
                          (dim-window w (get-opacity '100))
                        (dim-window w (get-opacity opacity-dialog-i))))
                  (if (eq type-is '_NET_WM_WINDOW_TYPE_NOTIFY)
                      (if (eq (input-focus) w)
                          (dim-window w (get-opacity opacity-notify-i))
                        (dim-window w (get-opacity opacity-notify-i))))
                  (if (eq type-is '_NET_WM_WINDOW_TYPE_DOCK)
                      (if (eq (input-focus) w)
                          (dim-window w (get-opacity opacity-dock-a))
                        (dim-window w (get-opacity opacity-dock-i))))
                  (if (eq type-is '_NET_WM_WINDOW_TYPE_DESKTOP)
                      (if (eq (input-focus) w)
                          (dim-window w (get-opacity '100))
                        (dim-window w (get-opacity opacity-desktop-i)))))
              (if (eq (input-focus) w)
                  (dim-window w (get-opacity '100))
                (dim-window w (get-opacity opacity-normal-i))))))))

    (define (update-opacity type)
      (map-windows (lambda (w)
                     (let ((type-is (get-type w)))
                       (if (or (and (window-get w 'tabbed)
                                    (eq type 'tabbed))
                               (and (eq type-is '_NET_WM_WINDOW_TYPE_DIALOG)
                                    (eq type 'dialog))
                               (and (eq type-is '_NET_WM_WINDOW_TYPE_NOTIFY)
                                    (eq type 'notify))
                               (and (eq type-is '_NET_WM_WINDOW_TYPE_DOCK)
                                    (eq type 'dock))
                               (and (eq type-is '_NET_WM_WINDOW_TYPE_DESKTOP)
                                    (eq type 'desktop))
                               (and (not (window-get w 'tabbed))
                                    (not (eq type-is '_NET_WM_WINDOW_TYPE_DIALOG))
                                    (not (eq type-is '_NET_WM_WINDOW_TYPE_NOTIFY))
                                    (not (eq type-is '_NET_WM_WINDOW_TYPE_DOCK))
                                    (not (eq type-is '_NET_WM_WINDOW_TYPE_DESKTOP))
                                    (eq type 'normal)))
                           (window-opacity w))))))

    (define (switch-opacity)
      (when (not timer-load-compton)
        (if opacity-enable
            (start-compton)
          (stop-compton))
        (map-windows (lambda (w)
                       (if opacity-enable
                           (window-opacity w)
                         (dim-window w (get-opacity '100)))))))

    (define (max-window w)
      (if shadows-crop-maximized
          (window-shadow w)))

    (define (before-move w)
      (dim-window w (get-opacity opacity-by-move)))

    (define (before-resize w)
      (dim-window w (get-opacity opacity-by-resize)))
    
    (define (tab-release w)
      (if (and (car w)
               (not (cdr w)))
          (if opacity-enable
              (window-opacity (car w))
            (dim-window (car w) (get-opacity '100)))))

    (define (timer-compton-load)
      (setq timer-load-compton
            (make-timer (lambda ()
                          (setq timer-load-compton nil)
                          (switch-opacity))
                        (quotient 3000 1000) (mod 3000 1000))))
    
    (timer-compton-load)

    (add-hook 'window-maximized-hook (lambda (w) (if opacity-enable (max-window w))))
    (add-hook 'window-unmaximized-hook (lambda (w) (if opacity-enable (max-window w))))
    (add-hook 'tab-group-windows-hook (lambda (w) (if opacity-enable (tab-release w))))
    (add-hook 'after-add-window-hook (lambda (w) (if opacity-enable (window-opacity w))))
    (add-hook 'shade-window-hook (lambda (w) (if opacity-enable (window-opacity w))))
    (add-hook 'focus-in-hook (lambda (w) (if opacity-enable (window-opacity w))))
    (add-hook 'focus-out-hook (lambda (w) (if opacity-enable (window-opacity w))))
    (add-hook 'before-move-hook (lambda (w) (if opacity-enable (before-move w))))
    (add-hook 'after-move-hook (lambda (w) (if opacity-enable (window-opacity w))))
    (add-hook 'before-resize-hook (lambda (w) (if opacity-enable (before-resize w))))
    (add-hook 'after-resize-hook (lambda (w) (if opacity-enable (window-opacity w))))
    (add-hook 'after-initialization-hook switch-opacity)
    (add-hook 'before-exit-hook stop-compton)))
