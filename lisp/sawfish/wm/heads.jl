;; heads.jl -- Multihead support

;; Copyright (C) 2014 fuchur <flohtransporter@gmail.com>

;; This file is part of sawfish.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.


(define-structure sawfish.wm.heads

    (export send-window-to-next-head
            send-window-to-previous-head)
    
    (open rep
          rep.data
          rep.system
          sawfish.wm.misc
          sawfish.wm.windows
          sawfish.wm.commands
          sawfish.wm.state.maximize
          sawfish.wm.tabs.tabgroup)


  (define (move-win-to-head w head)
    "Move window to head"
    (let ((maximized-fullscreen (window-get w 'maximized-fullscreen))
          (maximized-vertically (window-get w 'maximized-vertically))
          (maximized-horizontally (window-get w 'maximized-horizontally))
          is-maximized)
      
      (when (window-maximized-p w)
        (unmaximize-window w)
        (setq is-maximized t))
      
      (let ((dim-x (car (window-position w)))
            (dim-y (cdr (window-position w)))
            (head-dim-x (car (current-head-dimensions w)))
            (head-dim-y (cdr (current-head-dimensions w)))
            (head-offset-x (car (current-head-offset w)))
            (head-offset-y (cdr (current-head-offset w)))
            (wins (tab-group-windows w))
            (new-head head)
            new-dim-x new-dim-y new-head-dim-x new-head-dim-y)
        
        (setq new-head-dim-x (car (head-dimensions new-head)))
        (setq new-head-dim-y (cdr (head-dimensions new-head)))
        (setq new-dim-x (+ (* (/ new-head-dim-x head-dim-x) (- dim-x head-offset-x)) (car (head-offset new-head))))
        (setq new-dim-y (+ (* (/ new-head-dim-y head-dim-y) (- dim-y head-offset-y)) (cdr (head-offset new-head))))
        
        (if (eq is-maximized t)
            (progn
              (move-window-to w new-dim-x new-dim-y)
              (if maximized-fullscreen
                  (maximize-window-fullscreen w 't)
                (if maximized-horizontally
                    (maximize-window-horizontally w))
                (if maximized-vertically
                    (maximize-window-vertically w)))
              (setq is-maximized nil))
          (mapcar (lambda (w) (move-window-to w new-dim-x new-dim-y)) wins)))))
  
  (define (send-window-to-next-head w)
    "Send window to the next head."
    (interactive "%W")
    (when (> (head-count) 1)
      (if (< (current-head w) (- (head-count) 1))
          (move-win-to-head w (+ (current-head w) 1))
        (move-win-to-head w 0))))
  
  (define (send-window-to-previous-head w)
    "Send window to the previous head."
    (interactive "%W")
    (when (> (head-count) 1)
      (if (> (current-head w) 0)
          (move-win-to-head w (- (current-head w) 1))
        (move-win-to-head w (- (head-count) 1)))))
  
  (define-command 'send-window-to-next-head send-to-next-head #:spec "%W")
  (define-command 'send-window-to-previous-head send-to-previous-head #:spec "%W")
  )
