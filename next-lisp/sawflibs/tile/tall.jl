;; Description: Handles tiling of all windows on a given workspace to
;; maintain them aligned with a main pane in the left and the others
;; stacked on the right.
;;
;; Author: Jose A. Ortega Ruiz <jao@gnu.org>
;; Inspiraton: smart-tile by Mark Triggs <mst@dishevelled.net>
;;
;; Usage:
;;   (require 'sawflibs.tile.tall)
;;   (tall-tiling 3 #:width 2 #:top 0 #:bottom 1 #:gap 1 #:max 3)
;;   (tall-tiling 1 #:width 3 #:top 20 #:bottom 3 #:gap 1 #:auto #f)
;;   (bind-keys global-keymap "M-=" 'increase-max-windows)
;;   (bind-keys global-keymap "M--" 'decrease-max-windows)
;;

(define-structure sawflibs.tile.tall
    (export tall-tiling
            tall-rotate-left
            tall-rotate-right
            increase-max-windows
            decrease-max-windows)
    (open rep
	  rep.system
          sawflibs.utils
          sawflibs.tile.tiler
          sawflibs.tile.utils)

  (define (tall-tiling n #!key
                       (width 2) (top 0) (bottom 0) (gap 0) (max 2) (auto t))
    (register-workspace-tiler n
                              tall-tiler
                              (list width top bottom gap max)
                              auto
                              'tall-tiler))

  (define (tall-tiler master ignore)
    (let ((windows (workspace-windows ignore)))
      (when (> (length windows) 0)
        (let* ((master (or master (input-focus) (car windows)))
               (master (if (eq master ignore) (car windows) master))
               (children (remove master windows)))
          (do-tile master children)))))

  (define (do-tile master children)
    (let* ((lc (length children))
           (cno (if (> (setting 4) 1) (min (1- (setting 4)) lc) lc))
           (groups (reverse (group-by children cno)))
           (tm (setting 1))
           (bm (setting 2))
           (gap (setting 3))
           (m-width (master-width (setting 0)))
           (c-height (child-height cno tm bm))
           (c-width (scr-width m-width (* 3 gap) 1)))
      (mapc (lambda (g)
              (push-children (reverse g)
                             (+ m-width (* 2 gap))
                             (scr-height c-height bm)
                             (if (= (length g) cno) tm -1)
                             c-width
                             c-height))
            groups)
      (push-window master gap tm m-width (scr-height tm bm))))

  (define (master-width f) (floor (1- (/ (scr-width) f))))

  (define (child-height total tm bm)
    (floor (/ (scr-height tm bm) (if (> total 0) total 1))))

  (define (push-children children x y min-y w dy)
    (define (top-y)
      (if (and (null (cdr children)) (>= min-y 0)) min-y y))
    (when (not (null children))
      (push-window (car children) x (top-y) w dy)
      (push-children (cdr children) x (- y dy) min-y w dy)))

  (define (ws-inc-max ws delta)
    (let ((old (setting 4 #f ws)))
      (when old
        (let ((no (+ old delta)))
          (when (> no 1)
            (set-setting 4 no ws)
            no)))))

  (define (change-max-windows delta)
    (let ((n (ws-inc-max current-workspace delta)))
      (when n
        (tall-tiler nil nil)
        (notify "Maximum windows set to %s" n))))

  (define (increase-max-windows)
    (interactive)
    (change-max-windows 1))

  (define (decrease-max-windows)
    (interactive)
    (change-max-windows -1))

  (define (tall-rotate-left)
    (interactive)
    (when (eq 'tall-tiler (current-tiler-name))
      (let* ((windows (workspace-windows))
             (first (car windows))
             (rest (cdr windows))
             (master (car rest))
             (children (append (cdr rest) (list first))))
        (do-tile master children)
        (focus-window master))))

  (define (tall-rotate-right)
    (interactive)
    (when (eq 'tall-tiler (current-tiler-name))
      (let* ((windows (workspace-windows))
             (first (car windows))
             (rest (cdr windows))
             (master (last rest))
             (children (cons first (remove master rest))))
        (do-tile master children)
        (focus-window master)))))
