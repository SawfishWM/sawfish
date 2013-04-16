
(define-structure sawfish.wm.tile.col
    (export col-tiling
            increase-max-cols
            decrease-max-cols)
    (open rep
          rep.system
          sawfish.wm
          sawfish.wm.tile.tiler
          sawfish.wm.tile.utils)

  (define (col-tiling ws #!key
                      (top 0) (bottom 0) (cols 3) (gap 1) (auto t) (resize t))
    (register-workspace-tiler ws
                              col-tiler
                              (list cols top bottom gap resize)
			      auto
			      'col-tiler))

  (define (cols) (setting 0))
  (define (top-m) (setting 1))
  (define (bottom-m) (setting 2))
  (define (gap) (setting 3))
  (define (resize) (setting 4))

  (define (col-tiler focused deleted)
    (let ((windows (tileable-windows deleted)))
      (when (> (length windows) 0)
        (let* ((master (or focused (input-focus) (car windows)))
               (windows (cons master (delete master windows)))
               (groups (group-by windows (cols))))
          (mapc make-column groups)))))

  (define (make-column windows)
    (let* ((min-y (top-m))
           (max-y (max (scr-height (window-height (last windows)) (bottom-m))
                       min-y))
           (max-h (scr-height (top-m) (bottom-m)))
           (dy (floor (/ (- max-y min-y) (max (1- (cols)) 1))))
           (dx (floor (/ (scr-width (* (1+ (cols)) (gap))) (cols)))))
      (push-column windows (gap) min-y dx dy (gap) max-h)))

  (define (push-column ws x y dx dy g max-h)
    (when (not (null ws))
      (let* ((w (car ws))
             (wdx (if (resize) dx (window-width w)))
             (wdy (if (resize)
                      (min (window-height (car ws)) max-h)
                    (window-height w)))
             (sh (screen-width))
             (wx (if (> (+ x wdx) sh) (- sh wdx g) x)))
        (push-window (car ws) wx y wdx wdy)
        (push-column (cdr ws) (+ x dx g) (+ y dy) dx dy g max-h))))

  (define (ws-col-max ws delta)
    (let ((old (setting 0 #f ws)))
      (when old
        (let ((no (+ old delta)))
          (when (> no 1)
            (set-setting 0 no ws)
            no)))))

  (define (change-cols delta)
    (let ((n (ws-col-max current-workspace delta)))
      (when n
        (col-tiler nil nil)
        (notify "Maximum columns set to %s" n))))

  (define (increase-max-cols)
    (interactive)
    (change-cols 1))

  (define (decrease-max-cols)
    (interactive)
    (change-cols -1)))
