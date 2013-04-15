(define-structure sawfish.wm.tile.tiler
    (export tile-workspace
            untile-window
            register-workspace-tiler
            current-tiler-name
            tileable-windows
            next-tiling
            setting
            set-setting
	    tileable-window-p)

    (open rep
          rep.system
          rep.data.tables
          sawfish.wm.windows
          sawfish.wm.tile.utils
          sawfish.wm.state.ignored)

  (define %tilers '())
  (define %sizes (make-table eq-hash eq))

  (define (save-size w)
    (table-set %sizes w
               (list (window-x w) (window-y w)
                     (window-width w) (window-height w))))

  (define (forget w) (table-unset %sizes w))

  (define (restore-window w)
    (let ((s (table-ref %sizes w)))
      (when s
        (apply push-window (cons w s))
        t)))

  (define (restore-windows ws)
    (mapc restore-window (tileable-workspace-windows ws)))

  (define (restore-sizes ws)
    (mapc (lambda (w)
            (let ((s (table-ref %sizes w)))
              (when s
                (resize-frame-to w (nth 2 s) (nth 3 s)))))
          (tileable-workspace-windows ws)))

  (define null-tiler
    (list (lambda ()
            (restore-windows current-workspace))))

  (define (register-workspace-tiler ws tiler args auto #!optional name picker)
    (let ((curr (assoc ws %tilers))
          (new (list tiler args auto name (or picker (lambda (w) #t)))))
      (if (null curr)
          (setq %tilers (cons (list ws new null-tiler) %tilers))
        (setcdr curr (cons new (cdr curr))))))

  (define (next-tiling)
    (interactive)
    (when (rotate-tiling)
      (restore-sizes current-workspace)
      (tile-workspace)))

  (define (rotate-tiling #!optional ws)
    (let ((ts (assoc (or ws current-workspace) %tilers)))
      (when (> (length ts) 2)
        (setcdr ts `(,@(cddr ts) ,(cadr ts))))))

  (define (tilings #!optional ws)
    (cdr (assoc (or ws current-workspace) %tilers)))

  (define (tiling #!optional ws)
    (car (tilings ws)))

  (define (tiling-tiler ti) (nth 0 ti))
  (define (tiling-settings ti) (nth 1 ti))

  (define (tiling-auto-p ti w)
    (let ((p (nth 2 ti)))
      (if (functionp p) (p w) p)))

  (define (tiling-name ti) (nth 3 ti))

  (define (tiling-master-picker ti) (nth 4 ti))

  (define (tileable-windows #!optional ignore)
    (let ((ws (tileable-workspace-windows ignore))
          (tp (nth 2 (tiling))))
      (if (functionp tp) (filter tp ws) ws)))

  (define (current-tiler-name) (tiling-name (tiling)))

  (define (pick ti master ignore)
    (let ((test (tiling-master-picker ti)))
      (or (and master (not (eq master ignore)) (test master) master)
          (let ((wl (tileable-workspace-windows ignore)))
            (or (car (filter test wl)) master (car wl))))))

  (define (tile-workspace #!optional new-window destroyed-window)
    (interactive)
    (let ((ti (tiling)))
      (when ti
	((tiling-tiler ti) (pick ti new-window destroyed-window)
                           destroyed-window))))

  (define (untile-window w)
    (interactive "%f")
    (when (restore-window w) (tile-workspace nil w)))

  (define (tileable-window-p w)
    (and (tiling-auto-p (tiling (window-workspace w)) w)
         (not (window-never-tile-p w))
	 (not (window-ignored-p w))
	 (not (dock-window-p w))
         (eq (window-type w) 'default)))

  (define (add-autotile w)
    (save-size w)
    (when (tileable-window-p w)
      (tile-workspace w)))

  (define (destroy-autotile w)
    (forget w)
    (when (tileable-window-p w)
      (tile-workspace nil w)))

  (define (maybe-save w)
    (let ((ct (tiling)))
      (when (or (not ct) (eq ct null-tiler))
        (save-size w))))

  (define (setting n #!optional def ws)
    (or (nth n (tiling-settings (tiling ws))) def))

  (define (set-setting n v #!optional ws)
    (let ((s (tiling-settings (tiling ws))))
      (when s (rplaca (nthcdr n s) v))))

  (add-hook 'shape-notify-hook maybe-save t)
  (add-hook 'after-move-hook maybe-save t)
  (add-hook 'after-resize-hook maybe-save t)
  (add-hook 'after-add-window-hook add-autotile t)
  (add-hook 'uniconify-window-hook add-autotile t)
  (add-hook 'iconify-window-hook destroy-autotile t)
  (add-hook 'destroy-notify-hook destroy-autotile t)
  (add-hook 'unmap-notify-hook destroy-autotile t))
