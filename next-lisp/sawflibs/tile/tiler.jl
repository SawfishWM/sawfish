(define-structure sawflibs.tile.tiler
    (export tile-workspace
            register-workspace-tiler
            current-tiler-name
            next-tiling
            setting
            set-setting)
    (open rep
          rep.system
          rep.data.tables
          sawflibs.tile.utils)

  (define %tilers '())
  (define %sizes (make-table eq-hash eq))

  (define (save-size w)
    (table-set %sizes w
               (list (window-x w) (window-y w)
                     (window-width w) (window-height w))))

  (define (forget w) (table-unset %sizes w))

  (define (restore-windows ws)
    (mapc (lambda (w)
            (let ((s (table-ref %sizes w)))
              (when s (apply push-window (cons w s)))))
          (workspace-windows ws)))

  (define (restore-sizes ws)
    (mapc (lambda (w)
            (let ((s (table-ref %sizes w)))
              (when s
                (resize-frame-to w (nth 2 s) (nth 3 s)))))
          (workspace-windows ws)))

  (define null-tiler
    (list (lambda (a b)
            (restore-windows current-workspace))))

  (define (register-workspace-tiler ws tiler args auto #!optional name)
    (let ((curr (assoc ws %tilers))
          (new (list tiler args auto name)))
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
  (define (tiling-auto-p ti) (nth 2 ti))
  (define (tiling-name ti) (nth 3 ti))

  (define (current-tiler-name) (tiling-name (tiling)))

  (define (tile-workspace #!optional new-window destroyed-window)
    (interactive)
    (let ((ti (tiling)))
      (when ti ((tiling-tiler ti) new-window destroyed-window))))

  (define (tileable-window-p w)
    (and (tiling-auto-p (tiling (window-workspace w)))
         (eq (window-type w) 'default)))

  (define (add-autotile w)
    (save-size w)
    (when (tileable-window-p w)
      (tile-workspace w)))

  (define (destroy-autotile w)
    (forget w)
    (when (tileable-window-p w)
      (tile-workspace nil w)))

  (define (maybe-save w #!optional ignored)
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
  (add-hook 'destroy-notify-hook destroy-autotile t))
