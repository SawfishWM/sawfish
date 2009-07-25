#|

Functions taken from sawfish-mmc

|#

(define-structure sawfish.wm.mmc.window-manipulation

  (export maximize-unframe
          maximize-reframe)

  (open rep
        rep.system
        sawfish.wm.state.maximize
        sawfish.wm.windows
        sawfish.wm.commands
        sawfish.wm.misc
	sawfish.wm.frames
	sawfish.wm.util.prompt)

  (define-structure-alias window-manipulation sawfish.wm.mmc.window-manipulation)

  (define (maximize-unframe w)
    (set-window-type w 'unframed)
    (maximize-window w))

  (define (maximize-reframe w)
    (set-window-type w 'default)
    (maximize-window w))

  (autoload 'prompt-for-string "sawfish/wm/util/prompt-extras")

  (define (rename-window w)
    (let ((new-name (prompt-for-string "new title:" (window-name w))))
      (set-x-text-property w '_NET_WM_NAME (vector new-name))))

  (define-command 'maximize-unframe maximize-unframe #:spec "%W")
  (define-command 'maximize-reframe maximize-reframe #:spec "%W")
  (define-command 'rename-window rename-window #:spec "%W"))
