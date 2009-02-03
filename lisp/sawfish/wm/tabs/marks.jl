;; marks.jl - Provide a way to operate on multiple windows
;;
;; Author : Yann Hodique <Yann.Hodique@lifl.fr>

(define-structure sawfish.wm.tabs.marks

        (export mark-window
	        unmark-window
	        unmark-all-windows
		apply-on-marked-windows
		marked-windows)

	(open rep
	      rep.system
	      sawfish.wm.misc
	      sawfish.wm.custom
	      sawfish.wm.commands)

	(define-structure-alias marks sawfish.wm.tabs.marks)

  (defvar marked-windows-list nil)

  (define (mark-window win)
    (setq marked-windows-list (append marked-windows-list (list win))))

  (define-command 'mark-window mark-window #:spec "%W")

  (define (unmark-window win)
    (setq marked-windows-list (remove win marked-windows-list)))

  (define-command 'unmark-window unmark-window #:spec "%W")

  (define (unmark-all-windows)
    (setq marked-windows-list nil))

  (define-command 'unmark-all-windows unmark-all-windows)

  (define (apply-on-marked-windows func)
    (mapcar func marked-windows-list))

  (define (marked-windows)
    (not (eq marked-windows-list nil))))
