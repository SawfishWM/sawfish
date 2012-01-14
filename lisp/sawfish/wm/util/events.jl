;; hqw-util.jl 1.0 -- utility for cabinet

;; Time-stamp: <2011-12-15 10:23:36 hqwrong>
;; Copyright (C) 2011, hqwrong <hq.wrong@gmail.com>

(define-structure sawfish.wm.util.events

    (export event-wait-for
            event-exit-wait)

    (open rep
	  rep.system
	  sawfish.wm.events
	  sawfish.wm.misc
	  sawfish.wm.windows)

  (define-structure-alias events-util sawfish.wm.util.events)

(define (event-wait-for #!key
                      (keymap '(keymap))
                      loop-on-unbound
                      handler
                      exit-hook)
   "wait-for an event.
`keymap' is used as the override keymap during `recursive-edit'
`handler' is a list of cons cell,as (predict
. procedure), when the key event has no bound in keymap, then the
key name is passed to each predict by order.Once eval to t ,then
the associated procedure is called with key as argument,then
re-enter in `recursive-edit'.Otherwise,if loop-on-unbound is nil,
exit loop.

`exit-hook' ,if setted,will be called with key's name as argument,
after exit from loop."

   (call-with-keyboard-grabbed
    (lambda ()
       (let ((override-keymap keymap)
             (re-exit (lambda ()
                         (throw 're-exit
                                (event-name (current-event)))))
             (key nil))
          (add-hook 'unbound-key-hook re-exit)
          (while (catch 'event-exit
                    (setq key
                          (catch 're-exit
                             (recursive-edit)))
                    (when handler
                       (do ((l handler (cdr handler)))
                             ((null l) t)
                          (let* ((cell (car l))
                                 (pred (car cell))
                                 (proc (cdr cell)))
                             (when (pred key)
                                (proc key)
                                (throw 'event-exit t)))))
                    (when (not loop-on-unbound)
                       (throw 'event-exit nil))
                    t)
             t)
          (when exit-hook
             (exit-hook key))))))

(define (event-exit-wait)
   (throw 'event-exit)))
