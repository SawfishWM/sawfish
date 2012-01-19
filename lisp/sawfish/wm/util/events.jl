;; hqw-util.jl 1.0 -- A bunch of util functions for sawfish

;; Time-stamp: <2011-12-15 10:23:36 hqwrong>
;; Copyright (C) 2011, hqwrong <hq.wrong@gmail.com>

;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation.

;;
;;; Commentary:
(define-structure sawfish.wm.util.events

      (export event-wait-for
              event-exit-wait)

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
            sawfish.wm.custom
            sawfish.wm.commands
            sawfish.wm.colors
            sawfish.wm.events
            sawfish.wm.fonts
            sawfish.wm.images
            sawfish.wm.misc
            sawfish.wm.util.x
            sawfish.wm.windows
            sawfish.wm.workspace)

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
   "You'll find it useful,when you're using `event-wait-for'"
   (throw 'event-exit)))
