;; events.jl

;; Copyright (C) 2011, hqwrong <hq.wrong@gmail.com>

;; This file is part of sawfish.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License                                                                                      
;; along with this program.  If not, see `http://www.gnu.org/licenses/'. 


(define-structure sawfish.wm.util.events

    (export event-wait-for
            event-exit-wait)
    
    (open rep
          rep.data
          rep.system
          rep.regexp
          sawfish.wm.misc
          sawfish.wm.events
          sawfish.wm.commands)

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
           (exit-hook key))
         (remove-hook 'unbound-key-hook re-exit)))))

  (define (event-exit-wait)
    "You'll find it useful,when you're using `event-wait-for'"
    (throw 'event-exit)))
