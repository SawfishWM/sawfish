;;; Hi-level operations on Viewports
;; 
(define-structure sawfish.wm.viewport-hi
    (export
     set-viewport
     set-screen-viewport
     select-workspace-an

     d-viewport

     move-viewport
     move-viewport-to-window

     ;; move-window-to-current-viewport
     ;; set-window-viewport
     ;; move-window-viewport
     ;; window-viewport
     ;; window-absolute-position
     set-number-of-viewports

;;      ;; mmc:
;;      new-viewport
;;      viewport?
;;      restore-current-viewport
;;      save-current-viewport
;;      describe-window-position
     )
    (open
     rep
     rep.system
     rep.mmsystem
     rep.trace
     rep.io.files
     rep.lang.math                      ; logior
      
     sawfish.wm.windows
     sawfish.wm.misc
     sawfish.wm.events
     sawfish.wm.commands
     sawfish.wm.workspace
     sawfish.wm.custom
     sawfish.wm.session.init
     
     ;;sawfish.wm.adt
     sawfish.wm.viewport
     sawfish.wm.util.window-order
     sawfish.wm.stacking		;raise
     ;rep.data.records
     )

  

  (define debug #f "used by rep.trace macros")
  (defvar viewport-debug 0 "")
  (define viewport-debug-lists 2)
  (defvar moving-window #f "the window which is being carried to another VP")


;;;
  (define (set-viewport x y)
    ;; move W to its new position
    (define (move-window w)             ;  fixme: why not (let ( ??

      (unless (or (window-get w 'sticky-viewport) ; or 'being-moved !
                  (window-get w 'moving-viewport))
        (let ((pos (window-position w)))
          ;; debugging/tracing:
                                        ;(if (or (eq w special-window)
                                        ;  (eq w special-window-2))
          (unless (zerop viewport-debug)
            (DB "viewport: move-window  %s\t %d, %d -> %d, %d"
                (window-name w)
                (car pos) (cdr pos)
                (- (+ (car pos) viewport-x-offset) x)
                (- (+ (cdr pos) viewport-y-offset) y)))
                                        ;)
          (move-window-to w
                          (- (+ (car pos) viewport-x-offset) x)
                          (- (+ (cdr pos) viewport-y-offset) y))
                                        ;(sleep-for 0 30)
          (if (or (eq w special-window)
                  (eq w special-window-2))
              (describe-window-position w)))))

    (unless (and (= viewport-x-offset x) (= viewport-y-offset y))
      (with-server-grabbed
       (set-proactive-move 1)
       ;;(message (format #f "set-viewport:  WS: %d:  %d %d" current-workspace x y))
       (let loop ((rest
		   ;; mmc: 2003-06-29  hm, that was _my_ problem i did the error of taking
		   ;; only on the WS,
		   ;;  but not in the stacking-order.
		   (if #f
		       (workspace-windows current-workspace)
		     ;; mmc:  fixme: are they in stacking order ??? i doubt it
		     ;; mmc:
                                        ; (delete-if window-outside-viewport-p
		     (filter (lambda (w) (window-in-workspace-p w current-workspace))
			     (stacking-order))))
		  (inside '())
		  (outside '()))

	    (cond
	     ((null rest)
	      ;; this is the final step:
	      (unless (zerop (logand viewport-debug viewport-debug-lists))
		(DB "outside: %s\n" outside))
	      (if #t
		  (progn
		    (let ((new-focus
			   (update-order+determine-focus-in-viewport
			    (cons
			     (- viewport-x-offset x)
			     (- viewport-y-offset y)))))
		      (when new-focus
			(DB "new focus will be %s" (window-name new-focus))
			;; fixme: useless?
			;;(if (window-really-wants-input-p new-focus)
			(set-future-input-focus new-focus))

		    ;; mmc:  [21 ott 04]
		    ;; reverse! i don't know difference!

		    ;; First move all windows not on the old viewport, and
		    ;; move in top-to-bottom order..
		    (mapc move-window (nreverse outside))

		    ;; ..then move away the windows on the old viewport,
		    ;; in bottom-to-top order
		    (mapc move-window inside)

		    (when new-focus
		      (set-input-focus new-focus)
		      (raise-window new-focus))))


		(progn
		  ;; mmc:  [21 ott 04]
		  ;; reverse! i don't know difference!

		  ;; ..then move away the windows on the old viewport,
		  ;; in bottom-to-top order
		  (mapc
		   (lambda (w)
		     (DB "moving away %s\n" (window-name w))
		     (move-window w))
		   inside)
                        
                                        ;(sleep-for 1)
		  ;; First move all windows not on the old viewport, and
		  ;; move in top-to-bottom order..
		  (mapc
		   (lambda (w)
		     (DB "moving here %s\n" (window-name w))
		     (move-window w))
		   (nreverse outside)))))

	     ;; first we collect the 2 colections:  outside and inside:
	     ((window-outside-viewport-p (car rest))
		   (loop (cdr rest) inside (cons (car rest) outside)))

	     (t
	      (loop (cdr rest) (cons (car rest) inside) outside))))

      (setq viewport-x-offset x)
      (setq viewport-y-offset y)
      ;(call-hook 'viewport-moved-hook)
      (set-proactive-move 0)
      )))

  (define (viewport-before-exiting)
    ;; so that the geometry of windows is not negative
    (DB "viewport-before-exiting:\n")
    (set-screen-viewport 0 0))

  ;; [05 nov 04] no:
  ;;(add-hook-s 'before-exit-hook viewport-before-exiting t)
  ;;  because i save the _NET_DESKTOP_VIEWPORT

  ;; returns t if it actually moved the viewport
  (define (set-screen-viewport col row)
    (when (and (>= col 0) (< col (car viewport-dimensions))
               (>= row 0) (< row (cdr viewport-dimensions)))
      (set-viewport (* col (screen-width))
                    (* row (screen-height)))
      t))

  ;; currently unused!
  (define (select-workspace-and-viewport space col row)
    (select-workspace space nil (lambda ()
                                  (set-screen-viewport col row))))
  
  ;; returns t if it actually moved the viewport
  (define (move-viewport right down)
    (let ((port (screen-viewport)))
      (set-screen-viewport (+ (car port) right)
                           (+ (cdr port) down))))


  ;;  mmc:  i want to do this w/ an abstract  vp:
  (define (move-viewport-to-window window)
    (when (window-outside-viewport-p window)
      (let ((pos (window-position window)))
        (rplaca pos (+ (car pos) viewport-x-offset))
        (rplacd pos (+ (cdr pos) viewport-y-offset))
        (set-screen-viewport (quotient (car pos) (screen-width))
                             (quotient (cdr pos) (screen-height))))))


  (define (viewport-size-changed)
    (DB "viewport-size-changed\n")
    (let ((port (screen-viewport)))
      (set-screen-viewport (min (car port) (1- (car viewport-dimensions)))
                           (min (cdr port) (1- (cdr viewport-dimensions))))
      (map-windows (lambda (w)
                     (when (window-outside-workspace-p w)
                       (move-window-to-current-viewport w))))
      (call-hook 'viewport-resized-hook)))

  (define (set-number-of-viewports width height)
    (setq viewport-dimensions (cons width height))
    (viewport-size-changed))


;; commands

  (define (activate-viewport x y)
    "Select the specified viewport."
    (set-screen-viewport (1- x) (1- y)))

  (define-command 'activate-viewport activate-viewport
                  #:spec "NX:\nNY:"
                  #:type `(and (labelled ,(_ "Column:") (number 1))
                               (labelled ,(_ "Row:") (number 1)))
                  #:class 'viewport)

  (define (activate-viewport-column x)
    "Select the specified viewport column."
    (set-screen-viewport (1- x) (cdr (screen-viewport))))

  (define-command 'activate-viewport-column activate-viewport-column
                  #:spec "NX:"
                  #:type `(and (labelled ,(_ "Column:") (number 1)))
                  #:class 'viewport)

  (define (activate-viewport-row y)
    "Select the specified viewport row."
    (set-screen-viewport (car (screen-viewport)) (1- y)))

  (define-command 'activate-viewport-row activate-viewport-row
                  #:spec "NY:"
                  #:type `(and (labelled ,(_ "Row:") (number 1)))
                  #:class 'viewport)

  (define (move-window-to-viewport x y)
    "Move the current window to the specified viewport."
    (move-window-viewport (current-event-window) (1- x) (1- y)))

  (define-command 'move-window-to-viewport move-window-to-viewport
                  #:spec "NX:\nNY:"
                  #:type '(and (labelled "X:" (number 1)) (labelled "Y:" (number 1)))
                  #:class 'viewport)

  (define (move-viewport-right)
    "Move the viewport one screen to the right."
    (move-viewport 1 0))

  (define (move-viewport-left)
    "Move the viewport one screen to the left."
    (move-viewport -1 0))

  (define (move-viewport-down)
    "Move the viewport one screen down."
    (move-viewport 0 1))

  (define (move-viewport-up)
    "Move the viewport one screen up."
    (move-viewport 0 -1))

;; Moves the window by the specified offsets and then flips to the
;; viewport that is relative those offsets to the current viewport.
  (define (move-window-to-viewport-and-move-viewport window col row)
    (require 'sawfish.wm.util.stacking)
    ;; (let ((sticky-viewport (window-get window 'sticky-viewport)))
    (setq moving-window window)

    ;;(window-put window 'sticky-viewport t)
    (window-put window 'moving-viewport t)


    (with-server-grabbed                ; mmc: why?  why not condition-case
     (raise-window* window)
     (move-viewport col row)
     ;;(display-message "sticky window lost focus, revert it")
     (set-input-focus window))
    (setq moving-window #f)
    ;'(unless sticky-viewport
    ;   (window-put window 'sticky-viewport nil))

    (window-put window 'moving-viewport #f)
    (if (eq nil (window-get window 'order)) ;mmc: could be removed now!
        (display-message "sticky window: -> lost order")))



  (define (move-window-left w)
    "Move the window to the viewport on the left, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w -1 0))

  (define (move-window-right w)
    "Move the window to the viewport on the right, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w 1 0))

  (define (move-window-down w)
    "Move the window to the viewport below, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w 0 1))

  (define (move-window-up w)
    "Move the window to the viewport above, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w 0 -1))

  (define-command 'move-viewport-right move-viewport-right #:class 'viewport)
  (define-command 'move-viewport-left move-viewport-left #:class 'viewport)
  (define-command 'move-viewport-up move-viewport-up #:class 'viewport)
  (define-command 'move-viewport-down move-viewport-down #:class 'viewport)
  (define-command 'move-window-right move-window-right #:spec "%W" #:class 'viewport)
  (define-command 'move-window-left move-window-left #:spec "%W" #:class 'viewport)
  (define-command 'move-window-up move-window-up #:spec "%W" #:class 'viewport)
  (define-command 'move-window-down move-window-down #:spec "%W" #:class 'viewport)
)
