;;
;;  Then, in your ~/.sawfishrc, put the command: 
;;       (require 'sawfish.wm.ext.infinite-desktop)
;;
;;  Use the Customize->Sawfish->Infinite Desktop
;;   configurator to enable and configure the option.  Edge
;;   flipping should be disabled.
;;

(define-structure sawfish.wm.ext.infinite-desktop

	(export )

	(open rep
	      rep.system
	      sawfish.wm.misc
	      sawfish.wm.custom
	      sawfish.wm.commands.move-cursor
	      sawfish.wm.viewport
	      sawfish.wm.util.prompt
	      sawfish.wm.util.flippers
	      sawfish.wm.ext.edge-flip)

	(define-structure-alias infinite-desktop sawfish.wm.ext.infinite-desktop)

;;
;; Remove our hooks if they're already installed - 
;; allows us to be imported multiple times safely.
;;

(define (infinite-desktop.remove a l) 
  (cond ((not l) nil)		
	((eq a (car l)) (infinite-desktop.remove a (cdr l)))
	(t (cons (car l) (infinite-desktop.remove a (cdr l))))))

(defgroup infinite-desktop (_"Infinite Desktop")
  :group workspace)

(defcustom infinite-desktop-p 1
  (_"Simulate an infinite desktop (Conflicts edge-flipping).")
  :group (workspace infinite-desktop)
  :after-set (lambda () (infinite-desktop.infinite-desktop))
  :type boolean)

(defcustom infinite-desktop.move-distance 64
  (_"Amount to move the workspace.")
  :group (workspace infinite-desktop)
  :type number
  :range (1 . nil))

(defcustom infinite-desktop.move-cursor-distance 32
  (_"Amount to move the cursor after moving the workspace.")
  :group (workspace infinite-desktop)
  :type number
  :range (1 . nil))

(defcustom infinite-desktop.stop-at-workspace-borders nil
  (_"Stop scrolling at workspace borders (Fixes warp-to-window bugs).")
  :group (workspace infinite-desktop)
  :type boolean )

(define (infinite-desktop.move-right)
  (let ((dist infinite-desktop.move-distance)
	(cdist infinite-desktop.move-cursor-distance)
	(maxx (* (screen-width) (1- (car viewport-dimensions)))))
    (if 
	(and infinite-desktop.stop-at-workspace-borders
	     (> (+ dist viewport-x-offset) maxx))
	(setq dist (- maxx viewport-x-offset)))
    (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
    (move-cursor (- (min dist cdist)) 0)))
  
(define (infinite-desktop.move-left)
  (let ((dist (- infinite-desktop.move-distance))
	(cdist (- infinite-desktop.move-cursor-distance))
	(minx 0))
    (if 
	(and infinite-desktop.stop-at-workspace-borders
	     (< (+ viewport-x-offset dist) minx))
	(setq dist (- minx viewport-x-offset)))
    (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
    (move-cursor (max dist cdist) 0)))

(define (infinite-desktop.move-top)
  (let ((dist (- infinite-desktop.move-distance))
	(cdist (- infinite-desktop.move-cursor-distance))
	(miny 0))
    (if 
	(and infinite-desktop.stop-at-workspace-borders
	     (< (+ viewport-y-offset dist) miny))
	(setq dist (- miny viewport-y-offset)))
    (set-viewport viewport-x-offset (+ viewport-y-offset dist))
    (move-cursor 0 (max dist cdist))))

(define (infinite-desktop.move-bottom)
  (let ((dist infinite-desktop.move-distance)
	(cdist infinite-desktop.move-cursor-distance)
	(maxy (* (screen-height) (1- (cdr viewport-dimensions)))))
    (if 
	(and infinite-desktop.stop-at-workspace-borders
	     (> (+ dist viewport-y-offset) maxy))
	(setq dist (- maxy viewport-y-offset)))
    (set-viewport viewport-x-offset (+ viewport-y-offset dist))
    (move-cursor 0 (- (min dist cdist)))))

(define (infinite-desktop.enter-flipper-hook w)
  (if infinite-desktop-p 
      (cond ((eq w 'right) (infinite-desktop.move-right))
	    ((eq w 'left) (infinite-desktop.move-left))
	    ((eq w 'bottom) (infinite-desktop.move-bottom))
	    ((eq w 'top) (infinite-desktop.move-top))
	    (t (display-message "move-unknown")))))

(define (infinite-desktop.infinite-desktop) 
  (if infinite-desktop-p 
      (enable-flippers)))

(unless batch-mode
  (add-hook 'enter-flipper-hook infinite-desktop.enter-flipper-hook)))
