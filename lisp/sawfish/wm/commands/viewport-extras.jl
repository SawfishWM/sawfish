;; viewport-extras.jl -- extra viewport commands
;;
;; Contributed by: Dams Nadé <anvil@amin.unice.fr>

(define-structure sawfish.wm.commands.viewport-extras

    (export move-viewport-next
	    move-viewport-previous)

    (open rep
          sawfish.wm.viewport
          sawfish.wm.commands)

  ;; Returns (cons next-x next-y) from current screen-viewport
  (define (next-coords)
    (let* ((port (screen-viewport))
	   (x (car port))
	   (y (cdr port))
	   (xmax (- (car viewport-dimensions) 1))
	   (ymax (- (cdr viewport-dimensions) 1)))
      (cond ((not (= x xmax)) (cons (+ 1 x) y))
	    ((= y ymax) (cons 0 0))
	    (t (cons 0 (+ y 1))))))

  ;; Returns (cons prev-x prev-y) from current screen-viewport
  (define (previous-coords)
    (let* ((port (screen-viewport))
	   (x (car port))
	   (y (cdr port))
	   (xmax (- (car viewport-dimensions) 1))
	   (ymax (- (cdr viewport-dimensions) 1)))
      (cond ((not (= 0 x)) (cons (- x 1) y))
	    ((= y 0) (cons xmax ymax))
	    (t (cons xmax (- y 1))))))

  ;; Move window & screen-viewport
  (define (set-window-and-viewport w x y)
    (set-window-viewport w x y)
    (set-screen-viewport x y))

  (define (move-viewport-next)
    "Move to the next viewport."
    (let ((nextcoords (next-coords)))
      (set-screen-viewport (car nextcoords) (cdr nextcoords))))

  (define (move-viewport-previous)
    "Move to the previous viewport."
    (let ((prevcoords (previous-coords)))
      (set-screen-viewport (car prevcoords) (cdr prevcoords))))

  (define (move-window-next w)
    "Move the window to the next viewport."
    (let ((nexts (next-coords)))
      (set-window-and-viewport w (car nexts) (cdr nexts))))

  (define (move-window-previous w)
    "Move the window to the previous viewport."
    (let ((prevs (previous-coords)))
      (set-window-and-viewport w (car prevs) (cdr prevs))))

  ;;###autoload
  (define-command 'move-viewport-next
    move-viewport-next #:class 'viewport)
  (define-command 'move-viewport-previous
    move-viewport-previous #:class 'viewport)
  (define-command 'move-window-previous
    move-window-previous #:spec "%W" #:class 'viewport)
  (define-command 'move-window-next
    move-window-next #:spec "%W" #:class 'viewport))
