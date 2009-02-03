;; tab.jl - Emulate fluxbox tabs system
;;
;; Author : Yann Hodique <Yann.Hodique@lifl.fr>
;;
;; Heavily modified by Scott Scriven <sawfish@toykeeper.net>
;;
;; Modified by Nathan Froyd <froydnj@gmail.com>
;;
;; Intltoolized/Reworked by Christopher Bratusek <zanghar@freenet.de>
;;
;; Usage:
;;  Copy this file to somewhere in your sawfish load-path, for example
;;  "~/.sawfish/lisp". Then add (require 'tab) to your ~/.sawfishrc and restart
;;  sawfish or issue the (load "tab") from sawfish-client.
;;
;;  Be sure to also put marks.jl and tabgroup.jl in the same location.  The tab.jl
;;  file requires these.
;;
;;  After loading "tab", be sure to do the same with "tab-keymap" .  Feel free to
;;  customize the key bindings in that file.
;;
;;  You will also need a tab-enabled theme, in order to see any visual difference.
;;  There should be such a theme included with this file.


(define-structure sawfish.wm.tabs.tab

	(export add-to-group )

	(open rep
	      rep.system
	      sawfish.wm.misc
	      sawfish.wm.custom
	      sawfish.wm.commands
	      sawfish.wm.frames
	      sawfish.wm.tabs.tabgroup
	      sawfish.wm.tabs.marks
	      sawfish.wm.windows)

     (define-structure-alias tab sawfish.wm.tabs.tab)

  ;; TODO:
  ;; - change other tab sizes when window resizes itself
  ;; - make calculations work with tiny windows
  ;; - hide some frame parts on leftmost and rightmost tabs
  ;; - add a drag-n-drop way to group windows by tabs

  ;;###autoload (defgroup tabs (_"Tabs"))

  (defgroup tabs (_"Tabs"))

  (defcustom tab-left-dec-width 11 (_"Width of tab's left-edge decoration")
    :group tabs
    :type number)

  (defcustom tab-right-dec-width 11 (_"Width of tab's right-edge decoration")
    :group tabs
    :type number)

  (defcustom tab-left-margin 16 (_"Width of tab area's left-edge decoration")
    :group tabs
    :type number)

  (defcustom tab-right-margin 16 (_"Width of tab area's right-edge decoration")
    :group tabs
    :type number)

  (define (get-tab-pos win)
    (let* ((group (tab-find-window win))
       	   (tabnum (tab-rank win (tab-group-window-list group))))
      (tab-pos group tabnum win))) 

  (define (tab-pos group tabnum win)
    "find the left and right pixel offsets of a tab"
    (let* ((tabarea-width (+ 
	     ; get width of a window in this group
	     ;(car (window-dimensions (car (tab-group-window-list group)))) 
	     (car (window-dimensions win)) 
	     (- tab-left-margin)
	     (- tab-right-margin)))
	 (numtabs (length (tab-group-window-list group)))
	 (left (quotient (* tabnum tabarea-width) numtabs))
	 ; the right edge is not always "left + (window-width / numtabs)"
	 ; that would be inaccurate due to rounding errors
	 (right (quotient (* (+ tabnum 1) tabarea-width) numtabs))
	 (width (- right left)))
    (list left right width)))

  (define (tab-title-text-width win)
    "width of the title text area is the tabwidth minus decorations"
    (let* ((tabwidth (nth 2 (get-tab-pos win))))
      (+ tabwidth 
        (- tab-left-dec-width)
        (- tab-right-dec-width))))

  (define (tab-left-edge win)
    "Compute left edge of tab"
    (let* ((left (nth 0 (get-tab-pos win))))
      (+ left tab-left-margin)))

  (define (tab-right-dec-pos win)
    "Compute position of tab's right-edge decoration"
    (let* ((right (nth 1 (get-tab-pos win))))
      (+ right tab-left-margin (- tab-right-dec-width))))

  (define (tab-title-left-edge win)
    "Compute left edge of tab"
    (+ (tab-left-edge win) tab-left-dec-width))

  ;; new class : tab
  (define-frame-class 'tab
    `((cursor . left_ptr)
      (x-justify . center)
      (y-justify . center)
      (left-edge . ,tab-title-left-edge)
      (width . ,tab-title-text-width)))
  (set-frame-part-value 'tab 'keymap 'title-keymap)

  (define-frame-class 'tab-l
    `((cursor . left_ptr)
      (left-edge . ,tab-left-edge)) t)

  (define-frame-class 'tab-r
    `((cursor . left_ptr)
      (left-edge . ,tab-right-dec-pos)) t)

  (define (mygroup win)
    (if (marked-windows)
        (progn
          (apply-on-marked-windows (lambda (w) (tab-group-window w win)))
          (unmark-all-windows))
      (mark-window win)))

  (define-command 'add-to-group mygroup #:spec "%W"))

  ;(require 'x-cycle)
  ;(define-cycle-command-pair
  ;  'cycle-tabgroup 'cycle-tabgroup-backwards
  ;  (lambda (w)
  ;    (delete-if-not window-in-cycle-p   
  ;                   (delete-if (lambda (win)
  ;                                (and (not (eq win w))
  ;                                     (tab-same-group-p win w))) 
  ;                              (workspace-windows current-workspace))
  ;                   )
  ;    )
  ;  #:spec "%W")

  ;(require 'sawfish.wm.util.window-order)

