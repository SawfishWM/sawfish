;; wm-spec.jl -- ewmh support

;; Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.state.wm-spec

    (export define-wm-spec-window-type
	    define-wm-spec-window-state)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.windows
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.state.iconify
	  sawfish.wm.util.workarea)

  ;; todo:

  ;; - _NET_WM_ICON

  ;; 1.1 additions:
  ;;  - _NET_WM_ALLOWED_ACTIONS
  ;;  - _STATE_HIDDEN?
  ;;  - _NET_WM_MOVERESIZE changes
  ;;  - _NET_SHOWING_DESKTOP?
  ;;  - _NET_MOVERESIZE_WINDOW

;;; constants

  (defconst _NET_WM_MOVERESIZE_SIZE_TOPLEFT 0)
  (defconst _NET_WM_MOVERESIZE_SIZE_TOP 1)
  (defconst _NET_WM_MOVERESIZE_SIZE_TOPRIGHT 2)
  (defconst _NET_WM_MOVERESIZE_SIZE_RIGHT 3)
  (defconst _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT 4)
  (defconst _NET_WM_MOVERESIZE_SIZE_BOTTOM 5)
  (defconst _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT 6)
  (defconst _NET_WM_MOVERESIZE_SIZE_LEFT 7)
  (defconst _NET_WM_MOVERESIZE_MOVE 8)
  (defconst _NET_WM_MOVERESIZE_SIZE_KEYBOARD 9)
  (defconst _NET_WM_MOVERESIZE_MOVE_KEYBOARD 10)

  (defconst _NET_WM_STATE_REMOVE 0)
  (defconst _NET_WM_STATE_ADD 1)
  (defconst _NET_WM_STATE_TOGGLE 2)

  (define wm-spec-window-id nil)

  (define supported-atoms
    [_NET_ACTIVE_WINDOW
     _NET_CLIENT_LIST
     _NET_CLIENT_LIST_STACKING
     _NET_CLOSE_WINDOW
     _NET_CURRENT_DESKTOP
     _NET_DESKTOP_GEOMETRY
     _NET_DESKTOP_NAMES
     _NET_DESKTOP_VIEWPORT
     _NET_NUMBER_OF_DESKTOPS
     _NET_PROTOCOLS
     _NET_SHOWING_DESKTOP
     _NET_SUPPORTED
     _NET_SUPPORTING_WM_CHECK
     _NET_WORKAREA
     _NET_WM_DESKTOP
     _NET_WM_ICON_GEOMETRY
     _NET_WM_MOVERESIZE
     _NET_WM_MOVERESIZE_MOVE
     _NET_WM_MOVERESIZE_SIZE_BOTTOM
     _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT
     _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT
     _NET_WM_MOVERESIZE_SIZE_LEFT
     _NET_WM_MOVERESIZE_SIZE_RIGHT
     _NET_WM_MOVERESIZE_SIZE_TOP
     _NET_WM_MOVERESIZE_SIZE_TOPLEFT
     _NET_WM_MOVERESIZE_SIZE_TOPRIGHT
     _NET_WM_MOVERESIZE_SIZE_KEYBOARD
     _NET_WM_MOVERESIZE_MOVE_KEYBOARD
     _NET_WM_PING
     _NET_WM_STATE
     _NET_WM_STATE_ABOVE
     _NET_WM_STATE_ADD
     _NET_WM_STATE_BELOW
     _NET_WM_STATE_FULLSCREEN
     _NET_WM_STATE_MAXIMIZED
     _NET_WM_STATE_MAXIMIZED_HORZ
     _NET_WM_STATE_MAXIMIZED_VERT
     _NET_WM_STATE_REMOVE
     _NET_WM_STATE_SHADED
     _NET_WM_STATE_SKIP_PAGER
     _NET_WM_STATE_SKIP_TASKBAR
     _NET_WM_STATE_STICKY
     _NET_WM_STATE_TOGGLE
     _NET_WM_STRUT
     _NET_WM_WINDOW_TYPE
     _NET_WM_WINDOW_TYPE_DESKTOP
     _NET_WM_WINDOW_TYPE_DIALOG
     _NET_WM_WINDOW_TYPE_DOCK
     _NET_WM_WINDOW_TYPE_TOOLBAR
     _NET_WM_WINDOW_TYPE_MENU
     _NET_WM_WINDOW_TYPE_UTILITY
     _NET_WM_WINDOW_TYPE_SPLASH
     _NET_WM_USER_TIME])

  (defvar wm-spec-below-depth -2)
  (defvar wm-spec-above-depth +2)

  (define supported-states '())

;;; setting the client list hints

  (define (update-client-list-hints #!key only-stacking-list)
    (define (set-prop lst prop)
      (let loop ((rest lst)
		 (collected '()))
	(cond ((null rest)
	       (set-x-property 'root prop
			       (apply vector (nreverse collected))
			       'WINDOW 32))
	      ((window-mapped-p (car rest))
	       (loop (cdr rest) (cons (window-id (car rest)) collected)))
	      (t (loop (cdr rest) collected)))))
    (unless only-stacking-list
      (set-prop (managed-windows) '_NET_CLIENT_LIST))
    (set-prop (nreverse (stacking-order)) '_NET_CLIENT_LIST_STACKING))

  ;; setting the desktop / viewport hints

  (define last-workspace nil)
  (define last-workspace-count 0)
  (define last-workspace-names nil)
  (define last-area nil)
  (define last-area-count nil)
  (define last-workarea nil)
  (define last-showing-desktop nil)

  (define (update-window-workspace-hints w #!key (limits (workspace-limits)))
    (let ((vec (if (window-sticky-p/workspace w)
		   (vector #xffffffff)
		 (let ((space (or (window-get w 'swapped-in)
				  (car (window-workspaces w)))))
		   (and space (vector (- space (car limits))))))))
      (unless (equal vec (window-get w 'wm-spec/last-workspace))
	(if vec
	    (set-x-property w '_NET_WM_DESKTOP vec 'CARDINAL 32)
	  (delete-x-property w '_NET_WM_DESTOP))
	(window-put w 'wm-spec/last-workspace vec))))

  (define (update-workspace-hints)
    (let* ((limits (workspace-limits))
	   (port (screen-viewport))
           ;; Since vp size can vary from workspace to workspace, use
           ;; the maximum dimensions accross all workspaces.
	   (port-size (let ((dims (cons viewport-dimensions
                                        (mapcar (lambda (e)
                                                  (unless (eq (car e)
                                                              current-workspace)
                                                    (nth 3 e)))
                                                workspace-viewport-data))))
                        (cons (apply max (mapcar car dims))
                              (apply max (mapcar cdr dims)))))
	   (total-workspaces (1+ (- (cdr limits) (car limits))))
	   (workarea (make-vector (* 4 total-workspaces)))
	   (showing-desktop (showing-desktop-p)))

      (define (set-ws-hints)
	;; _NET_NUMBER_OF_DESKTOPS
	(unless (equal last-workspace-count total-workspaces)
	  (setq last-workspace-count total-workspaces)
	  (set-x-property 'root '_NET_NUMBER_OF_DESKTOPS
			  (vector total-workspaces) 'CARDINAL 32))

	;; _NET_DESKTOP_NAMES
	(unless (equal last-workspace-names workspace-names)
	  (setq last-workspace-names workspace-names)
	  (set-x-text-property 'root '_NET_DESKTOP_NAMES
			       (apply vector workspace-names)
			       'UTF8_STRING))

	;; _NET_CURRENT_DESKTOP
	(unless (equal last-workspace
		       (- current-workspace (car limits)))
	  (setq last-workspace (- current-workspace (car limits)))
	  (set-x-property 'root '_NET_CURRENT_DESKTOP
			  (vector last-workspace) 'CARDINAL 32))

	;; _NET_DESKTOP_GEOMETRY
	(unless (equal last-area-count port-size)
	  (setq last-area-count port-size)
	  (set-x-property 'root '_NET_DESKTOP_GEOMETRY
			  (vector (* (car port-size) (screen-width))
				  (* (cdr port-size) (screen-height)))
			  'CARDINAL 32))

	;; _NET_DESKTOP_VIEWPORT
	(unless (equal last-area port)
	  (let ((view (make-vector (* total-workspaces 2))))
	    (let loop ((i 0))
	      (if (= i total-workspaces)
		  (set-x-property 'root '_NET_DESKTOP_VIEWPORT
				  view 'CARDINAL 32)
		(if (eq i current-workspace)
		    (progn
		      (aset view (* i 2) (* (car port) (screen-width)))
		      (aset view (1+ (* i 2)) (* (cdr port)
						 (screen-height))))
		  (let ((vp-data (cdr (assoc i workspace-viewport-data))))
		    (aset view (* i 2) (car vp-data))
		      (aset view (1+ (* i 2)) (nth 1 vp-data))))
		(loop (1+ i))))))

	;; _NET_WORKAREA
	(unless (equal last-workarea workarea)
	  (set-x-property 'root '_NET_WORKAREA workarea 'CARDINAL 32)
	  (setq last-workarea workarea))

	;; _NET_SHOWING_DESKTOP
	(unless (equal showing-desktop last-showing-desktop)
	  (set-x-property 'root '_NET_SHOWING_DESKTOP
			  (vector (if showing-desktop 1 0)) 'CARDINAL 32)
	  (setq last-showing-desktop showing-desktop)))

      (define (set-window-hints w)
	(update-window-workspace-hints w #:limits limits))

      ;; calculate workareas
      (do ((i 0 (1+ i)))
	  ((= i total-workspaces))
	(let ((area (calculate-workarea-from-struts
		     #:workspace (+ i (car limits)))))
	  (aset workarea (+ (* i 4) 0) (nth 0 area))
	    (aset workarea (+ (* i 4) 1) (nth 1 area))
	    (aset workarea (+ (* i 4) 2) (- (nth 2 area) (nth 0 area)))
	    (aset workarea (+ (* i 4) 3) (- (nth 3 area) (nth 1 area)))))

      ;; apparently some pagers don't like it if we place windows
      ;; on (temporarily) non-existent workspaces
      (when (< last-workspace-count total-workspaces)
	(set-ws-hints))

      (map-windows set-window-hints)

      (when (>= last-workspace-count total-workspaces)
	(set-ws-hints))))

;;; setting the focus hints

  (define last-focus nil)

  (define (update-focus-state)
    (let ((focus (input-focus)))
      (unless (eq last-focus focus)
	(setq last-focus focus)
	(set-x-property 'root '_NET_ACTIVE_WINDOW
			(vector (if focus (window-id focus) 0)) 'WINDOW 32))))

;;; setting the window state hints

  (define (update-client-state w)
    (let ((state '()))
      (mapc (lambda (x)
	      (when (and (not (pseudo-state-p x))
			 (call-state-fun w x 'get))
		(setq state (cons x state))))
	    supported-states)
	(set-x-property w '_NET_WM_STATE (apply vector state) 'ATOM 32)))

;;; honouring the initially set window state hints

  (define (update-icon-geometry w geom)
    (when (>= (length geom) 2)
      (window-put w 'icon-position (cons (aref geom 0) (aref geom 1)))))

  (define (update-strut w)
    (let ((strut (get-x-property w '_NET_WM_STRUT)))
      (when (and strut (eq (nth 0 strut) 'CARDINAL))
	(let ((data (nth 2 strut)))
	  (define-window-strut w (aref data 0) (aref data 2)
            (aref data 1) (aref data 3))))))

  (define (honour-client-state w)
    (let ((space (get-x-property w '_NET_WM_DESKTOP)))
      (when space
	(setq space (aref (nth 2 space) 0))
	(cond ((equal space #xffffffff)
	       (window-put w 'sticky t))
	      ((and (integerp space) (null (window-workspaces w)))
	       (set-window-workspaces w (list space))))))

    (let ((type (get-x-property w '_NET_WM_WINDOW_TYPE)))
      (when type
	(setq type (nth 2 type))
	;; _NET_WM_WINDOW_TYPE is a vector of atoms, the first atom
	;; about which we know something is the type we'll use
	(let loop ((i 0))
	  (cond ((= i (length type)))
		((get (aref type i) 'wm-spec-type)
		 ((get (aref type i) 'wm-spec-type) w))
		(t (loop (1+ i)))))))

    (let ((state (get-x-property w '_NET_WM_STATE)))
      (when state
	(setq state (nth 2 state))
	(do ((i 0 (1+ i)))
	    ((= i (length state)))
	  (call-state-fun w (aref state i) 'init))))

    (update-strut w)

    (let ((geom (get-x-property w '_NET_WM_ICON_GEOMETRY)))
      (when geom
	(update-icon-geometry w (nth 2 geom))))
    (when (equal (get-x-property w '_NET_WM_USER_TIME)
                 '(CARDINAL 32 #(0)))
      (window-put w 'inhibit-focus-when-mapped t)))

;;; helper functions

  (define (define-wm-spec-window-type x fun)
    (if (listp x)
	(mapc (lambda (y) (define-wm-spec-window-type y fun)) x)
      (put x 'wm-spec-type fun)))

  (define (define-wm-spec-window-state x fun #!key pseudo)
    (put x 'wm-spec-state fun)
    (unless (memq x supported-states)
      (setq supported-states (cons x supported-states)))
    (when pseudo
      (put x 'wm-spec-pseudo-state t)))

  (define (supported-state-p x) (and (symbolp x) (get x 'wm-spec-state)))
  (define (pseudo-state-p x) (and (symbolp x) (get x 'wm-spec-pseudo-state)))

  (define (call-state-fun w state mode)
    (let ((fun (and (symbolp state) (get state 'wm-spec-state))))
      (when fun
	(fun w mode))))

  (define-wm-spec-window-type '_NET_WM_WINDOW_TYPE_DESKTOP
    (lambda (w)
      (mark-window-as-desktop w)))

  (define-wm-spec-window-type '_NET_WM_WINDOW_TYPE_DOCK
    (lambda (w)
      (mark-window-as-dock w)))

  (define-wm-spec-window-type '_NET_WM_WINDOW_TYPE_DIALOG
    (lambda (w)
      (mark-window-as-transient w)))

  (define-wm-spec-window-type '_NET_WM_WINDOW_TYPE_UTILITY
    (lambda (w)
      (require 'sawfish.wm.frames)
      (set-window-type w 'utility)))

  (define-wm-spec-window-type '_NET_WM_WINDOW_TYPE_TOOLBAR
    (lambda (w)
      (require 'sawfish.wm.frames)
      (set-window-type w 'toolbar)))

  (define-wm-spec-window-type '_NET_WM_WINDOW_TYPE_MENU
    (lambda (w)
      (require 'sawfish.wm.frames)
      (set-window-type w 'menu)))

  (define-wm-spec-window-type '_NET_WM_WINDOW_TYPE_SPLASH
    (lambda (w)
      (require 'sawfish.wm.frames)
      (set-window-type w 'splash)
      (window-put w 'place-mode 'centered)))

  (define-wm-spec-window-state '_NET_WM_STATE_STICKY
    (lambda (w mode)
      (case mode
        ((init)   (window-put w 'sticky-viewport t))
        ((remove) (make-window-unsticky/viewport w))
        ((add)    (make-window-sticky/viewport w))
        ((toggle) (if (window-sticky-p/viewport w)
                      (make-window-unsticky/viewport w)
                    (make-window-sticky/viewport w)))
        ((get)    (window-sticky-p/viewport w)))))

  (define (wm-spec-maximize-handler direction)
    (lambda (w mode)
      (require 'sawfish.wm.state.maximize)
      (case mode
	((init)
	 (window-put w (if (eq direction 'vertical)
			   'queued-vertical-maximize
			 'queued-horizontal-maximize) t))
	((remove) (unmaximize-window w direction))
	((add)    (maximize-window w direction))
	((toggle) (maximize-window-toggle w direction))
	((get)    (if (window-maximized-fullscreen-p w)
		      nil
		    (case direction
		      ((vertical) (window-maximized-vertically-p w))
		      ((horizontal) (window-maximized-horizontally-p w))
		      (t (window-maximized-p w))))))))

  (define-wm-spec-window-state '_NET_WM_STATE_MAXIMIZED_VERT
    (wm-spec-maximize-handler 'vertical))
  (define-wm-spec-window-state '_NET_WM_STATE_MAXIMIZED_HORZ
    (wm-spec-maximize-handler 'horizontal))
  (define-wm-spec-window-state '_NET_WM_STATE_MAXIMIZED
    (wm-spec-maximize-handler nil)
    #:pseudo t)

  (define-wm-spec-window-state '_NET_WM_STATE_SHADED
    (lambda (w mode)
      (require 'sawfish.wm.state.shading)
      (case mode
        ((init)   (window-put w 'shaded t))
        ((add)    (shade-window w))
        ((remove) (unshade-window w))
        ((toggle) (toggle-window-shaded w))
        ((get)    (window-get w 'shaded)))))

  (define-wm-spec-window-state '_NET_WM_STATE_SKIP_PAGER
    (lambda (w mode)
      (case mode
        ((init add) (window-put w 'window-list-skip t))
        ((remove)   (window-put w 'window-list-skip nil))
        ((toggle)   (window-put w 'window-list-skip
                                (not (window-get w 'window-list-skip))))
        ((get)      (window-get w 'window-list-skip)))))

  (define-wm-spec-window-state '_NET_WM_STATE_SKIP_TASKBAR
    (lambda (w mode)
      (case mode
        ((init add) (window-put w 'task-list-skip t))
        ((remove)   (window-put w 'task-list-skip nil))
        ((toggle)   (window-put w 'task-list-skip
                                (not (window-get w 'task-list-skip))))
        ((get)      (window-get w 'task-list-skip)))))

  (define-wm-spec-window-state '_NET_WM_STATE_FULLSCREEN
    (lambda (w mode)
      (require 'sawfish.wm.state.maximize)
      (case mode
        ((init) (window-put w 'queued-fullscreen-maximize t))
        ((add remove) (maximize-window-fullscreen w (eq mode 'add)))
        ((toggle) (maximize-window-fullscreen-toggle w))
        ((get) (window-maximized-fullscreen-p w)))))

  (define (above-below-handler depth w mode)
    (require 'sawfish.wm.stacking)
    (case mode
      ((init)
       (window-put w 'depth depth))
      ((add remove)
       (set-window-depth w (if (eq mode 'add) depth 0)))
      ((toggle)
       (set-window-depth w (if (= (window-depth w) depth) 0 depth)))
      ((get)
       (= (window-depth w) depth))))

  (define-wm-spec-window-state '_NET_WM_STATE_BELOW
    (lambda (w mode)
      (above-below-handler wm-spec-below-depth w mode)))

  (define-wm-spec-window-state '_NET_WM_STATE_ABOVE
    (lambda (w mode)
      (above-below-handler wm-spec-above-depth w mode)))

;;; client messages

  (define (client-message-handler w type data)
    (let ((handled t))
      (case type
	((_NET_CLOSE_WINDOW)
	 (when (windowp w)
	   (delete-window w)))

	((_NET_SHOWING_DESKTOP)
	 (if (= (aref data 0) 1)
	     (show-desktop)
	   (hide-desktop)))

	((_NET_WM_MOVERESIZE)
	 (when (and (windowp w) (window-mapped-p w))
	   (require 'sawfish.wm.commands.move-resize)
	   (let ((mode (aref data 2)))
	     ;; don't want grabs failing, sigh
	     (x-server-timestamp t t)
	       (if (or (eq mode _NET_WM_MOVERESIZE_MOVE)
		       (eq mode _NET_WM_MOVERESIZE_MOVE_KEYBOARD))
		   (move-window-interactively w)
		 (let ((move-resize-moving-edges
			(cond ((eq mode _NET_WM_MOVERESIZE_SIZE_TOPLEFT)
			       '(top left))
			      ((eq mode _NET_WM_MOVERESIZE_SIZE_TOP)
			       '(top))
			      ((eq mode _NET_WM_MOVERESIZE_SIZE_TOPRIGHT)
			       '(top right))
			      ((eq mode _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT)
			       '(bottom left))
			      ((eq mode _NET_WM_MOVERESIZE_SIZE_BOTTOM)
			       '(bottom))
			      ((eq mode _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT)
			       '(bottom right))
			      ((eq mode _NET_WM_MOVERESIZE_SIZE_LEFT)
			       '(left))
			      ((eq mode _NET_WM_MOVERESIZE_SIZE_RIGHT)
			       '(right)))))
		   (resize-window-interactively w))))))

	((_NET_NUMBER_OF_DESKTOPS)
	 (set-number-of-workspaces (aref data 0)))

	((_NET_DESKTOP_GEOMETRY)
	 (set-number-of-viewports (aref data 0) (aref data 1)))

	((_NET_DESKTOP_VIEWPORT)
	 (set-viewport (aref data 0) (aref data 1)))

	((_NET_CURRENT_DESKTOP)
         ;; KDE spews _NET_CURRENT_DESKTOP( -1) messages so often that it
         ;; is best to just ignore out of bounds errors silently.
         (let ((ws (workspace-id-from-logical (aref data 0)))
               (limits (workspace-limits)))
           (if (<= (car limits) ws (cdr limits))
               (select-workspace ws))))

	((_NET_DESKTOP_NAMES)
	 ;; XXX this is kind of broken now we use workspace-names to
	 ;; XXX define the minimum number of workspaces to display?
	 (setq data (aref data 0))
	 (let loop ((i 0)
		    (out '()))
	   (if (= i (length data))
	       (setq workspace-names (nreverse out))
	     (loop (1+ i) (cons (aref data i) out)))))

	((_NET_ACTIVE_WINDOW)
	 (require 'sawfish.wm.util.display-window)
	 (when (and (windowp w) (window-mapped-p w))
	   (display-window w)))

	((_NET_WM_STATE)
	 (when (windowp w)
	   (let ((mode (cond ((eql (aref data 0) _NET_WM_STATE_REMOVE)
                              'remove)
			     ((eql (aref data 0) _NET_WM_STATE_ADD)
                              'add)
			     ((eql (aref data 0) _NET_WM_STATE_TOGGLE)
                              'toggle)))
		 (atom1 (x-atom-name (aref data 1)))
		 (atom2 (x-atom-name (aref data 2))))
	     (when (or (and (eq atom1 '_NET_WM_STATE_MAXIMIZED_VERT)
			    (eq atom2 '_NET_WM_STATE_MAXIMIZED_HORZ))
		       (and (eq atom2 '_NET_WM_STATE_MAXIMIZED_VERT)
			    (eq atom1 '_NET_WM_STATE_MAXIMIZED_HORZ)))
	       (setq atom1 '_NET_WM_STATE_MAXIMIZED)
	       (setq atom2 nil))
	       (when atom1
		 (call-state-fun w atom1 mode))
	       (when atom2
		 (call-state-fun w atom2 mode)))))

	((_NET_WM_DESKTOP)
	 (when (windowp w)
	   (let ((desktop (aref data 0)))
	     (if (eql desktop #xffffffff)
		 ;; making window sticky
		 (make-window-sticky/workspace w)
	       ;; changing the desktop
	       (make-window-unsticky/workspace w)
	       (send-window-to-workspace-from-first w desktop nil)))))

	(t (setq handled nil)))
	handled))

;;; property changes

  (define (property-change-handler w prop kind)
    (declare (unused kind))
    (case prop
      ((_NET_WM_ICON_GEOMETRY)
       (let ((geom (get-x-property w '_NET_WM_ICON_GEOMETRY)))
	 (when geom
	   (update-icon-geometry w (nth 2 geom)))))
      ((_NET_WM_STRUT)
       (update-strut w))))

;;; utilities

  (define (vector->list vec)
    (do ((i 0 (1+ i))
	 (out '() (cons (aref vec i) out)))
	((= i (length vec)) (nreverse out))))

  (define (update-on-configure-notify w)
    (when (eq w 'root)
      (update-workspace-hints)))

;;; initialisation

  (define (init)
    (setq wm-spec-window-id (create-window 'root -200 -200 5 5))

    (set-x-property 'root '_NET_SUPPORTING_WM_CHECK
		    (vector wm-spec-window-id) 'WINDOW 32)
    (set-x-property wm-spec-window-id '_NET_SUPPORTING_WM_CHECK
		    (vector wm-spec-window-id) 'WINDOW 32)
    (set-x-property wm-spec-window-id '_NET_WM_NAME "Sawfish" 'UTF8_STRING 8)

    (set-x-property 'root '_NET_SUPPORTED supported-atoms 'ATOM 32)

    (let ((current-desktop (get-x-property 'root '_NET_CURRENT_DESKTOP)))
      (when (and current-desktop
		 (eq (car current-desktop) 'CARDINAL)
		 (>= (length (caddr current-desktop)) 1))
	(add-hook 'after-initialization-hook
		  ;; Don't do this yet, it can screw things up
		  (lambda ()
		    (select-workspace-from-first
		     (aref (caddr current-desktop) 0))))))

    (update-client-list-hints)
    (update-workspace-hints)

    (add-hook 'workspace-state-change-hook update-workspace-hints)
    (add-hook 'viewport-resized-hook update-workspace-hints)
    (add-hook 'viewport-moved-hook update-workspace-hints)
    (add-hook 'workarea-changed-hook update-workspace-hints)
    (add-hook 'configure-notify-hook update-on-configure-notify)

    ;; Better not expose work in progress.  map-notify-hook gets
    ;; called after this anyway.
    ;;(add-hook 'add-window-hook update-client-list-hints)
    (add-hook 'destroy-notify-hook update-client-list-hints)
    (add-hook 'map-notify-hook update-client-list-hints)
    (add-hook 'unmap-notify-hook update-client-list-hints)
    (add-hook 'after-restacking-hook update-client-list-hints)

    (add-hook 'before-add-window-hook honour-client-state)
    (add-hook 'add-window-hook update-client-state)
    (call-after-state-changed '(sticky shaded maximized stacking
                                       window-list-skip task-list-skip)
			      update-client-state)
    (call-after-state-changed 'sticky update-window-workspace-hints)

    (add-hook 'focus-in-hook update-focus-state)
    (add-hook 'focus-out-hook update-focus-state)

    (add-hook 'client-message-hook client-message-handler)
    (add-hook 'property-notify-hook property-change-handler)

    (add-hook 'before-exit-hook exit)

    (map-windows update-client-state))

  (define (exit)
    (destroy-window wm-spec-window-id)
    (delete-x-property 'root '_NET_SUPPORTING_WM_CHECK)
    (delete-x-property 'root '_NET_PROTOCOLS)
    (delete-x-property 'root '_NET_DESKTOP_GEOMETRY)
    (delete-x-property 'root '_NET_DESKTOP_VIEWPORT))

  (unless (or wm-spec-window-id batch-mode)
    (init)))
