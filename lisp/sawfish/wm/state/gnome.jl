;; gnome.jl -- minimal GNOME compliance
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.state.gnome

    (export WIN_STATE_STICKY
	    WIN_STATE_MAXIMIZED_VERT
	    WIN_STATE_MAXIMIZED_HORIZ
	    WIN_STATE_HIDDEN
	    WIN_STATE_SHADED
	    WIN_STATE_FIXED_POSITION
	    WIN_LAYER_NORMAL
	    WIN_HINTS_SKIP_FOCUS
	    WIN_HINTS_SKIP_WINLIST
	    WIN_HINTS_SKIP_TASKLIST
	    WIN_HINTS_FOCUS_ON_CLICK)

    (open rep
	  rep.regexp
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.events
	  sawfish.wm.misc
	  sawfish.wm.menus
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.stacking
	  sawfish.wm.state.maximize
	  sawfish.wm.state.iconify
	  sawfish.wm.state.shading)

  (define-structure-alias gnome sawfish.wm.state.gnome)

  (defconst WIN_STATE_STICKY 1)
  (defconst WIN_STATE_MAXIMIZED_VERT 4)
  (defconst WIN_STATE_MAXIMIZED_HORIZ 8)
  (defconst WIN_STATE_HIDDEN 16)
  (defconst WIN_STATE_SHADED 32)
  (defconst WIN_STATE_FIXED_POSITION 256)

  (defconst WIN_LAYER_NORMAL 4)

  (defconst WIN_HINTS_SKIP_FOCUS 1)
  (defconst WIN_HINTS_SKIP_WINLIST 2)
  (defconst WIN_HINTS_SKIP_TASKLIST 4)
  (defconst WIN_HINTS_FOCUS_ON_CLICK 16)

  (defvar gnome-window-id nil)

  (defvar gnome-supported-protocols [_WIN_CLIENT_LIST _WIN_WORKSPACE
				     _WIN_WORKSPACE_COUNT _WIN_STATE
				     _WIN_LAYER])

  ;; this is needed since the gnome tasklist applet doesn't honour
  ;; the _WIN_HIDDEN property (?)
  (defvar gnome-ignored-windows-in-client-list t)

  (define (gnome-set-client-list)
    (let (clients vec)
      (map-windows (lambda (w)
		     (when (and (windowp w) (window-mapped-p w)
				(or gnome-ignored-windows-in-client-list
				    (not (window-get w 'ignored))))
		       (setq clients (cons (window-id w) clients)))))
      (setq vec (apply vector clients))
      (set-x-property 'root '_WIN_CLIENT_LIST vec 'CARDINAL 32)))

  (define current-workspace-index nil)
  (define current-workspace-count 0)
  (define current-workspace-names nil)
  (define current-area nil)
  (define current-area-count nil)

  (define (gnome-set-workspace)
    (let* ((limits (workspace-limits))
	   (port (screen-viewport))
	   (port-size viewport-dimensions)
	   (total-workspaces (1+ (- (cdr limits) (car limits)))))
      ;; apparently some pagers don't like it if we place windows
      ;; on (temporarily) non-existent workspaces
      (when (< current-workspace-count total-workspaces)
	(setq current-workspace-count total-workspaces)
	(set-x-property 'root '_WIN_WORKSPACE_COUNT
			(vector current-workspace-count) 'CARDINAL 32))
      (map-windows
       (lambda (w)
	 (let
	     ;; XXX the gnome-wm standard sucks..
	     ((space (and (not (window-get w 'sticky))
			  (window-get w 'swapped-in)))
	      (w-port (and (not (window-get w 'viewport-sticky))
			   (window-viewport w))))
	   (if space
	       (set-x-property w '_WIN_WORKSPACE
			       (vector (- space (car limits))) 'CARDINAL 32)
	     (delete-x-property w '_WIN_WORKSPACE))
	   (if w-port
	       (set-x-property w '_WIN_AREA
			       (vector (car w-port) (cdr w-port))
			       'CARDINAL 32)
	     (delete-x-property w '_WIN_AREA)))))
      (unless (equal current-workspace-index
		     (- current-workspace (car limits)))
	(setq current-workspace-index (- current-workspace (car limits)))
	(set-x-property 'root '_WIN_WORKSPACE
			(vector current-workspace-index) 'CARDINAL 32))
      (when (> current-workspace-count total-workspaces)
	(setq current-workspace-count total-workspaces)
	(set-x-property 'root '_WIN_WORKSPACE_COUNT
			(vector current-workspace-count) 'CARDINAL 32))
      (unless (equal current-workspace-names workspace-names)
	(setq current-workspace-names workspace-names)
	(set-x-text-property 'root '_WIN_WORKSPACE_NAMES
			     (apply vector workspace-names)))
      (unless (equal current-area port)
	(setq current-area port)
	(set-x-property 'root '_WIN_AREA (vector (car port) (cdr port))
			'CARDINAL 32))
      (unless (equal current-area-count port-size)
	(setq current-area-count port-size)
	(set-x-property 'root '_WIN_AREA_COUNT (vector (car port-size)
						       (cdr port-size))
			'CARDINAL 32))))

  (define (gnome-set-client-state w)
    (let ((state 0))
      (when (window-get w 'sticky)
	(setq state (logior state WIN_STATE_STICKY)))
      (when (window-get w 'shaded)
	(setq state (logior state WIN_STATE_SHADED)))
      (when (window-get w 'fixed-position)
	(setq state (logior state WIN_STATE_FIXED_POSITION)))
      (when (window-maximized-vertically-p w)
	(setq state (logior state WIN_STATE_MAXIMIZED_VERT)))
      (when (window-maximized-horizontally-p w)
	(setq state (logior state WIN_STATE_MAXIMIZED_HORIZ)))
      (when (window-get w 'ignored)
	(setq state (logior state WIN_STATE_HIDDEN)))
      (set-x-property w '_WIN_STATE (vector state) 'CARDINAL 32)
      (when (window-get w 'depth)
	(set-x-property w '_WIN_LAYER
			(vector (+ (window-get w 'depth) WIN_LAYER_NORMAL))
			'CARDINAL 32))))

  (define (gnome-honour-client-state w)
    (let ((class (get-x-text-property w 'WM_CLASS)))
      (when (and class (>= (length class) 2))
	(cond ((and (string= (aref class 1) "Panel")
		    (string= (aref class 0) "panel"))
	       ;; XXX I don't think the GNOME hints specify these things...
	       (window-put w 'focus-click-through t)
	       (window-put w 'avoid t))
	      ((and (string= (aref class 1) "Nautilus")
		    (string= (aref class 0) "desktop_window"))
	       ;; XXX ...or these
	       (window-put w 'desktop t)
	       (window-put w 'keymap root-window-keymap)))))
    (let ((state (get-x-property w '_WIN_STATE))
	  (hints (get-x-property w '_WIN_HINTS))
	  (layer (get-x-property w '_WIN_LAYER))
	  (space (get-x-property w '_WIN_WORKSPACE))
	  bits)
      (when (eq (car state) 'CARDINAL)
	(setq bits (aref (nth 2 state) 0))
	(unless (zerop (logand bits WIN_STATE_STICKY))
	  (window-put w 'sticky t)
	  (window-put w 'sticky-viewport t))
	(unless (zerop (logand bits WIN_STATE_SHADED))
	  (window-put w 'shaded t))
	(unless (zerop (logand bits WIN_STATE_FIXED_POSITION))
	  (window-put w 'fixed-position t))
;;; XXX this doesn't work since the frame hasn't been created yet..
;      (unless (zerop (logand bits WIN_STATE_MAXIMIZED_VERT))
;	(unless (window-maximized-vertically-p w)
;	  (maximize-window-vertically w)))
;      (unless (zerop (logand bits WIN_STATE_MAXIMIZED_HORIZ))
;	(unless (window-maximized-horizontally-p w)
;	  (maximize-window-horizontally w)))
	)
      (when (eq (car hints) 'CARDINAL)
	(setq bits (aref (nth 2 hints) 0))
	(unless (zerop (logand bits WIN_HINTS_SKIP_FOCUS))
	  (window-put w 'never-focus t)
	  (window-put w 'ignored t)))
      (when layer
	(setq layer (aref (nth 2 layer) 0))
	(set-window-depth w (- layer WIN_LAYER_NORMAL)))
      (when (and space (not (window-workspaces w)))
	(set-window-workspaces w (list (aref (nth 2 space) 0))))))

  (define (gnome-client-message-handler w type data)
    (cond ((eq type '_WIN_WORKSPACE)
	   (let ((limits (workspace-limits)))
	     (select-workspace (+ (aref data 0) (car limits)))
	     t))
	  ((eq type '_WIN_AREA)
	   (set-screen-viewport (aref data 0) (aref data 1)))
	  ((and (eq type '_WIN_STATE) (windowp w))
	   (let ((mask (aref data 0))
		 (values (aref data 1))
		 tem)
	     (unless (zerop (logand mask WIN_STATE_STICKY))
	       (if (zerop (logand values WIN_STATE_STICKY))
		   (make-window-unsticky w)
		 (make-window-sticky w)))
	     (unless (zerop (logand mask WIN_STATE_SHADED))
	       (if (zerop (logand values WIN_STATE_SHADED))
		   (unshade-window w)
		 (shade-window w)))
	     (unless (zerop (logand mask WIN_STATE_FIXED_POSITION))
	       (window-put w 'fixed-position
			   (/= (logand values WIN_STATE_FIXED_POSITION) 0)))
	     (unless (zerop (logand mask WIN_STATE_MAXIMIZED_VERT))
	       (setq tem (window-maximized-vertically-p w))
	       (if (or (and (not tem) (not (zerop (logand values WIN_STATE_MAXIMIZED_VERT))))
		       (and tem (zerop (logand values WIN_STATE_MAXIMIZED_VERT))))
		   (maximize-window-vertically-toggle w)))
	     (unless (zerop (logand mask WIN_STATE_MAXIMIZED_HORIZ))
	       (setq tem (window-maximized-horizontally-p w))
	       (if (or (and (not tem) (not (zerop (logand values WIN_STATE_MAXIMIZED_HORIZ))))
		       (and tem (zerop (logand values WIN_STATE_MAXIMIZED_HORIZ))))
		   (maximize-window-horizontally-toggle w))))
	   t)
	  ((and (eq type '_WIN_LAYER) (windowp w))
	   (set-window-depth w (- (aref data 0) WIN_LAYER_NORMAL))
	   t)))

  (define (gnome-event-proxyer)
    (when (and (current-event) (eq (current-event-window) 'root))
      (let ((event (event-name (current-event))))
	;; only proxy Click1 or Off events, and only if we don't have
	;; a binding for an event that may follow in the same grab
	(cond ((and (string-match "^(.*)-Click1$" event)
		    (let ((mirror (lookup-event
				   (expand-last-match "\\1-Off"))))
		      (not (or (search-keymap mirror global-keymap)
			       (search-keymap mirror root-window-keymap)))))
	       ;; send with SubstructureNotifyMask
	       (proxy-current-event gnome-window-id (lsh 1 19))
	       t)
	      ((and (string-match "^(.*)-Off$" event)
		    (let ((mirrors
			   (mapcar (lambda (x)
				     (lookup-event
				      (concat (expand-last-match "\\1-") x)))
				   '("Click1" "Click2" "Click3" "Move"))))
		      (catch 'out
			(mapc (lambda (ev)
				(when (or (search-keymap ev global-keymap)
					  (search-keymap ev root-window-keymap))
				  (throw 'out nil)))
			      mirrors)
			t)))
	       ;; send with SubstructureNotifyMask
	       (proxy-current-event gnome-window-id (lsh 1 19))
	       t)))))


;;; initialisation

  (define (gnome-init)
    (setq gnome-window-id (create-window 'root -200 -200 5 5))

    (set-x-property 'root '_WIN_SUPPORTING_WM_CHECK
		    (vector gnome-window-id) 'CARDINAL 32)
    (set-x-property gnome-window-id '_WIN_SUPPORTING_WM_CHECK
		    (vector gnome-window-id) 'CARDINAL 32)

    (set-x-property 'root '_WIN_DESKTOP_BUTTON_PROXY
		    (vector gnome-window-id) 'CARDINAL 32)
    (set-x-property gnome-window-id '_WIN_DESKTOP_BUTTON_PROXY
		    (vector gnome-window-id) 'CARDINAL 32)

    (set-x-property 'root '_WIN_PROTOCOLS
		    gnome-supported-protocols 'ATOM 32)

    (let ((port (screen-viewport)))
      (set-x-property 'root '_WIN_AREA
		      (vector (car port) (cdr port)) 'CARDINAL 32)
      (set-x-property 'root '_WIN_AREA_COUNT
		      (vector (car viewport-dimensions)
			      (cdr viewport-dimensions)) 'CARDINAL 32)

      ;; XXX I'm using this property to tell desk-guide to move
      ;; XXX the current area on all desktops at once
      ;; XXX This is totally non-standard and may change..
      (set-x-property 'root '_WIN_UNIFIED_AREA (vector 1) 'CARDINAL 32))

    (delete-x-property 'root '_WIN_WORKSPACE_NAMES)

    (add-hook 'workspace-state-change-hook gnome-set-workspace)
    (add-hook 'viewport-resized-hook gnome-set-workspace)
    (add-hook 'viewport-moved-hook gnome-set-workspace)

    (add-hook 'add-window-hook gnome-set-client-list)
    (add-hook 'destroy-notify-hook gnome-set-client-list)
    (add-hook 'map-notify-hook gnome-set-client-list)
    (add-hook 'unmap-notify-hook gnome-set-client-list)
    (add-hook 'workspace-state-change-hook gnome-set-client-list)

    (add-hook 'before-add-window-hook gnome-honour-client-state)
    (add-hook 'add-window-hook gnome-set-client-state)
    (call-after-state-changed '(sticky shaded maximized ignored stacking)
			      gnome-set-client-state)

    (add-hook 'client-message-hook gnome-client-message-handler)
    (add-hook 'unbound-key-hook gnome-event-proxyer)
    (add-hook 'before-exit-hook gnome-exit))

  (define (gnome-exit)
    (destroy-window gnome-window-id)
    (delete-x-property 'root '_WIN_SUPPORTING_WM_CHECK)
    (delete-x-property 'root '_WIN_PROTOCOLS)
    (delete-x-property 'root '_WIN_AREA)
    (delete-x-property 'root '_WIN_AREA_COUNT)
    (delete-x-property 'root '_WIN_UNIFIED_AREA))

  (unless (or gnome-window-id batch-mode)
    (gnome-init)
    (require 'sawfish.wm.gnome.match-window))

  (add-window-menu-toggle (_ "In _window list") 'gnome-toggle-skip-winlist)
  (add-window-menu-toggle (_ "In _task list") 'gnome-toggle-skip-tasklist))
