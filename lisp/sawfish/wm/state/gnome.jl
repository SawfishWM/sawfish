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

(require 'maximize)
(provide 'gnome)

(defconst WIN_STATE_STICKY 1)
(defconst WIN_STATE_MAXIMIZED_VERT 4)
(defconst WIN_STATE_MAXIMIZED_HORIZ 8)
(defconst WIN_STATE_SHADED 32)

(defconst WIN_LAYER_NORMAL 4)

(defvar gnome-window-id nil)

(defvar gnome-supported-protocols [_WIN_CLIENT_LIST _WIN_WORKSPACE
				   _WIN_WORKSPACE_COUNT _WIN_STATE
				   _WIN_LAYER])

(defun gnome-add-window (w)
  (when (string-match "^gmc|panel$" (window-name w))
    (window-put w 'focus-proxy-click t))
  (gnome-set-client-list))

(defun gnome-set-client-list ()
  (let
      (clients vec)
    (mapc #'(lambda (w)
	      (when (and (windowp w) (window-mapped-p w)
			 (not (window-get w 'ignored)))
		(setq clients (cons (window-id w) clients))))
	  (managed-windows))
    (setq vec (apply 'vector clients))
    (set-x-property 'root '_WIN_CLIENT_LIST vec 'CARDINAL 32)))

(defun gnome-set-workspace ()
  (let
      ((total 0))
    (mapc #'(lambda (space)
	      (when (eq space current-workspace)
		(set-x-property 'root '_WIN_WORKSPACE
				(vector total) 'CARDINAL 32))
	      (mapc #'(lambda (w)
			(set-x-property w '_WIN_WORKSPACE
					(vector total) 'CARDINAL 32))
		    (cdr space))
	      (setq total (1+ total)))
	  workspace-list)
    (set-x-property 'root '_WIN_WORKSPACE_COUNT (vector total) 'CARDINAL 32)))

(defun gnome-set-client-state (w)
  (let
      ((state 0))
    (when (window-get w 'sticky)
      (setq state (logior state WIN_STATE_STICKY)))
    (when (window-get w 'shaded)
      (setq state (logior state WIN_STATE_SHADED)))
    (when (window-maximized-vertically-p w)
      (setq state (logior state WIN_STATE_MAXIMIZED_VERT)))
    (when (window-maximized-horizontally-p w)
      (setq state (logior state WIN_STATE_MAXIMIZED_HORIZ)))
    (set-x-property w '_WIN_STATE (vector state) 'CARDINAL 32)
    (when (window-get w 'depth)
      (set-x-property w '_WIN_LAYER
		      (vector (+ (window-get w 'depth) WIN_LAYER_NORMAL))
		      'CARDINAL 32))))

(defun gnome-honour-client-state (w)
  (let
      ((state (get-x-property w '_WIN_STATE))
       (layer (get-x-property w '_WIN_LAYER))
       (space (get-x-property w '_WIN_WORKSPACE))
       bits)
    (when (eq (car state) 'CARDINAL)
      (setq bits (aref (nth 2 state) 0))
      (unless (zerop (logand bits WIN_STATE_STICKY))
	(unless (window-get w 'sticky)
	  (toggle-window-sticky w)))
      (unless (zerop (logand bits WIN_STATE_SHADED))
	(unless (window-get w 'shaded)
	  (shade-window w)))
      (unless (zerop (logand bits WIN_STATE_MAXIMIZED_VERT))
	(unless (window-maximized-vertically-p w)
	  (maximize-window-vertically w)))
      (unless (zerop (logand bits WIN_STATE_MAXIMIZED_HORIZ))
	(unless (window-maximized-horizontally-p w)
	  (maximize-window-horizontally w))))
    (when layer
      (setq layer (aref (nth 2 layer) 0))
      (set-window-depth w (- layer WIN_LAYER_NORMAL)))
    (when space
      (ws-add-window-to-space-by-index w (aref (nth 2 space) 0)))
    w))

(defun gnome-client-message-handler (w type data)
  (cond ((eq type '_WIN_WORKSPACE)
	 (select-workspace (aref data 0))
	 t)
	((eq type '_WIN_STATE)
	 (let
	     ((mask (aref data 0))
	      (values (aref data 1))
	      tem)
	   (unless (zerop (logand mask WIN_STATE_STICKY))
	     (setq tem (window-get w 'sticky))
	     (if (or (and (not tem)
			  (not (zerop (logand values WIN_STATE_STICKY))))
		     (and tem (zerop (logand values WIN_STATE_STICKY))))
		 (toggle-window-sticky w)))
	   (unless (zerop (logand mask WIN_STATE_SHADED))
	     (setq tem (window-get w 'shaded))
	     (if (or (and (not tem)
			  (not (zerop (logand values WIN_STATE_SHADED))))
		     (and tem (zerop (logand values WIN_STATE_SHADED))))
		 (toggle-window-shaded w)))
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
	((eq type '_WIN_LAYER)
	 (set-window-depth w (- (aref data 0) WIN_LAYER_NORMAL))
	 t)))

(defun gnome-event-proxyer ()
  (when (and (current-event) (eq (current-event-window) 'root))
    (let
	((event (event-name (current-event))))
      ;; XXX should check that there's no binding of the
      ;; XXX corresponding event in the root-window-keymap or
      ;; XXX the global-keymap
      (when (string-match "-(Click1|Off)$" event)
	;; send with SubstructureNotifyMask
	(proxy-current-event gnome-window-id (lsh 1 19))
	t))))


;; initialisation

(defun gnome-init ()
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

  (set-x-property 'root '_WIN_AREA (vector 0 0) 'CARDINAL 32)
  (set-x-property 'root '_WIN_AREA_COUNT (vector 1 1) 'CARDINAL 32)
  (delete-x-property 'root '_WIN_WORKSPACE_NAMES)

  (add-hook 'workspace-state-change-hook 'gnome-set-workspace)

  (add-hook 'add-window-hook 'gnome-add-window)

  (add-hook 'destroy-notify-hook 'gnome-set-client-list)
  (add-hook 'map-notify-hook 'gnome-set-client-list)
  (add-hook 'umap-notify-hook 'gnome-set-client-list)

  (add-hook 'window-state-change-hook 'gnome-set-client-state)
  (add-hook 'before-add-window-hook 'gnome-honour-client-state)
  (add-hook 'add-window-hook 'gnome-set-client-state t)

  (add-hook 'client-message-hook 'gnome-client-message-handler)
  (add-hook 'unbound-key-hook 'gnome-event-proxyer)
  (add-hook 'before-exit-hook 'gnome-exit))

(defun gnome-exit ()
  (destroy-window gnome-window-id)
  (delete-x-property 'root '_WIN_SUPPORTING_WM_CHECK)
  (delete-x-property 'root '_WIN_PROTOCOLS))

(unless (or gnome-window-id batch-mode)
  (gnome-init))
