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

(provide 'gnome)

(defconst WIN_STATE_STICKY 1)

(defvar gnome-window-names "^(gmc|panel)$")

(defvar gnome-window-id nil)

(defvar gnome-supported-protocols [_WIN_CLIENT_LIST _WIN_WORKSPACE
				   _WIN_WORKSPACE_COUNT _WIN_STATE])

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
	      (when (eq space ws-current-workspace)
		(set-x-property 'root '_WIN_WORKSPACE
				(vector total) 'CARDINAL 32))
	      (mapc #'(lambda (w)
			(set-x-property w '_WIN_WORKSPACE
					(vector total) 'CARDINAL 32))
		    (cdr space))
	      (setq total (1+ total)))
	  ws-workspaces)
    (set-x-property 'root '_WIN_WORKSPACE_COUNT (vector total) 'CARDINAL 32)))

(defun gnome-set-client-state (w)
  (let
      ((state 0))
    (when (window-get w 'sticky)
      (setq state (logior state WIN_STATE_STICKY)))
    (set-x-property w '_WIN_STATE (vector state) 'CARDINAL 32)))

(defun gnome-honour-client-state (w)
  (let
      ((state (get-x-property w '_WIN_STATE)))
    (when (eq (car state) 'CARDINAL)
      (unless (zerop (logand (aref (nth 2 state) 0) WIN_STATE_STICKY))
	(unless (window-get w 'sticky)
	  (toggle-window-sticky w))))
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
		 (toggle-window-sticky w))))
	 t)))

(defun gnome-event-proxyer ()
  (when (eq (current-event-window) 'root)
    (let
	((event (event-name (current-event))))
      ;; XXX should check that there's no binding of the
      ;; XXX corresponding event in the root-window-keymap or
      ;; XXX the global-keymap
      (when (string-match "-(Click1|Off)$" event)
	(proxy-current-event gnome-window-id)
	t))))


;; initialisation

(defun gnome-init ()
  (setq ignored-window-names (cons gnome-window-names ignored-window-names))
  (setq sticky-window-names (cons gnome-window-names sticky-window-names))
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

  (delete-x-property 'root '_WIN_AREA)
  (delete-x-property 'root '_WIN_AREA_COUNT)
  (delete-x-property 'root '_WIN_WORKSPACE_NAMES)

  (add-hook 'enter-workspace-hook 'gnome-set-workspace)
  (add-hook 'add-workspace-hook 'gnome-set-workspace)
  (add-hook 'delete-workspace-hook 'gnome-set-workspace)
  (add-hook 'add-to-workspace-hook 'gnome-set-workspace)
  (add-hook 'remove-from-workspace-hook 'gnome-set-workspace)

  (add-hook 'add-window-hook 'gnome-set-client-list)
  (add-hook 'destroy-notify-hook 'gnome-set-client-list)
  (add-hook 'map-notify-hook 'gnome-set-client-list)
  (add-hook 'umap-notify-hook 'gnome-set-client-list)

  (add-hook 'client-message-hook 'gnome-client-message-handler)

  (add-hook 'window-state-change-hook 'gnome-set-client-state)
  (add-hook 'add-window-hook 'gnome-honour-client-state)

  (add-hook 'unbound-key-hook 'gnome-event-proxyer)

  (add-hook 'before-exit-hook 'gnome-exit))

(defun gnome-exit ()
  (destroy-window gnome-window-id)
  (delete-x-property 'root '_WIN_SUPPORTING_WM_CHECK)
  (delete-x-property 'root '_WIN_PROTOCOLS))

(unless gnome-window-id
  (gnome-init))
