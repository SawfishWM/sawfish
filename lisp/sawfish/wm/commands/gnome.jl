;; gnome-commands.jl -- more GNOME stuff
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

(require 'gnome)

(defun gnome-set-hint (w bit)
  (let
      ((hints (get-x-property w '_WIN_HINTS)))
    (if hints
	(setq hints (aref (nth 2 hints) 0))
      (setq hints 0))
    (setq hints (logior bit hints))
    (set-x-property w '_WIN_HINTS (vector hints) 'CARDINAL 32)))

(defun gnome-clear-hint (w bit)
  (let
      ((hints (get-x-property w '_WIN_HINTS)))
    (if hints
	(setq hints (aref (nth 2 hints) 0))
      (setq hints 0))
    (setq hints (logand (lognot bit) hints))
    (set-x-property w '_WIN_HINTS (vector hints) 'CARDINAL 32)))

(defun gnome-toggle-hint (w bit)
  (let
      ((hints (get-x-property w '_WIN_HINTS)))
    (if hints
	(setq hints (aref (nth 2 hints) 0))
      (setq hints 0))
    (setq hints (logxor bit hints))
    (set-x-property w '_WIN_HINTS (vector hints) 'CARDINAL 32)))


;; commands

;;;###autoload
(defun gnome-toggle-skip-winlist (w)
  "Toggle the GNOME SKIP_WINLIST hint of the window."
  (interactive "%W")
  (gnome-toggle-hint w WIN_HINTS_SKIP_WINLIST))

;;;###autoload
(defun gnome-set-skip-winlist (w)
  "Set the GNOME SKIP_WINLIST hint of the window."
  (interactive "%W")
  (gnome-set-hint w WIN_HINTS_SKIP_WINLIST))

;;;###autoload
(defun gnome-clear-skip-winlist (w)
  "Unset the GNOME SKIP_WINLIST hint of the window."
  (interactive "%W")
  (gnome-clear-hint w WIN_HINTS_SKIP_WINLIST))

;;;###autoload
(defun gnome-toggle-skip-tasklist (w)
  "Toggle the GNOME SKIP_TASKLIST hint of the window."
  (interactive "%W")
  (gnome-toggle-hint w WIN_HINTS_SKIP_TASKLIST))

;;;###autoload
(defun gnome-set-skip-tasklist (w)
  "Set the GNOME SKIP_TASKLIST hint of the window."
  (interactive "%W")
  (gnome-set-hint w WIN_HINTS_SKIP_TASKLIST))

;;;###autoload
(defun gnome-clear-skip-tasklist (w)
  "Unset the GNOME SKIP_TASKLIST hint of the window."
  (interactive "%W")
  (gnome-clear-hint w WIN_HINTS_SKIP_TASKLIST))


;; extras

;;;###autoload
(defun gnome-logout ()
  (interactive)
  (system "save-session --quit &"))
