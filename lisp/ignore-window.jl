;; ignore-window.jl -- controlling the ignored property
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

;; originally by Julian Missig <julian@linuxpower.org> (X-ViRGE), with
;; some extra hacking/renaming

;;;###autoload
(defun make-window-ignored (w)
  "Ignore the window."
  (interactive "%W")
  (unless (window-get w 'ignored)
    (set-window-frame w nil-frame)
    (window-put w 'current-frame-style nil)
    (window-put w 'ignored t)
    (call-window-hook 'window-state-change-hook w (list '(ignored)))
    (call-hook 'workspace-state-change-hook)))

;;;###autoload
(defun make-window-not-ignored (w)
  "Unignore the window."
  (interactive "%W")
  (when (window-get w 'ignored)
    (window-put w 'ignored nil)
    (set-frame-for-window w t)
    (call-window-hook 'window-state-change-hook w (list '(ignored)))
    (call-hook 'workspace-state-change-hook)))

;;;###autoload
(defun toggle-window-ignored (w)
  "Toggle whether a window is ignored or not."
  (interactive "%W")
  (if (window-get w 'ignored)
      (make-window-not-ignored w)
    (make-window-ignored w)))
