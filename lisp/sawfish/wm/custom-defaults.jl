;; custom-defaults.jl -- customize settings loaded if no user customization
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(custom-set-typed-variable (quote workspace-geometry) (quote (4 1 . 1)) (quote workspace-geometry))
(custom-set-typed-variable (quote tooltips-show-doc-strings) (quote ()) (quote boolean))
(custom-set-typed-variable (quote warp-to-selected-windows) (quote ()) (quote boolean))
(custom-set-typed-variable (quote cycle-warp-pointer) (quote ()) (quote boolean))
(custom-set-typed-variable (quote focus-mode) (quote click) (quote symbol))
(custom-set-keymap (quote window-keymap) (quote (keymap (move-window-interactively . "W-Button1-Move") (popup-window-menu . "W-Button2-Click1") (raise-lower-window . "W-Button3-Click1") (lower-window . "W-Down") (raise-window . "W-Up") (raise-and-pass-through-click . "Button1-Click1"))))
(custom-set-variable (quote tooltips-enabled) t (quote tooltips))
