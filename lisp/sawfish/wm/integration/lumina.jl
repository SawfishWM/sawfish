;; lumina.jl -- LUMINA integration

;; Copyright (C) 2014 Christopher Bratusek <nano@jpberlin.de>

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.integration.lumina

    (export detect-lumina
            lumina-window-matchers)

    (open rep
	  rep.system
          sawfish.wm.menus
	  sawfish.wm.misc
          sawfish.wm.windows
	  sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.commands.launcher
	  sawfish.wm.ext.match-window)

  (define-structure-alias lumina-int sawfish.wm.integration.lumina)

  (define (lumina-window-matchers)
    ;; window matchers so we properly interact with lumina-panel
    (add-window-matcher '((WM_CLASS . "^Lumina Desktop Environment/lumina-desktop$"))
     '((opacity . 100)
       (never-tile . t))))

  (define (init)
      (setq desktop-environment "lumina")
      (setq want-poweroff-menu nil)

      ;; invoke the LUMINA terminal instead of xterm
      (unless (variable-customized-p 'xterm-program)
	(setq xterm-program "xterm"))

      ;; use the LUMINA filemanager
      (unless (variable-customized-p 'filemanager-program)
	(setq filemanager-program "lumina-fm")))

  ;; Returns nil if lumina is not found.
  ;; If detected, returns t, and do also lumina support init.
  (define (detect-lumina)
    (when (or (equal (getenv "XDG_CURRENT_DESKTOP") "LUMINA")
	      (equal (getenv "XDG_CURRENT_DESKTOP") "Lumina")
	      (equal (getenv "DESKTOP_SESSION") "sawfish-lumina")
	      (get-window-by-class "Lumina" #:regex t)
	      (get-window-by-class "Lumina Desktop Environment" #:regex t))
      (init)
      t)))
