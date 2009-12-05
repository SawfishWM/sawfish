;; standalone.jl

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.integration.standalone

  (export add-extra-session-menu)

  (open rep
        rep.system
        sawfish.wm.menus
        sawfish.wm.custom
        sawfish.wm.commands)

  (define-structure-alias standalone-int sawfish.wm.integration.standalone)

  (defvar extra-session-menu t)

  (define (add-extra-session-menu)
    (if extra-session-menu
      (let ((menu (assoc (_ "Sessi_on") root-menu)))
        (when menu
          (nconc menu `(()
                        (,(_ "_Reboot System")
                         (poweroff 'reboot))
                        (,(_ "_Shutdown System")
                         (poweroff 'halt))
                        (,(_ "S_uspend System")
                         (poweroff 'suspend))
                        (,(_ "_Hibernate System")
                         (poweroff 'hibernate)))))))))
