;; conky.jl -- conky clipboard manager integration

;; Copyright (C) 2014 Christopher Roy Bratusek <nano@jpberlin.de>

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

(define-structure sawfish.wm.prg.conky

  (export start-conky
          stop-conky)

  (open rep
        rep.system
        rep.io.processes
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias conky sawfish.wm.prg.conky)

  (defcustom init-conky nil
    "Whether to start conky with Sawfish."
    :type boolean
    :group (misc apps))

  (defcustom conky-args ""
    "Extra arguments for launching conky"
    :type string
    :group (misc apps)
    :depends init-conky)

  (define (start-conky)
    "Start conky. If a conky process already exists, it's beeing killed."
    (if (program-exists-p "conky")
        (system "conky %s &" conky-args)
      (display-message (format nil "conky executable not found in PATH."))))

  (define (stop-conky)
    "Stop conky, if running."
    (system "killall -qw conky &"))

  (add-hook 'before-exit-hook stop-conky))
