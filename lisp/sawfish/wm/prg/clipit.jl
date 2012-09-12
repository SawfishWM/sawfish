;; clipit.jl -- clipit integration

;; Copyright (C) 2012 Christopher Roy Bratusek <nano@tuxfamily.org>

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

(define-structure sawfish.wm.prg.clipit

  (export start-clipit
          stop-clipit)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias clipit sawfish.wm.prg.clipit)

  (define %clipit-proc nil)

  (defcustom init-clipit nil
    "Whether to start clipit with Sawfish."
    :type boolean
    :group (misc apps))

  (define (start-clipit)
    "Start clipit. If a clipit process already exists, it's beeing killed.
     Configuration-file may be passed, if not, $HOME/.clipitrc is used."
    (if (program-exists-p "clipit")
        (progn
	  (when %clipit-proc (kill-process %clipit-proc))
          (setq %clipit-proc (make-process))
          (start-process %clipit-proc "clipit"))
      (display-message (format nil "clipit executable not found in PATH."))))

  (define (stop-clipit)
    "Stop clipit, if running."
    (when %clipit-proc (kill-process %clipit-proc))))
