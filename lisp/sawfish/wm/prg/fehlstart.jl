;; fehlstart.jl -- fehlstart integration

;; Copyright (C) 2012 Christopher Roy Bratusek <nano@jpberlin.de>

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

(define-structure sawfish.wm.prg.fehlstart

  (export start-fehlstart
          stop-fehlstart)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias fehlstart sawfish.wm.prg.fehlstart)

  (define %fehlstart-proc nil)

  (defcustom init-fehlstart nil
    "Whether to start fehlstart with Sawfish."
    :type boolean
    :group (misc apps))

  (define (start-fehlstart )
    "Start fehlstart. If a fehlstart process already exists, it's beeing killed."
    (if (program-exists-p "fehlstart")
        (progn
	  (when %fehlstart-proc (kill-process %fehlstart-proc))
          (setq %fehlstart-proc (make-process))
          (start-process %fehlstart-proc "fehlstart"))
      (display-message (format nil "fehlstart executable not found in PATH."))))

  (define (stop-fehlstart)
    "Stop fehlstart, if running."
    (when %fehlstart-proc
      (kill-process %fehlstart-proc)
      (setq %fehlstart-proc nil))))
