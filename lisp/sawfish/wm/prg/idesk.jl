;; idesk.jl -- idesk (desktop icon manager) integration

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

(define-structure sawfish.wm.prg.idesk

  (export start-idesk
          stop-idesk)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias idesk sawfish.wm.prg.idesk)

  (define %idesk-proc nil)

  (defcustom init-idesk nil
    "Whether to start idesk with Sawfish."
    :type boolean
    :group (misc apps))

  (define (start-idesk)
    "Start idesk. If a idesk process already exists, it's being killed."
    (if (program-exists-p "idesk")
        (progn
	  (when %idesk-proc (kill-process %idesk-proc))
          (setq %idesk-proc (make-process))
          (start-process %idesk-proc "idesk"))
      (display-message (format nil "idesk executable not found in PATH."))))

  (define (stop-idesk)
    "Stop idesk, if running."
    (when %idesk-proc
      (kill-process %idesk-proc)
      (setq %idesk-proc nil))))
