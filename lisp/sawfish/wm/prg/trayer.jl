;; trayer.jl -- Razor-Qt integration

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

(define-structure sawfish.wm.prg.trayer

  (export start-trayer
          stop-trayer)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.util.misc
        sawfish.wm.misc)

  (define-structure-alias trayer sawfish.wm.prg.trayer)

  (define %trayer-proc nil)

  ;; SAWFISHRC
  ;; (require 'sawfish.wm.prg.trayer)
  ;; (add-hook 'after-initialization-hook start-trayer t)

  (define (start-trayer #!key
			(expand "false")
			(edge "top")
			(align "left")
			(widthtype "percent")
			(width 15)
			(heighttype "pixel")
			(height 28)
			(margin 0)
			(setdocktype "false")
			(setpartialstrut "false")
			(transparent "false")
			(alpha 0)
			(tint 0)
			(distance 0)
			(distancefrom 0)
			(padding 0)
			(monitor 0))
    "Start trayer. If a trayer process already exists, it's beeing killed.
     All options of trayer are supported. Note that boolean values must be
     passed as strings (true/false), not booleans (t/nil)."
    (if (program-exists-p "trayer")
        (progn
	  (when %trayer-proc (kill-process %trayer-proc))
	  (setq %trayer-proc (make-process))
	  (start-process %trayer-proc "trayer" "--expand" expand "--edge" edge
            "--align" align "--widthtype" widthtype "--width" (number->string width)
            "--heighttype" heighttype "--height" (number->string height)
            "--SetDockType" setdocktype "--margin" (number->string margin)
            "--SetPartialStrut" setpartialstrut "--monitor" (number->string monitor)
            "--alpha" (number->string alpha) "--tint" (number->string tint)
            "--distance" (number->string distance) "--distancefrom" (number->string distancefrom)
            "--padding" (number->string padding) "--transparent" transparent))
      (display-message (format nil "trayer executable was not found in PATH."))))

  (define (stop-trayer)
    "Stop trayer, if running."
    (when %trayer-proc (kill-process %trayer-proc))))
