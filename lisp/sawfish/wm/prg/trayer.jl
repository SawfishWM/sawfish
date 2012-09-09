;; trayer.jl -- trayer integration

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
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias trayer sawfish.wm.prg.trayer)

  (define %trayer-proc nil)

  (defcustom init-trayer nil
    "Whether to start trayer with Sawfish."
    :type boolean
    :group (misc apps))

  (defcustom trayer-expand 'false
    "Wether to expand trayer."
    :type (choice true false)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-edge 'top
    "What edge to place trayer at."
    :type (choice top right bottom left none)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-align 'left
    "Alignment of trayer."
    :type (choice left center right)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-widthtype 'percent
    "Unit for trayer's width."
    :type (choice request pixel percent)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-width 15
    "Width for trayer."
    :type number
    :depends init-trayer
    :range (0 . 2000)
    :group (misc apps))

  (defcustom trayer-heighttype 'pixel
    "Unit for trayer's height."
    :type (choice request pixel percent)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-height 28
    "Height for trayer."
    :type number
    :range (0 . 1000)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-margin 0
    "Margins for trayer."
    :type number
    :range (0 . 50)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-setdocktype 'false
    "Whether to set trayer's window-type to dock."
    :type (choice true false)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-setpartialstrut 'false
    "Whether to set partial strut."
    :type (choice true false)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-transparent 'false
    "Whether to make trayer transparent."
    :type (choice true false)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-alpha 0
    "Alpha channel value for trayer."
    :type (range (0 . 256))
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-tint 0
    "Tint value for trayer."
    :type (range (0 . 100))
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-distance 0
    "Distance value for trayer."
    :type number
    :range (0 . 200)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-distancefrom 0
    "DistanceFrom value for trayer."
    :type number
    :range (0 . 200)
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-padding 0
    "Padding value for trayer."
    :type (range (0 . 100))
    :depends init-trayer
    :group (misc apps))

  (defcustom trayer-monitor 0
    "Monitor to display trayer on."
    :type number
    :range (0 . 16)
    :depends init-trayer
    :group (misc apps))

  (define (start-trayer #!key
			(expand trayer-expand)
			(edge trayer-edge)
			(align trayer-align)
			(widthtype trayer-widthtype)
			(width trayer-width)
			(heighttype trayer-heighttype)
			(height trayer-height)
			(margin trayer-margin)
			(setdocktype trayer-setdocktype)
			(setpartialstrut trayer-setpartialstrut)
			(transparent trayer-transparent)
			(alpha trayer-alpha)
			(tint trayer-tint)
			(distance trayer-distance)
			(distancefrom trayer-distancefrom)
			(padding trayer-padding)
			(monitor trayer-monitor))
    "Start trayer. If a trayer process already exists, it's beeing killed.
All options of trayer are supported."
    (if (program-exists-p "trayer")
        (progn
	  (when %trayer-proc (kill-process %trayer-proc))
	  (setq %trayer-proc (make-process))
	  (start-process %trayer-proc "trayer" "--expand" (format nil "%s" expand) "--edge" (format nil "%s" edge)
            "--align" (format nil "%s" align) "--widthtype" (format nil "%s" widthtype) "--width" (number->string width)
            "--heighttype" (format nil "%s" heighttype) "--height" (number->string height)
            "--SetDockType" (format nil "%s" setdocktype) "--margin" (number->string margin)
            "--SetPartialStrut" (format nil "%s" setpartialstrut) "--monitor" (number->string monitor)
            "--alpha" (number->string alpha) "--tint" (number->string tint)
            "--distance" (number->string distance) "--distancefrom" (number->string distancefrom)
            "--padding" (number->string padding) "--transparent" (format nil "%s" transparent)))
      (display-message (format nil "trayer executable was not found in PATH."))))

  (define (stop-trayer)
    "Stop trayer, if running."
    (when %trayer-proc (kill-process %trayer-proc))))
