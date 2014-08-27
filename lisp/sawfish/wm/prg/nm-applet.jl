;; nm-applet.jl -- network manager applet integration

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

(define-structure sawfish.wm.prg.nm-applet

  (export start-nm-applet
          stop-nm-applet)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias nm-applet sawfish.wm.prg.nm-applet)

  (define %nm-applet-proc nil)

  (defcustom init-nm-applet nil
    "Whether to start nm-applet with Sawfish."
    :type boolean
    :group (misc apps))

  (define (start-nm-applet)
    "Start nm-applet. If a nm-applet process already exists, it's beeing killed."
    (if (program-exists-p "nm-applet")
        (progn
	  (when %nm-applet-proc (kill-process %nm-applet-proc))
          (setq %nm-applet-proc (make-process))
          (start-process %nm-applet-proc "nm-applet"))
      (display-message (format nil "nm-applet executable not found in PATH."))))

  (define (stop-nm-applet)
    "Stop nm-applet, if running."
    (when %nm-applet-proc
      (kill-process %nm-applet-proc)
      (setq %nm-applet-proc nil))))
