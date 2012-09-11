;; pancake.jl -- pancake integration

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

(define-structure sawfish.wm.prg.pancake

  (export start-pancake
          stop-pancake)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias pancake sawfish.wm.prg.pancake)

  (define %pancake-proc nil)

  (defcustom init-pancake nil
    "Whether to start pancake with Sawfish."
    :type boolean
    :group (misc apps))

  (defcustom pancake-config (concat (getenv "HOME") "/.pancakerc")
    "pancake configuration file to use."
    :type file
    :depends init-pancake
    :group (misc apps))

  (define (start-pancake #!key (config pancake-config))
    "Start pancake. If a pancake process already exists, it's beeing killed.
     Configuration-file may be passed, if not, $HOME/.pancakerc is used."
    (if (program-exists-p "pancake")
        (progn
	  (when %pancake-proc (kill-process %pancake-proc))
          (setq %pancake-proc (make-process))
          (if (file-exists-p config)
	      (start-process %pancake-proc "pancake" config)
	    (display-message (format nil "given configuration filie does not exist."))))
      (display-message (format nil "pancake executable not found in PATH."))))

  (define (stop-pancake)
    "Stop pancake, if running."
    (when %pancake-proc (kill-process %pancake-proc))))
