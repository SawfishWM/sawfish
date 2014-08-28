;; xsettingsd.jl -- xsettingsd integration

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

(define-structure sawfish.wm.prg.xsettingsd

  (export start-xsettingsd
          stop-xsettingsd
          dump-xsettings)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias xsettingsd sawfish.wm.prg.xsettingsd)

  (define %xsettingsd-proc nil)

  (defcustom init-xsettingsd nil
    "Whether to start xsettingsd with Sawfish."
    :type boolean
    :group (misc apps))

  (defcustom xsettingsd-config (concat (getenv "HOME") "/.xsettingsd")
    "xsettingsd configuration file to use."
    :type file
    :depends init-xsettingsd
    :group (misc apps))

  (define (dump-xsettings #!key (config xsettingsd-config))
    (if (program-exists-p "dump_xsettings")
	(system (format nil "dump_xsettings > %s &" config))
      (message (format nil "dump_xsettings executable not found in PATH."))))

  (define (start-xsettingsd #!key (config xsettingsd-config))
    "Start xsettingsd. If a xsettingsd process already exists, it's beeing killed.
     Configuration-file may be passed, if not, $HOME/.xsettingsd is used."
    (if (program-exists-p "xsettingsd")
        (progn
	  (when %xsettingsd-proc (kill-process %xsettingsd-proc))
          (setq %xsettingsd-proc (make-process))
          (if (file-exists-p config)
	      (start-process %xsettingsd-proc "xsettingsd" config)
	    (display-message (format nil "given configuration filie does not exist.
\n use (dump-xsettings) while gnome-settings-daemon is running  to generate default configuration."))))
      (display-message (format nil "xsettingsd executable not found in PATH."))))

  (define (stop-xsettingsd)
    "Stop xsettingsd, if running."
    (when %xsettingsd-proc
      (kill-process %xsettingsd-proc)
      (setq %xsettingsd-proc nil))))
