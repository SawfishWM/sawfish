;; xmobar.jl -- xmobar integration

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

(define-structure sawfish.wm.prg.xmobar

  (export start-xmobar
          stop-xmobar)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
	rep.io.files
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias xmobar sawfish.wm.prg.xmobar)

  (define %xmobar-proc nil)

  (defcustom init-xmobar nil
    "Whether to start xmobar with Sawfish."
    :type boolean
    :group (misc apps))

  (defcustom xmobar-config (concat (getenv "HOME") "/.xmobarrc")
    "xmobar configuration file to use."
    :type file
    :depends init-xmobar
    :group (misc apps))

  (define (start-xmobar #!key (config xmobar-config))
    "Start xmobar. If a xmobar process already exists, it's beeing killed.
     Configuration-file may be passed, if not, $HOME/.xmobarrc is used."
    (if (program-exists-p "xmobar")
        (progn
	  (when %xmobar-proc (kill-process %xmobar-proc))
          (setq %xmobar-proc (make-process))
          (if (file-exists-p config)
	      (start-process %xmobar-proc "xmobar" config)
	    (display-message (format nil "given configuration filie does not exist."))))
      (display-message (format nil "xmobar executable not found in PATH."))))

  (define (stop-xmobar)
    "Stop xmobar, if running."
    (when %xmobar-proc (kill-process %xmobar-proc))))
