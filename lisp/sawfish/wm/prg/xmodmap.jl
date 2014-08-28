;; xmodmap.jl -- Xmodmap integration

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

(define-structure sawfish.wm.prg.xmodmap

  (export load-xmodmap
	  save-keymap
	  restore-keymap)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
        rep.io.files
	rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias xmodmap sawfish.wm.prg.xmodmap)

  (defcustom init-xmodmap nil
    "Whether to start xmodmap with Sawfish."
    :type boolean
    :group (misc apps))

  (defcustom xmodmap-config (concat (getenv "HOME") "/.Xmodmap")
    "xmodmap configuration file to use."
    :type file
    :depends init-xmodmap
    :group (misc apps))

  (define (save-keymap #!key (file "~/.Xmodmap.orig"))
    "Save current keymap to ~/.Xmodmap.orig - or a path passed."
    (system (format nil "xmodmap -pke >  %s &" file)))

  (define (restore-keymap)
    "Restore keymap from ~/.Xmodmap.org."
    (if (file-exists-p "~/.Xmodmap.orig")
        (system "xmodmap ~/.Xmodmap.orig &")
      (display-message (format nil "~/.xmodmap.orig does not exist."))))

  (define (load-xmodmap #!key (config xmodmap-config))
    "Start xmodmap. configuration-file may be passed, else $HOME/.Xmodmap is used."
    (if (program-exists-p "xmodmap")
        (progn
	  (save-keymap)
	  (if (file-exists-p config)
	      (system (format nil "xmodmap %s &" config))
	    (display-message (format nil "given configuration file does not exist."))))
      (display-message (format nil "xmodmap executable not found in PATH.")))))
