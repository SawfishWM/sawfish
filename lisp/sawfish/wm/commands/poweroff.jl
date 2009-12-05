;; poweroff.jl -- reboot, halt, suspend or hibernate your machine
;;
;; Copyright (C) 2009 Christopher Roy Bratusek <zanghar@freenet.de>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.commands.poweroff

    (export poweroff)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.timers
	  rep.io.files
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.commands.launcher
	  sawfish.wm.windows)

  (define-structure-alias poweroff sawfish.wm.commands.poweroff)

  (defcustom reboot-command "sudo shutdown -r now"
    "The command used to reboot the computer."
    :type string
    :group (misc apps))

  (defcustom halt-command "sudo shutdown -h now"
    "The command used to halt the computer."
    :type string
    :group (misc apps))

  (defcustom suspend-command "sudo suspend"
    "The command used to suspend the computer."
    :type string
    :group (misc apps))

  (defcustom hibernate-command "sudo hibernate"
    "The command used to hibernate the computer."
    :type string
    :group (misc apps))

  (define (poweroff action)
    (case action
      ((reboot)
       (call-hook 'before-exit-hook)
       (map-windows delete-window)
       (system (format nil "%s &" reboot-command)))
      ((halt)
       (call-hook 'before-exit-hook)
       (map-windows delete-window)
       (system (format nil "%s &" halt-command)))
      ((suspend)   (system (format nil "%s &" suspend-command)))
      ((hibernate) (system (format nil "%s &" hibernate-command)))))

  ;;###autoload
  (define-command 'poweroff poweroff #:class 'default))
