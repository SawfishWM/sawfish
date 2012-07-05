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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

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

  (defcustom reboot-command "ssd --reboot"
    "The command used to reboot the computer."
    :type string
    :group (misc apps))

  (defcustom halt-command "ssd --shutdown"
    "The command used to halt the computer."
    :type string
    :group (misc apps))

  (defcustom suspend-command "ssd --suspend"
    "The command used to suspend the computer."
    :type string
    :group (misc apps))

  (defcustom hibernate-command "ssd --hibernate"
    "The command used to hibernate the computer."
    :type string
    :group (misc apps))

  (defcustom logout-command "ssd --logout"
    "The command used to logout the user."
    :type string
    :group (misc apps))

  (defcustom lockdown-command "ssd --lockdown"
    "The command used to lockdown the display."
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
      ((hibernate) (system (format nil "%s &" hibernate-command)))
      ((logout) (system (format nil "%s &" logout-command)))
      ((lockdown) (system (format nil "%s &" lockdown-command)))))

  ;;###autoload
  (define-command 'poweroff poweroff))
