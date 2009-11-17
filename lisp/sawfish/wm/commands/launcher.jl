;; user.jl -- command to launch an xterm/brower and more
;;
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
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

(define-structure sawfish.wm.commands.user

    (export xterm
	    browser)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.timers
	  rep.io.files
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.commands)

  (defgroup apps "Default Applications" :group misc)

  (defcustom xterm-program "xterm"
    "The program launched by the `xterm' command."
    :type string
    :group (misc apps))

  (defcustom browser-program "www-browser"
    "The program launched by the `browser' command."
    :type string
    :group (misc apps))

  (define (xterm #!optional command)
    "Start a new xterm."
    (if (not command)
	(system (format nil "%s >/dev/null 2>&1 </dev/null &"
			xterm-program))
      (system (format nil "%s -e %s >/dev/null 2>&1 </dev/null &"
		      xterm-program command))))

  (define (browser #!optional website)
    "Start a new browser instance"
    (if (not website)
        (system (format nil "%s >/dev/null 2>&1 </dev/null &"
			browser-program))
      (system (format nil "%s %s >/dev/null 2>&1 </dev/null &"
                      browser-program website))))

  ;;###autoload
  (define-command 'xterm xterm #:class 'default)
  (define-command 'browser browser #:class 'default))
