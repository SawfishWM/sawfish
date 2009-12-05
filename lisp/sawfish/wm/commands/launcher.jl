;; launcher.jl -- command to launch external apps, xterm & browser
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

;; This module was renamed from xterm in Sawfish-1.6.
(define-structure sawfish.wm.commands.launcher

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

  (define-structure-alias launcher sawfish.wm.commands.launcher)

  (defgroup apps "External Applications" :group misc)

  (defcustom xterm-program "xterm"
    "The program launched by the `xterm' function. Interpreted by shell."
    :type string
    :group (misc apps))

  (defcustom browser-program "www-browser"
    "The program launched by the `browser' function. Interpreted by shell."
    :type string
    :group (misc apps))

  (define (xterm #!optional command)
    "Start a new terminal. Optional argument `COMMAND' is passed to the
terminal with -e option, so for most, including xterm, it can contain
arguments to be passed."
    (if (not command)
	(system (format nil "%s >/dev/null 2>&1 </dev/null &"
			xterm-program))
      ;; Note that -e has to be the last argument. See man xterm.
      (system (format nil "%s -e %s >/dev/null 2>&1 </dev/null &"
		      xterm-program command))))

  (define (browser #!optional url)
    "Start a new browser instance."
    (if (not url)
        (system (format nil "%s >/dev/null 2>&1 </dev/null &"
			browser-program))
      (system (format nil "%s %s >/dev/null 2>&1 </dev/null &"
                      browser-program url))))

  ;;###autoload
  (define-command 'xterm xterm #:class 'default
    #:spec "sCommand:"
    #:type `(and (labelled ,(_ "Command:") string))
    #:doc "Start xterm. Optional command is passed with -e."
    )
  (define-command 'browser browser #:class 'default
    #:spec "sUrl:"
    #:type `(and (labelled ,(_ "url:") string))
    #:doc "Start browser. Url is optional."
    )
  )
