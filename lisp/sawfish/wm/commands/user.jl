#| user.jl -- command to launch an xterm/brower and more

   $Id: user.jl,v 1.7 2002/04/21 22:25:12 jsh Exp $

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.commands.user

    (export xterm
	    browser
	    some
	    display-message-with-timeout
	    program-available
	    view-clipboard
	    display-workspace-name)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.timers
	  rep.io.files
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.workspace
	  sawfish.wm.util.selection)

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

  ;; some
  ;; XXX description
  (define (some pred lst)
    (cond ((null lst) nil)
      ((pred (car lst)) t)
      (t (some pred (cdr lst)))))

  ;; display-message-with-timeout
  ;; like display-message, but it dissapears
  ;; after `timemout' seconds
  (define (display-message-with-timeout message timeout)
    (display-message message)
    (make-timer (lambda () (display-message nil)) timeout))

  ;; program-available
  ;; return true, if program `cmd' is in path
  (define (program-available cmd)
    (some (lambda (dir)
      (file-exists-p (concat dir "/" cmd)))
      (string-split ":" (getenv "PATH"))))

  (defcustom clipboard-preview-clip-length 60
    "Maximum length of Clipboard Preview"
    :type number
    :group misc)

  (defcustom clipboard-preview-timeout 5
    "How long to display Clipboard Preview"
    :type number
    :group misc)

  ;; view-clipboard
  ;; view the content of the primary x-clipboard
  (define (view-clipboard)
    "Show the contents of the clipboard in a message window"
    (let ((c (x-get-selection 'PRIMARY)))
      (if (> (length c) 0)
      (if (< (length c) clipboard-preview-clip-length)
        (display-message-with-timeout c clipboard-preview-timeout)
        (display-message-with-timeout (format nil "%s ..."
          (substring c 0 clipboard-preview-clip-length)) clipboard-preview-timeout)))))

  (defcustom display-ws-name-on-switch nil
    "Wether to display workspace name upon switch"
    :type boolean
    :group workspace
    :after-set (lambda () (display-ws-name-setter)))

  (defcustom display-ws-name-timeout 3
    "How long to display workspace name"
    :type number
    :group workspace)

  ;; display-workspace-name
  ;; display WS name on switch
  (define (display-workspace-name)
    (display-message-with-timeout
       (format nil "Now on Workspace: %s"
         (or (nth current-workspace workspace-names)
           (format nil (_ "Workspace %d") (1+ current-workspace))))
             display-ws-name-timeout))

  (define (display-ws-name-setter)
    (if (eq display-ws-name-on-switch 't)
      (add-hook 'enter-workspace-hook display-workspace-name)
      (remove-hook 'enter-workspace-hook display-workspace-name)))

  ;;###autoload
  (define-command 'xterm xterm #:class 'default)
  (define-command 'browser browser #:class 'default)
  (define-command 'view-clipboard view-clipboard #:class 'default)
  (define-command 'display-workspace-name display-workspace-name #:class 'default))
