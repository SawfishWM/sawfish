;; help.jl -- commands for the help menu
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.commands.help

    (export display-url
	    help-call-info
	    help-call-info-gnome)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.files
	  sawfish.wm.commands)

  ;; Info

  (define (help-call-info document node)
    (system (format nil "xterm -e info '%s' '%s' >/dev/null 2>&1 </dev/null &"
		    document node)))

  (define (help-call-info-gnome document node)
    (while (string-match " " node)
      (setq node (concat (substring node 0 (match-start)) ?_
			 (substring node (match-end)))))
    (system (format nil "gnome-help 'info:%s#%s' >/dev/null 2>&1 </dev/null &"
		    document node)))

  (defvar help-display-info-function help-call-info)

  ;; WWW

  (defvar display-url-command
    "( netscape -remote 'openUrl(%s)' || netscape '%s' ) &"
    "Shell command used to direct a web browser to load a url. Any `%s'
substrings will be replaced by the name of the url.")

  (define (display-url url)
    (let ((args (list url)))
      (rplacd args args)
      (system (apply format nil display-url-command args))))

  ;; Commands

  (define (show-faq) (help-display-info-function "sawfish" "FAQ"))

  (define (show-news) (help-display-info-function "sawfish" "News"))

  (define (show-programmer-manual)
    (help-display-info-function "sawfish" "Top"))

  (define (show-homepage) (display-url "http://sawmill.sourceforge.net/"))

  (define (show-about)
    (system (format nil "%s >/dev/null 2>&1 </dev/null &"
		    (expand-file-name "sawfish-about"
				      sawfish-exec-directory))))

  ;;###autoload
  (define-command 'help:show-faq show-faq)
  (define-command 'help:show-news show-news)
  (define-command 'help:show-programmer-manual show-programmer-manual)
  (define-command 'help:show-homepage show-homepage)
  (define-command 'help:about show-about))
