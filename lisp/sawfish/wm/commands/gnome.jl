;; gnome-commands.jl -- more GNOME stuff
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.commands.gnome

    (export gnome-toggle-skip-winlist
	    gnome-set-skip-winlist
	    gnome-clear-skip-winlist
	    gnome-toggle-skip-tasklist
	    gnome-set-skip-tasklist
	    gnome-clear-skip-tasklist
	    gnome-logout
	    gnome-www-page
	    gnome-help-browser)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.state.gnome
	  sawfish.wm.state.ignored
	  sawfish.wm.commands)

  (define-structure-alias gnome-commands sawfish.wm.commands.gnome)


;;; commands

  (define (gnome-toggle-skip-winlist w)
    "Toggle the GNOME SKIP_WINLIST hint of the window."
    (toggle-window-list-skip w))

  (define (gnome-set-skip-winlist w)
    "Set the GNOME SKIP_WINLIST hint of the window."
    (unless (window-get w 'window-list-skip)
      (toggle-window-list-skip w)))

  (define (gnome-clear-skip-winlist w)
    "Unset the GNOME SKIP_WINLIST hint of the window."
    (when (window-get w 'window-list-skip)
      (toggle-window-list-skip w)))

  ;;###autoload
  (define-command 'gnome-toggle-skip-winlist gnome-toggle-skip-winlist
    #:spec "%W")
  (define-command 'gnome-set-skip-winlist gnome-set-skip-winlist
    #:spec "%W")
  (define-command 'gnome-clear-skip-winlist gnome-clear-skip-winlist
    #:spec "%W")

  (define (gnome-toggle-skip-tasklist w)
    "Toggle the GNOME SKIP_TASKLIST hint of the window."
    (gnome-toggle-hint w WIN_HINTS_SKIP_TASKLIST))

  (define (gnome-set-skip-tasklist w)
    "Set the GNOME SKIP_TASKLIST hint of the window."
    (gnome-set-hint w WIN_HINTS_SKIP_TASKLIST))

  (define (gnome-clear-skip-tasklist w)
    "Unset the GNOME SKIP_TASKLIST hint of the window."
    (gnome-clear-hint w WIN_HINTS_SKIP_TASKLIST))

  ;;###autoload
  (define-command 'gnome-toggle-skip-tasklist gnome-toggle-skip-tasklist
    #:spec "%W")
  (define-command 'gnome-set-skip-tasklist gnome-set-skip-tasklist
    #:spec "%W")
  (define-command 'gnome-clear-skip-tasklist gnome-clear-skip-tasklist
    #:spec "%W")


;; extras

  (define (gnome-logout)
    "Logout from the current GNOME session."
    (system "save-session --kill &"))

  (define (gnome-www-page)
    "Display the WWW page of the GNOME project."
    (require 'sawfish.wm.commands.help)
    (display-url "http://www.gnome.org/"))

  (define (gnome-help-browser)
    "Launch the GNOME help browser."
    (system "gnome-help >/dev/null 2>&1 </dev/null &"))

  (define (gnome-about)
    "Launch the GNOME about dialog."
    (system "gnome-about >/dev/null 2>&1 </dev/null &"))

  ;;###autoload
  (define-command 'gnome-logout gnome-logout)
  (define-command 'gnome-www-page gnome-www-page)
  (define-command 'gnome-help-browser gnome-help-browser)
  (define-command 'gnome-about gnome-about))
