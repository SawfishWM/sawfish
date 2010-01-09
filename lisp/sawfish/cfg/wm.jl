;; wm.jl -- interface to WM as a server
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

(define-structure sawfish.cfg.wm

    (export wm-load-slots
	    wm-load-group
	    wm-apply-changes
	    wm-locale-dir
	    wm-grab-x-property
	    wm-grab-key
	    wm-command-list
	    wm-documentation)

    (open rep
	  sawfish.client)

;;; wm communication (possibly replace by CORBA..?)

  (define (wm-eval form #!optional read-back)
    (sawfish-client-eval form (not read-back)))

  (define (wm-eval-async form)
    (sawfish-client-eval-async form))

  (define done-init nil)
  (define (init-server)
    (unless done-init
      (wm-eval '(require 'sawfish.wm.util.nokogiri))
      (setq done-init t)))

;;; stub functions

  ;; returns a list of keyworded items. Keywords include:
  ;; #:name, #:depends, #:value, #:type, #:doc
  (define (wm-load-slots names)
    (init-server)
    (wm-eval `(nokogiri-report-slots ',names) t))

  ;; return the data in GROUP
  (define (wm-load-group group)
    (init-server)
    (wm-eval `(nokogiri-report-group ',group) t))

  ;; CHANGES is list of (SLOT-NAME . NEW-VALUE)
  (define (wm-apply-changes changes)
    (init-server)
    (wm-eval `(nokogiri-apply-changes ',changes)))

  (define (wm-locale-dir)
    (wm-eval 'sawfish-locale-directory t))

  (define (wm-grab-x-property prop-name)
    (wm-eval `(nokogiri-grab-match-window-property ',prop-name) t))

  (define (wm-grab-key)
    (wm-eval '(nokogiri-grab-key) t))

  (define wm-command-list
    (let ((commands '*undefined*))
      (lambda ()
	(unless (listp commands)
	  (init-server)
	  (setq commands (wm-eval '(nokogiri-report-commands) t)))
	commands)))

  (define (wm-documentation symbol)
    (wm-eval
     `(let ((doc (command-documentation ',symbol)))
	(and doc (_ doc))) t)))
