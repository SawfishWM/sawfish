#| nokogiri-config.jl -- options that affect the configurator, not the wm

   $Id$

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

(require 'nokogiri-interfaces)

(define-structure nokogiri-config nokogiri-config-interface

    (open rep
	  tables
	  nokogiri-apply
	  nokogiri-slot
	  nokogiri-group)

  (define done-init nil)

  ;; list of (NAME . THUNK) -- configs to initialize at some point
  (define pending-configs '())

  (define (define-config-item name var thunk)
    (let ((callback (lambda ()
		      (set var (custom-symbol-value name))
		      (thunk))))
      (if done-init
	  (let ((slot (or (get-slot name)
			  (error "Unknown slot: %s" name))))
	    (define-change-handler slot callback)
	    (callback))
	(setq pending-configs (cons (cons name callback) pending-configs)))))

  (define (initialize-configs)
    (unless done-init
      ;; ensure that config group has been loaded
      (fetch-group (get-group root-group))
      (mapc (lambda (x)
	      (let ((slot (or (get-slot (car x))
			      (error "Unknown slot: %s" (car x)))))
		(define-change-handler slot (cdr x))
		((cdr x))))
	    pending-configs)
      (setq pending-configs '())
      (setq done-init t))))
