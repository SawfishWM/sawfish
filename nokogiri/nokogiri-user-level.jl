#| nokogiri-user-level.jl -- user-level stuff

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

(define-structure nokogiri-user-level

    (export slot-is-appropriate-p)

    (open rep
	  nokogiri-config
	  nokogiri-slot
	  nokogiri-group)

  (defvar *nokogiri-user-level* nil)	;novice,intermediate,expert

  (put 'novice 'nokogiri-user-level 0)
  (put 'intermediate 'nokogiri-user-level 1)
  (put 'expert 'nokogiri-user-level 2)

  ;; return t if SLOT should be shown
  (define (slot-is-appropriate-p slot)
    (>= (get *nokogiri-user-level* 'nokogiri-user-level)
	(get (slot-user-level slot) 'nokogiri-user-level)))

  (define (user-level-changed) (redisplay-group))

  (define-config-item 'nokogiri-user-level
		      '*nokogiri-user-level*
		      user-level-changed))
