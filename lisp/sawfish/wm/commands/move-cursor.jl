;; move-cursor.jl -- commands to move the mouse pointer
;; $Id: move-cursor.jl,v 1.4 2000/07/27 13:19:29 john Exp $

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.commands.move-cursor

    (export move-cursor
            move-cursor-left-fine
            move-cursor-right-fine
            move-cursor-up-fine
            move-cursor-down-fine
            move-cursor-left
            move-cursor-right
            move-cursor-up
            move-cursor-down
	    move-cursor-northwest
	    move-cursor-northeast
	    move-cursor-southwest
	    move-cursor-southeast
            move-cursor-northwest-fine
	    move-cursor-northeast-fine
	    move-cursor-southwest-fine
	    move-cursor-southeast-fine
            move-cursor-center)

    (open rep
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.custom
	  sawfish.wm.commands
          sawfish.wm.gaol)

  (defcustom move-cursor-increment 16
    "Number of pixels to move pointer in."
    :group misc
    :type (number 1))

  (define (move-cursor right down)
     (let ((coords (query-pointer)))
         (warp-cursor (+ (car coords) right) (+ (cdr coords) down))))

  (define (move-cursor-left)
    "Move the cursor `move-cursor-increment' pixels to the left."
    (move-cursor (- move-cursor-increment) 0))

  (define (move-cursor-right)
    "Move the cursor `move-cursor-increment' pixels to the right."
    (move-cursor move-cursor-increment 0))

  (define (move-cursor-up)
    "Move the cursor `move-cursor-increment' pixels upwards."
    (move-cursor 0 (- move-cursor-increment)))

  (define (move-cursor-down)
    "Move the cursor `move-cursor-increment' pixels downwards."
    (move-cursor 0 move-cursor-increment))

  (define (move-cursor-northwest)
    "Move the cursor `move-cursor-increment' pixels northwest"
    (move-cursor (- move-cursor-increment) (- move-cursor-increment)))

  (define (move-cursor-northeast)
    "Move the cursor `move-cursor-increment' pixels northeast"
    (move-cursor move-cursor-increment (- move-cursor-increment)))

  (define (move-cursor-southwest)
    "Move the cursor `move-cursor-increment' pixels southwest"
    (move-cursor (- move-cursor-increment) move-cursor-increment))

  (define (move-cursor-southeast)
    "Move the cursor `move-cursor-increment' pixels southeast"
    (move-cursor move-cursor-increment move-cursor-increment))

  (define (move-cursor-left-fine)
    "Move the cursor 1 pixel to the left."
    (move-cursor -1 0))

  (define (move-cursor-right-fine)
    "Move the cursor 1 pixel to the right."
    (move-cursor 1 0))

  (define (move-cursor-up-fine)
    "Move the cursor 1 pixel upwards."
    (move-cursor 0 -1))

  (define (move-cursor-down-fine)
    "Move the cursor 1 pixel downwards."
    (move-cursor 0 1))

  (define (move-cursor-northwest-fine)
    "Move the cursor `move-cursor-increment' pixels northwest"
    (move-cursor -1 -1))

  (define (move-cursor-northeast-fine)
    "Move the cursor `move-cursor-increment' pixels northeast"
    (move-cursor 1 -1))

  (define (move-cursor-southwest-fine)
    "Move the cursor `move-cursor-increment' pixels southwest"
    (move-cursor -1 1))

  (define (move-cursor-southeast-fine)
    "Move the cursor `move-cursor-increment' pixels southeast"
    (move-cursor 1 1))

  (define (move-cursor-center)
    "Move the cursor to the center of the screen"
    (warp-cursor (/ (screen-width) 2) (/ (screen-height) 2)))

  ;;###autoload
  (define-command 'move-cursor-right move-cursor-right)
  (define-command 'move-cursor-left move-cursor-left)
  (define-command 'move-cursor-up move-cursor-up)
  (define-command 'move-cursor-down move-cursor-down)
  (define-command 'move-cursor-right-fine move-cursor-right-fine)
  (define-command 'move-cursor-left-fine move-cursor-left-fine)
  (define-command 'move-cursor-up-fine move-cursor-up-fine)
  (define-command 'move-cursor-down-fine move-cursor-down-fine)
  (define-command 'move-cursor-northwest move-cursor-northwest)
  (define-command 'move-cursor-northeast move-cursor-northeast)
  (define-command 'move-cursor-southwest move-cursor-southwest)
  (define-command 'move-cursor-southeast move-cursor-southeast)
  (define-command 'move-cursor-down-fine move-cursor-down-fine)
  (define-command 'move-cursor-northwest-fine move-cursor-northwest-fine)
  (define-command 'move-cursor-northeast-fine move-cursor-northeast-fine)
  (define-command 'move-cursor-southwest-fine move-cursor-southwest-fine)
  (define-command 'move-cursor-southeast-fine move-cursor-southeast-fine)
  (define-command 'move-cursor-center move-cursor-center))
