;; window-anim.jl -- visual feedback for window actions
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

(provide 'window-anim)

(defvar window-animators nil
  "List of all possible window animation types.")

(defcustom default-window-animator 'none
  "The default window animation mode: \\w"
  :type symbol
  :group appearance)

(defun define-window-animator (name fun)
  "Define a window animator called NAME (a symbol) that is managed by function
FUN. FUN is called as (FUN WINDOW OP [ACTION]) when it should change the state
of an animation sequence. OP may be one of the symbols `start', `stop'."
  (put name 'window-animator fun)
  (setq window-animators (cons name (delq name window-animators)))
  (custom-set-property 'default-window-animator ':options window-animators))

(defun run-window-animator (w action)
  "Invoke an animation for ACTION on window W."
  (let
      ((running (window-get w 'running-animator)))
    (when running
      (running w 'stop)))
  (let
      ((animator (or (window-get w 'animator) default-window-animator)))
    (when animator
      ((get animator 'window-animator) w 'start action))))

(defun record-window-animator (w animator)
  "Note that window W currently has an animation running; being controlled
by animator function ANIMATOR."
  (window-put w 'running-animator animator))

;; for the hardcore
(define-window-animator 'none nop)

;; in the window-state-change-hook
(defun window-anim-initiator (w states)
  ;; XXX select a state if >1? (never currently happens?)
  (run-window-animator w (car states)))

(add-hook 'window-state-change-hook window-anim-initiator)
