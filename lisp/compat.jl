;; compat.jl -- aliases for obsolete functions
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

(provide 'compat)

(define ws-workspace-limits workspace-limits)
(define ws-workspace-empty-p workspace-empty-p)

(defun show-message (&optional text font fg bg position)
  (let
      ((attrs nil))
    (when font
      (setq attrs (cons (cons 'font font) attrs)))
    (when fg
      (setq attrs (cons (cons 'fg fg) attrs)))
    (when bg
      (setq attrs (cons (cons 'bg bg) attrs)))
    (when position
      (setq attrs (cons (cons 'position position) attrs)))
    (display-message text attrs)))

(define sawmill-directory sawfish-directory)
(define sawmill-lisp-lib-directory sawfish-lisp-lib-directory)
(define sawmill-site-lisp-directory sawfish-site-lisp-directory)
(define sawmill-exec-directory sawfish-exec-directory)
(define sawmill-version sawfish-version)
