;; functions.jl -- miscellaneous stuff
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

(provide 'functions)

;; return a window called NAME, or nil
(defun get-window-by-name (name)
  (catch 'foo
    (mapc #'(lambda (w)
	      (when (string= (window-name w) name)
		(throw 'foo w))) (managed-windows))
    nil))

;; return the window with id ID, or nil
(defun get-window-by-id (id)
  (catch 'foo
    (mapc #'(lambda (w)
	      (when (= (window-id w) id)
		(throw 'foo w))) (managed-windows))))

;; execute FORMS with the server grabbed
(defmacro with-server-grabbed (&rest forms)
  `(progn
     (grab-server)
     (unwind-protect
	 (progn ,@forms)
       (ungrab-server))))

;; execute FORMS, then reinstall the original stacking order
(defmacro save-stacking-order (&rest forms)
  (let
      ((tem (gensym)))
    `(let
	 ((,tem (stacking-order)))
       (unwind-protect
	   (progn ,@forms)
	 (restack-windows ,tem)))))
