;; sm-common.jl -- code used by both sm-save and sm-load
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

(require 'sm-init)
(provide 'sm-common)


;; utilities

;; find PROP associated with W, or nil
(defun sm-get-window-prop (w prop)
  ;; first look in the window itself,
  (unless (nth 2 (get-x-property w prop))
    ;; else try the leader
    (let*
	(tem
	 (leader (cond ((and (setq tem (get-x-property w 'WM_CLIENT_LEADER))
			     (eq (car tem) 'WINDOW)
			     (eq (nth 1 tem) 32))
			(aref (nth 2 tem) 0))
		       ((window-group-id w))
		       ((window-transient-p w)))))
      (and leader (nth 2 (get-x-property leader prop))))))
