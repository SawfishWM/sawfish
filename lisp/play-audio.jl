;; play-audio.jl -- functions for playing sound samples
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

(provide 'play-audio)

(defvar audio-load-path (list "~/.sawmill/sounds"
			      (or (getenv "SAWMILLSOUNDSDIR")
				  (expand-file-name
				   "../sounds" sawmill-lisp-lib-directory))
			      (or (getenv "SAWMILLSITESOUNDSDIR")
				  (expand-file-name
				   "../../sounds" sawmill-lisp-lib-directory))
			      ".")
  "List of directories to search for sound samples.")

;;;###autoload
(defun play-sample (filename)
  "Play the audio sample stored in file FILENAME."
  (unless (file-exists-p filename)
    (setq filename (or (locate-file filename audio-load-path)
		       (error "No such sound sample: %s" filename))))
  (let
      ((real-name (local-file-name filename))
       (delete-it nil))
    (unless real-name
      (setq real-name (make-temp-name))
      (copy-file filename real-name)
      (setq delete-it t))
    (require 'play-sample)
    (primitive-play-sample real-name)
    (when delete-it
      (delete-file real-name))))
