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

(define-structure sawfish.wm.util.play-audio

    (export play-sample)

    (open rep
	  sawfish.wm.custom
	  sawfish.wm.misc)

  (defvar audio-load-path
    (list "~/.sawfish/sounds"
	  (or (getenv "SAWFISHSOUNDSDIR")
	      (expand-file-name "../sounds" sawfish-lisp-lib-directory))
	  (or (getenv "SAWFISHSITESOUNDSDIR")
	      (expand-file-name "../../sounds" sawfish-lisp-lib-directory))
	  ".")
    "List of directories to search for sound samples.")

  (defcustom play-sample-program nil
    "The program used to play audio samples. If unset, built-in support for \
ESD is used."
    :type (optional program)
    :user-level expert
    :group audio)

  ;; currently running audio process
  (define play-sample-process nil)

  (define (play-sample filename)
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
      (if play-sample-program
	  ;; start programs asynchronously in case they block..
	  (let ((sentinel (lambda (proc)
			    (when (eq play-sample-process proc)
			      (setq play-sample-process nil))
			    (when delete-it
			      (delete-file real-name)))))
	    (when play-sample-process
	      (kill-process play-sample-process))
	    (setq play-sample-process (make-process standard-error sentinel))
	    (start-process play-sample-process play-sample-program real-name))
	(require 'sawfish.wm.util.play-sample)
	(primitive-play-sample real-name)
	(when delete-it
	  (delete-file real-name))))))
