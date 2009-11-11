;; play-audio.jl -- functions for playing sound samples

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

(define-structure sawfish.wm.util.play-audio

    (export play-sample)

    (open rep
	  rep.system
	  rep.io.files
	  rep.io.processes
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

  (defcustom play-sample-program "/usr/bin/paplay"
    "The program used to play audio samples. Should be capable of
playing *.wav file.
  You can't set any arguments to pass from this variable. If you want
to give arguments or redirect output, write a wrapper program."
    :type program
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
	;; start programs asynchronously in case they block..
	(let ((sentinel (lambda (proc)
	    (when (eq play-sample-process proc)
	      (setq play-sample-process nil))
	    (when delete-it
	      (delete-file real-name)))))
	  (when play-sample-process
	    (kill-process play-sample-process))
	  (setq play-sample-process (make-process standard-error sentinel))
	  (start-process play-sample-process play-sample-program real-name)))))
