#| nokogiri-widgets/frame-style.jl -- theme chooser widget

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

(define-structure sawfish.ui.widgets.frame-style ()

    (open rep
	  gui.gtk-2.gtk
	  rep.regexp
	  rep.io.files
	  sawfish.gtk.widget
	  sawfish.ui.i18n)

  (define (make-frame-style-item changed-callback doc options path)

    (let ((vbox (gtk-vbox-new nil 0))
	  (hbox (gtk-hbox-new nil 0))
	  (combo (gtk-combo-new))
	  (doc-label (gtk-label-new doc))
	  (readme-text-view (gtk-text-view-new))
	  (readme-scroller (gtk-scrolled-window-new))
	  (value (car options)))

      (gtk-box-set-spacing hbox box-spacing)
      (gtk-box-set-spacing vbox box-spacing)
      (gtk-container-add readme-scroller readme-text-view)
      (gtk-box-pack-start hbox doc-label)
      (gtk-box-pack-start hbox combo t t)
      (gtk-box-pack-start vbox readme-scroller t t)
      (gtk-box-pack-start vbox hbox nil nil)
      (gtk-label-set-justify doc-label 'left)
      ;;(gtk-text-view-set-wrap-mode readme-text-view 'word)
      (gtk-text-view-set-editable readme-text-view nil)
      (gtk-editable-set-editable (gtk-combo-entry combo) nil)
      (gtk-scrolled-window-set-policy readme-scroller 'automatic 'automatic)

      (gtk-combo-set-popdown-strings combo (mapcar symbol-name options))
      (when value
	(gtk-entry-set-text (gtk-combo-entry combo) (symbol-name value)))

      (gtk-signal-connect (gtk-combo-entry combo) "changed"
			  (lambda ()
			    (setq value (intern (gtk-entry-get-text
						 (gtk-combo-entry combo))))
			    (update-readme value readme-text-view path)
			    (call-callback changed-callback)))

      (update-readme value readme-text-view path)
      (gtk-widget-show-all vbox)

      (lambda (op)
	(case op
	  ((gtk-widget) vbox)
	  ((clear) (lambda ()))
	  ((set) (lambda (x)
		   (gtk-entry-set-text
		    (gtk-combo-entry combo) (symbol-name x))))
	  ((ref) (lambda () value))
	  ((validp) (lambda (x) (memq x options)))))))

  (define-widget-type 'frame-style make-frame-style-item)
  (widget-accepts-doc-string 'frame-style)

  (define (text-view-set view string)
    (let ((buffer (gtk-text-view-get-buffer view))
	  (iter (gtk-text-iter-new)))
      (gtk-text-buffer-set-text buffer string (length string))
      (gtk-text-buffer-get-start-iter buffer iter)
      (gtk-text-buffer-place-cursor buffer iter)))

  (define (update-readme value text-view theme-path)
    (catch 'out
      (let ((theme (symbol-name value)))
	(mapc (lambda (dir)
		(let ((full (expand-file-name theme dir)))
		  (when (catch 'out
			  (mapc (lambda (suf)
				  (let ((dir (format nil suf full theme)))
				    (condition-case nil
					(when (file-directory-p dir)
					  (setq full dir)
					  (throw 'out t))
				      (error))))
				'("%s" "%s.tar#tar/%s" "%s.tar.gz#tar/%s"
				  "%s.tar.Z#tar/%s" "%s.tar.bz2#tar/%s"))
			  nil)
		    (setq full (i18n-filename
				(expand-file-name "README" full)))
		    (if (file-exists-p full)
			(let ((text (make-string-output-stream))
			      (file (open-file full 'read)))
			  (unwind-protect
			      (progn
				(copy-stream file text)
				(setq text (get-output-stream-string text))
				(when (string-match "\\s+$" text)
				  (setq text (substring text 0 (match-start))))
				(text-view-set text-view text))
			    (close-file file)))
		      (text-view-set text-view ""))
		    (throw 'out t))))
	      theme-path)
	(text-view-set text-view "")))))
