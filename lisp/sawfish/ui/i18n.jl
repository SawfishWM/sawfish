#| nokogiri-i18n -- support for internationalization

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

(define-structure sawfish.ui.i18n

    (export i18n-lang
	    i18n-filename
	    i18n-init)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.files
	  rep.i18n.gettext
	  sawfish.ui.wm)

  (define i18n-lang (or (getenv "LC_ALL")
			(getenv "LC_MESSAGES")
			(getenv "LANG")))

  (define i18n-lang-base (and i18n-lang
			      (if (string-match "^([^_.]+)_.*" i18n-lang)
				  (expand-last-match "\\1")
				i18n-lang)))

  ;; for a file called FILE, look for one with a .LC extension LC is
  ;; the language code for the current language
  (define (i18n-filename file)
    (cond ((and i18n-lang
		(file-exists-p (concat file #\. i18n-lang)))
	   (concat file #\. i18n-lang))
	  ((and i18n-lang-base
		(file-exists-p (concat file #\. i18n-lang-base)))
	   (concat file #\. i18n-lang-base))
	  (t file)))

  (define (i18n-init)
    (unless (get-command-line-option "--disable-nls")
      (let ((locale-dir (wm-locale-dir)))
	(when locale-dir
	  (bindtextdomain "sawfish" locale-dir)
	  (when (boundp 'bindtextdomaincodeset)
	    (bindtextdomaincodeset "sawfish" "UTF-8"))
	  (textdomain "sawfish"))))))
