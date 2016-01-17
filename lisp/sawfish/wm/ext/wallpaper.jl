;; wallpaper.jl - integrate wallpaper settings into Sawfish

;; (c) 2011 Lucas Pandolfo

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.ext.wallpaper

  (export set-wallpaper
	  set-wallpaper-gnome
	  set-wallpaper-xfce)

  (open rep
	rep.system
	rep.io.files
	rep.util.misc
	sawfish.wm
	sawfish.wm.custom)

  ;; XXX KDE4 doesn't support shell-based changing
  ;; XXX of the wallpaper ... KDE3 did

  ;; XXX Need GNOME3 support

  (define-structure-alias wallpaper sawfish.wm.ext.wallpaper)

  (defgroup wallpaper "Wallpaper" :group appearance)

  (defcustom init-wallpaper nil
    "Whether to change wallpaper on startup."
    :type boolean
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper)))

  (defcustom root-wallpaper nil
    "Wallpaper to use: \\w"
    :type image
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper))
    :depends init-wallpaper)

  (defcustom wallpaper-setter ""
    "Application to set desktop wallpaper."
    :type file-name
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper))
    :depends init-wallpaper)

  (defcustom wallpaper-setter-args ""
    "Additional arguments to pass to the application."
    :type string
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper))
    :depends init-wallpaper)

  (defcustom set-wallpaper-gnome2 nil
    "Whether to apply wallpaper to GNOME2"
    :type boolean
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper))
    :depends init-wallpaper)

  (defcustom set-wallpaper-xfce4 nil
    "Whether to apply wallpaper to XFCE4"
    :type boolean
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper))
    :depends init-wallpaper)

  (defcustom wallpaper-display-type 'centered
    "How to display the image (only for GNOME2 and XFCE4)."
    :type (choice auto/none centered tiled scaled streched zoom spanned)
    :group (appearance wallpaper)
    :depends init-wallpaper
    :after-set (lambda () (set-wallpaper)))

  (define (set-wallpaper)
    (when (and init-wallpaper
	       (not (equal root-wallpaper ())))
    (if (file-exists-p root-wallpaper)
        (progn
	  (when set-wallpaper-xfce4
            (set-wallpaper-xfce))
	  (when set-wallpaper-gnome2
            (set-wallpaper-gnome))
          (when (and wallpaper-setter
	        (not (eq desktop-environment "gnome"))
		(not (eq desktop-environment "xfce")))
            (setq wallpaper-filename (concat " \"" root-wallpaper "\""))
	    (when (or (program-exists-p wallpaper-setter)
	              (file-exists-p wallpaper-setter))
	      (system (concat wallpaper-setter " " wallpaper-setter-args " " wallpaper-filename " &")))))
      (display-message (format nil "Wallpaper %s not found." root-wallpaper)))))

  (defvar gnome-type nil)
  (defvar xfce-type nil)
  (defvar wallpaper-filename nil)

  (define (set-wallpaper-gnome)
    (when (eq desktop-environment "gnome")
      (setq wallpaper-filename (concat " \"" root-wallpaper "\""))
	(case wallpaper-display-type
	  ((auto/none) (setq gnome-type "none"))
	  ;; unknown value to GNOME2
	  ((tiled) (setq gnome-type "none"))
	  (t (setq gnome-type wallpaper-display-type)))
	(system (concat "gconftool-2 --set --type string
			/desktop/gnome/background/picture_filename " wallpaper-filename " &"))
	(system (concat "gconftool-2 --set --type string
			/desktop/gnome/background/picture_options " gnome-type " &"))))

  (define (set-wallpaper-xfce)
    (when (eq desktop-environment "xfce")
      (setq wallpaper-filename (concat " \"" root-wallpaper "\""))
      (case wallpaper-display-type
	((auto/none) (setq xfce-type 0))
	((centered) (setq xfce-type 1))
	((tiled) (setq xfce-type 2))
	((scaled) (setq xfce-type 4))
	((stretched) (setq xfce-type 3))
	((zoom) (setq xfce-type 5))
	;; unknown value to XFCE4
	((spanned) (setq xfce-type 0)))
      (system (concat "xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/image-path
		      -s " wallpaper-filename " &"))
      (system (concat "xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/image-style
		      -s " xfce-type " &")))))
