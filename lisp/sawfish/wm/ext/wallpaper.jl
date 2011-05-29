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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.ext.wallpaper

  (export set-wallpaper
	  set-wallpaper-gnome
	  set-wallpaper-xfce)

  (open rep
	rep.system
	rep.io.files
	sawfish.wm
	sawfish.wm.custom)

  ;; XXX KDE4 doesn't support shell-based changing
  ;; XXX of the wallpaper ... KDE3 did

  ;; XXX Need GNOME3 support

  (define-structure-alias wallpaper sawfish.wm.ext.wallpaper)

  (defgroup wallpaper "Wallpaper" :group appearance)

  (defcustom root-wallpaper nil
    "Root wallpaper to use."
    :type image
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper)))

  (defcustom wallpaper-setter ""
    "Application to set desktop wallpaper."
    :type file-name
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper)))

  (defcustom wallpaper-setter-args ""
    "Additional arguements to pass to the application."
    :type string
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper)))

  (defcustom set-wallpaper-gnome2 nil
    "Whether to apply wallpaper to GNOME2"
    :type boolean
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper-gnome)))

  (defcustom set-wallpaper-xfce4 nil
    "Whether to apply wallpaper to XFCE4"
    :type boolean
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper-xfce)))

  (defcustom wallpaper-display-type 'centered
    "How to display the image (only for GNOME2 and XFCE4)."
    :type (choice auto/none centered tiled scaled streched zoom spanned)
    :group (appearance wallpaper)
    :after-set (lambda () (set-wallpaper)))

  (define (set-wallpaper)
    (when (file-exists-p root-wallpaper)
      (setq wallpaper-filename (concat " \"" root-wallpaper "\""))
      (when wallpaper-setter
	(system (concat wallpaper-setter " " wallpaper-setter-args wallpaper-filename " &")))))

  (defvar gnome-type nil)
  (defvar xfce-type nil)
  (defvar wallpaper-filename nil)

  (define (set-wallpaper-gnome)
    (when (and (eq desktop-environment "gnome")
	       set-wallpaper-gnome2
	       (file-exists-p root-wallpaper))
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
    (when (and (eq desktop-environment "xfce")
	       set-wallpaper-xfce4
	       (file-exists-p root-wallpaper))
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

  

