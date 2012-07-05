;; gaol.jl -- protected environment for themes
;;
;; Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, 
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.gaol

    (export make-gaol
	    gaol-load
	    gaol-eval
	    gaol-add
	    gaol-define
	    gaol-define-special
	    define-gaol-structure)

    (open rep
	  rep.io.files
	  sawfish.wm.colors
	  sawfish.wm.cursors
	  sawfish.wm.events
	  sawfish.wm.fonts
	  sawfish.wm.images
	  sawfish.wm.misc
	  rep.util.gaol)

  ;; safe functions in the core module
  (define safe-functions
    '(get-color-rgb get-color color-name color-rgb colorp
      get-cursor cursorp get-font get-font-typed
      font-type-exists-p font-name fontp font-type text-width
      font-height font-ascent font-descent screen-width
      screen-height get-x-property get-x-text-property
      list-x-properties x-atom x-atom-name make-image
      make-image-from-x-drawable copy-image
      flip-image-horizontally flip-image-vertically
      flip-image-diagonally image-get image-put imagep
      image-dimensions image-border set-image-border
      image-shape-color set-image-shape-color image-modifier
      set-image-modifier make-sized-image bevel-image clear-image
      tile-image scale-image composite-images crop-image
      make-keymap bind-keys unbind-keys keymapp eventp image-ref
      image-set image-map image-fill color-rgb-8 uniquify-list))

  (define safe-specials
    '(default-foreground display-name canonical-display-name
      default-font default-frame nil-frame frame-part-classes
      decorate-transients batch-mode default-directory frame-font
      ;; This should be in librep, but I added it here, too.
      ;; Without this, wrong theme codes make Sawfish crash.
      ;; It's ok for librep to fix it, too.
      %in-condition-case))

  (define safe-features '(sawfish.wm.util.gtkrc
			  sawfish.wm.util.x
			  ;; compatibility
			  gtkrc x))
  (define fully-safe-features '(rep.io.timers rep.data.tables
				rep.data.records rep.data.ring
				rep.data.queues rep.data.symbol-table
				sawfish.wm.util.gradient
				sawfish.wm.util.recolor-image
				sawfish.wm.theming.make-theme
				sawfish.wm.tabs.tab
				;; compatibility
				timers gradient make-theme))

;;; functions

  (defmacro gaol-add vars
    `(mapc (lambda (x) (gaol-define x (symbol-value x))) ',vars))

  (defun gaol:require (feature)
    (cond ((memq feature fully-safe-features) (gaol-open feature))
	  ((memq feature safe-features) (load-module feature))
	  (t (error "Gaolled code trying to require %s" feature))))

;;; initialize the gaol envrironment

  ;; for backwards compatibility, / is integer division in themes, use
  ;; `divide' for real division
  (gaol-define 'divide /)
  (gaol-define '/ quotient)

  ;; use safe version of require
  (gaol-define 'require gaol:require)

  ;; add standard bindings
  (mapc (lambda (x)
	  (if (boundp x)
	      (gaol-define x (symbol-value x))
	    (format standard-error "warning: %s unbound\n" x))) safe-functions)
  (mapc gaol-define-special safe-specials)

  ;; a plugin, so easier to do this here..
  (call-after-load "sawfish.wm.util.x"
                   (lambda ()
                     (require 'sawfish.wm.util.x)
                     (mapc (lambda (x) (gaol-define x (symbol-value x)))
                           '(x-create-gc x-change-gc x-destroy-gc x-gc-p
                             x-create-pixmap x-create-bitmap
                             x-change-window-attributes x-window-p
                             x-destroy-drawable x-drawable-p
                             x-pixmap-p x-bitmap-p x-drawable-id
                             x-drawable-width x-drawable-height
                             x-window-id x-window-back-buffer
                             x-window-swap-buffers x-clear-window
                             x-draw-string x-draw-line
                             x-draw-rectangle x-fill-rectangle
                             x-draw-arc x-fill-arc x-fill-polygon
                             x-copy-area x-draw-image
                             x-grab-image-from-drawable
                             x-gc-set-dashes)))))
