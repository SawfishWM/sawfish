;; theme.jl for Arlo's new theme design
;;
;; Copyright (C) 2001 Eazel, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Authors: John Harper <jsh@eazel.com>

(require 'rep.data.tables)		;need hash tables for icon cache
(require 'sawfish.wm.util.recolor-image)

(defgroup Crux "Crux Theme"
  :group appearance)

(defcustom Crux:normal-color nil
  "Accent color for focused windows (if unset uses the GTK+ selection color)."
  :type (optional color)
  :group (appearance Crux)
  :after-set (lambda () (color-changed)))

(defcustom Crux:show-window-icons nil
  "Display the window's icon in its menu button."
  :type boolean
  :group (appearance Crux)
  :after-set (lambda () (rebuild-all)))

(defvar Crux:button-themes
  '((default
      ((close-button) . (iconify-button maximize-button shade-button)))
    (platinum
     ((close-button) . (maximize-button shade-button)))
    (macos-x
     ((close-button maximize-button iconify-button)))
    (windows
     ((menu-button) . (iconify-button maximize-button close-button))
     ((menu-button)))
    (next
     ((iconify-button) . (close-button))
     ((iconify-button) . (close-button)))
    (complete
     ((close-button) . (shade-button iconify-button maximize-button
                                     menu-button)))
    (complete-inverse
     ((menu-button maximize-button iconify-button shade-button) .
      (close-button)))))

(defcustom Crux:button-theme 'default
  "Display title buttons to mimic: \\w"
  ;; XXX it would be better if the choices were extracted from
  ;; XXX the above alist somehow
  :type (choice (default "Default")
		(platinum "Mac OS Platinum")
		(macos-x "Mac OS X")
		(windows "MS Windows")
		(next "NeXTSTEP")
		(complete "Complete")
		(complete-inverse "Complete (Inverse)"))
  :group (appearance Crux)
  :after-set (lambda () (reframe-all)))

;; maps WINDOW -> BUTTON-LIST
(define button-table (make-weak-table eq-hash eq))

;; images

;; 16x3
(define top-left-border
  (list (make-image "inactive:top-left-border.png")
	(make-image "active:top-left-border.png")))

;; 18x3
(define top-right-border
  (list (make-image "inactive:top-right-border.png")
	(make-image "active:top-right-border.png")))

;; 16*x3
(define top-center-left-border
  (list (set-image-border
	 (make-image "inactive:top-center-left-border.png") 0 13 0 0)
	(set-image-border
	 (make-image "active:top-center-left-border.png") 0 13 0 0)))

;; 16*x3
(define top-center-right-border
  (list (make-image "inactive:top-center-right-border.png")
	(make-image "active:top-center-right-border.png")))

;; 48*x19
(define top-center-left
  (list (make-image "inactive:top-center-left.png")
	(make-image "active:top-center-left.png")))

;; 48*x19
(define top-center-mid
  (list (make-image "inactive:top-center-mid.png")
	(make-image "active:top-center-mid.png")))

;; 48*x19
(define top-center-right
  (list (make-image "inactive:top-center-right.png")
	(make-image "active:top-center-right.png")))

;; 4x18
(define left-top-border
  (list (make-image "inactive:left-top-border.png")
	(make-image "active:left-top-border.png")))
(define left-top-border-shaped
  (list (make-image "inactive:left-top-border-shaped.png")
	(make-image "active:left-top-border-shaped.png")))

;; 5x16*
(define left-border
  (list (make-image "inactive:left-border.png")
	(make-image "active:left-border.png")))

;; 5x18
(define right-top-border
  (list (make-image "inactive:right-top-border.png")
	(make-image "active:right-top-border.png")))
(define right-top-border-shaped
  (list (make-image "inactive:right-top-border-shaped.png")
	(make-image "active:right-top-border-shaped.png")))

;; 6x16*
(define right-border
  (list (make-image "inactive:right-border.png")
	(make-image "active:right-border.png")))

;; 5x6
(define bottom-left-corner
  (list (make-image "inactive:bottom-left-corner.png")
	(make-image "active:bottom-left-corner.png")))

;; 32*x6
(define bottom-left-border
  (list (set-image-border
         (make-image "inactive:bottom-left-border.png") 0 30 0 0)
	(set-image-border
         (make-image "active:bottom-left-border.png") 0 30 0 0)))

;; 16*x6
(define bottom-right-border
  (list (make-image "inactive:bottom-right-border.png")
	(make-image "active:bottom-right-border.png")))

;; 6x6
(define bottom-right-corner
  (list (make-image "inactive:bottom-right-corner.png")
	(make-image "active:bottom-right-corner.png")))

;; 16x16
(define button-background
  `((inactive . ,(make-image "inactive:button.png"))
    (focused . ,(make-image "active:button.png"))
    (inactive-highlighted . ,(make-image "inactive:button-hilight.png"))
    (highlighted . ,(make-image "active:button-hilight.png"))
    (inactive-clicked . ,(make-image "inactive:button-pressed.png"))
    (clicked . ,(make-image "active:button-pressed.png"))))
(define menu-button
  `((inactive . ,(make-image "inactive:menu-button.png"))
    (focused . ,(make-image "active:menu-button.png"))
    (inactive-highlighted . ,(make-image "inactive:menu-button-hilight.png"))
    (highlighted . ,(make-image "active:menu-button-hilight.png"))
    (inactive-clicked . ,(make-image "inactive:menu-button-pressed.png"))
    (clicked . ,(make-image "active:menu-button-pressed.png"))))

(let ((make-button-fg
       (lambda (inactive active)
	 `((inactive . ,inactive)
	   (focused . ,active)
	   (inactive-highlighted . ,active)
	   (highlighted . ,active)
	   (inactive-clicked . ,active)
	   (clicked . ,active)))))

  (define minimize-fg
    (make-button-fg (make-image "inactive:minimize-button.png")
		    (make-image "active:minimize-button.png")))
  (define maximize-fg
    (make-button-fg (make-image "inactive:maximize-button.png")
		    (make-image "active:maximize-button.png")))
  (define close-fg
    (make-button-fg (make-image "inactive:close-button.png")
		    (make-image "active:close-button.png")))
  (define shade-fg
    (make-button-fg (make-image "inactive:shade-button.png")
		    (make-image "active:shade-button.png"))))

;; geometry computations

(define (title-left-width w)
  (let ((buttons (table-ref button-table w)))
    (max 0 (min (- (car (window-dimensions w))
		   (* (length (cdr buttons)) 18))
		(+ (text-width (window-name w)) 52
		   (* (length (car buttons)) 18))))))

(define (top-border-left-width w)
  (let ((buttons (table-ref button-table w)))
    (max 0 (min (- (car (window-dimensions w)) (* (length (cdr buttons)) 18))
		(+ (title-left-width w) -43)))))

(define (bottom-border-left-width w)
  (max 0 (min (car (window-dimensions w))
	      (+ (title-left-width w)
		 (quotient (cdr (window-dimensions w)) 2)))))

(define (vertical-justification)
  ;; `center' justification adjusted for the 3-pixel top-border
  (max 0 (- (/ (- 22 (font-height default-font)) 2) 3)))

(define (horizontal-justification w)
  (+ (* (length (car (table-ref button-table w))) 18) 2))

;; recolouring images

(define (foreground-color)
  (if (colorp Crux:normal-color)
      Crux:normal-color
    (initialize-gtkrc)
    (if (colorp (nth 3 gtkrc-background))
	(nth 3 gtkrc-background)
      (get-color "steelblue"))))

;; Recolor all images that need recolouring. Precalculates the lookup
;; tables first.
(define (recolor-all)
  ;; Use the SELECTED state of the background colors as the
  ;; midpoint of the gradient for recolouring images. (This is
  ;; usually a bright, contrasting colour, and thus is the
  ;; best choice. It works particularly well with the Eazel-Foo
  ;; themes)
  (let ((recolorer (make-image-recolorer (foreground-color)
					 #:zero-channel blue-channel
					 #:index-channel green-channel)))
    (mapc (lambda (x) (mapc recolorer (cdr x)))
	  (list top-left-border
		top-center-left-border
		top-center-left
		top-center-mid
		left-top-border
		left-top-border-shaped
		left-border
		bottom-left-corner
		bottom-left-border))
    (mapc (lambda (x)
	    (recolorer (cdr x))) menu-button)))

;; window icons

(define icon-table (make-weak-table eq-hash eq))

(define (window-icon w)
  (when Crux:show-window-icons
    (or (table-ref icon-table w)
	(let ((icon (window-icon-image w)))
	  (when icon
	    (let ((scaled (scale-image icon 12 12)))
	      (table-set icon-table w scaled)
	      scaled))))))

;; frames

(define common-frame-parts
  `(((background . ,top-left-border)
     (left-edge . -5)
     (top-edge . -22)
     (class . top-left-corner))

    ((background . ,top-center-left-border)
     (left-edge . 11)
     (top-edge . -22)
     (width . ,top-border-left-width)
     (class . top-border))
    ((background . ,top-center-right-border)
     (left-edge . ,(lambda (w) (+ (top-border-left-width w) 11)))
     (right-edge . 10)
     (top-edge . -22)
     (class . top-border))

    ((background . ,top-center-left)
     (left-edge . -2)
     (width . ,(lambda (w) (+ (title-left-width w) 2 -48)))
     (top-edge . -19)
     (text . ,window-name)
     (foreground . ("black" "white"))
     (x-justify . ,horizontal-justification)
     (y-justify . ,vertical-justification)
     (class . title))
    ((background . ,top-center-mid)
     (left-edge . ,(lambda (w) (max 0 (- (title-left-width w) 48))))
     (top-edge . -19)
     (class . title))
    ((background . ,top-center-right)
     (left-edge . ,title-left-width)
     (right-edge . -1)
     (top-edge . -19)
     (class . title))

    ((background . ,top-right-border)
     (right-edge . -6)
     (top-edge . -22)
     (class . top-right-corner))))

(define normal-frame
  `(,@common-frame-parts

    ((background . ,left-top-border)
     (left-edge . -5)
     (top-edge . -19)
     (class . top-left-corner))
    ((background . ,right-top-border)
     (right-edge . -6)
     (top-edge . -19)
     (class . top-right-corner))

    ((background . ,left-border)
     (left-edge . -5)
     (top-edge . 0)
     (bottom-edge . 0)
     (class . left-border))

    ((background . ,bottom-left-corner)
     (left-edge . -5)
     (bottom-edge . -6)
     (class . bottom-left-corner))

    ((background . ,right-border)
     (right-edge . -6)
     (top-edge . 0)
     (bottom-edge . 0)
     (class . right-border))

    ((background . ,bottom-right-corner)
     (right-edge . -6)
     (bottom-edge . -6)
     (class . bottom-right-corner))

    ((background . ,bottom-left-border)
     (left-edge . 0)
     (width . ,bottom-border-left-width)
     (bottom-edge . -6)
     (class . bottom-border))
    ((background . ,bottom-right-border)
     (left-edge . ,bottom-border-left-width)
     (right-edge . 0)
     (bottom-edge . -6)
     (class . bottom-border))))

(define shaped-frame
  `(,@common-frame-parts

    ((background . ,left-top-border-shaped)
     (left-edge . -5)
     (top-edge . -19)
     (class . top-left-corner))
    ((background . ,right-top-border-shaped)
     (right-edge . -6)
     (top-edge . -19)
     (class . top-right-corner))))

;; packing buttons

(define button-map
  `((iconify-button . ,minimize-fg)
    (maximize-button . ,maximize-fg)
    (close-button . ,close-fg)
    (menu-button . ,window-icon)
    (shade-button . ,shade-fg)))

(define (button-theme type)
  (let ((style (cdr (or (assq Crux:button-theme Crux:button-themes)
			(assq 'default Crux:button-themes)))))
    (if (eq type 'transient)
	(cadr style)
      (car style))))

(define (make-buttons spec background edge)

  (define (make-button class fg point)
    `((background . ,background)
      (foreground . ,fg)
      (x-justify . 2)
      (y-justify . 2)
      (,edge . ,point)
      (top-edge . -19)
      (class . ,class)
      (removable . t)))

  (do ((rest spec (cdr rest))
       (point -1 (+ point 18))
       (out '() (cons (make-button (car rest)
				   (cdr (assq (car rest) button-map))
				   point) out)))
      ((null rest) out)))

;; misc stuff

(define (rebuild-all)
  (rebuild-frames-with-style 'Crux))

(define (reframe-all)
  (reframe-windows-with-style 'Crux))

(define (reset-icon w)
  (table-unset icon-table w)
  (rebuild-frame w))

(define (color-changed)
  (recolor-all)
  (reframe-all))

(define (make-frame w frame buttons)
  (table-set button-table w buttons)
  (append frame
	  (make-buttons (car buttons) menu-button 'left-edge)
	  (make-buttons (reverse (cdr buttons)) button-background 'right-edge)))

(define (get-frame w type)
  (case type
    ((default)
     (make-frame w normal-frame (button-theme 'normal)))
    ((transient)
     (make-frame w normal-frame (button-theme 'transient)))
    ((shaped)
     (make-frame w shaped-frame (button-theme 'normal)))
    ((shaped-transient)
     (make-frame w shaped-frame (button-theme 'transient)))))

;; initialization

(define initialize-gtkrc
  (let ((done nil))
    (lambda ()
      (unless done
	(require 'gtkrc)
	;; recolour everything when the GTK theme changes
	(gtkrc-call-after-changed color-changed)
	(setq done t)))))

;; setup the initial colours
(recolor-all)

;; register the theme
(add-frame-style 'Crux get-frame)

;; recalibrate frames when the window-name changes
(call-after-property-changed '(WM_NAME _NET_WM_NAME) rebuild-frame)

;; Konqueror changes the icon by page.
(call-after-property-changed 'WM_HINTS reset-icon)
