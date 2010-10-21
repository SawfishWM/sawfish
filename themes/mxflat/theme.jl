;; mxflat/mxflat.jl
;;
;; theme.jl for mxflat (v0.7.3) sawfish theme
;;
;; Copyright (C) 2002 mx & ta
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; authors: mx (email) & ta (dev.null@gmx.net)

(require 'gradient)
;; need hash tables for icon cache
(require 'rep.data.tables)
(require 'rep.io.timers)
(require 'sawfish.wm.util.recolor-image)

;; required when gaoling is turned off (for debugging)
;;(require 'rep.mmsystem)
;;(require 'sawfish.wm.custom)
;;(require 'sawfish.wm.colors)
;;(require 'sawfish.wm.images)
;;(require 'sawfish.wm.fonts)

;;  ---------
;;; REDRAWING
;;  ---------

;;
;; redrawing:internal vars
;;

;; we need to define theme here
;; so we can reach them from our defcustom clauses

;; cache for window icons
(defvar icon-cache (make-weak-table eq-hash eq))

;; holds the shadow side for the current selected option
(defvar shadow-side nil)

;; for now empty, see below
(define (recolor-all))

;;
;; redrawing:functions
;;

(define (get-symbol #!rest strings)
  (intern (mapconcat identity strings "")))

(define (check-symbol #!rest strings)
  (let ((name (intern (mapconcat identity strings ""))))
    (if (boundp name)
        (symbol-value name)
      nil)))

(define (refresh-one-frame w)
  (if (eq (window-get w 'current-frame-style) 'mxflat)
      (refresh-window w)))

(define (refresh-all-frames)
  (map-windows refresh-window))

(define (rebuild-all-frames)
  (rebuild-frames-with-style 'mxflat))

;; same as rebuild but this also deletes the icon cache
(define (recreate-all-frames)
  (reframe-windows-with-style 'mxflat))

;; after the light source direction is updated we have to destroy the cache
;; with the correct shadow sides, it will be recreated on next access
(define (after-set-shadow-light-source)
  (setq shadow-side nil)
  (recreate-all-frames))

(define update-interval nil)
(define update-timer
  (make-timer (lambda ()
                (refresh-all-frames)
                (if update-interval
                    (set-timer update-timer update-interval)))))

(define (after-set-title-periodic-update)
  (if (check-symbol "mxflat:title-periodic-update")
      (set-timer update-timer update-interval)
    (delete-timer update-timer)))

(define (after-set-title-update-interval)
  (setq update-interval (check-symbol "mxflat:title-update-interval"))
  (after-set-title-periodic-update))

(define (recreate-all-frames-clear-cache)
  ;; reset icon cache
  (setq icon-cache (make-weak-table eq-hash eq))
  (recreate-all-frames))

(define (recreate-all-frames-recolor-all)
  (recolor-all)
  (recreate-all-frames))

;;  -------------
;;; CONFIGURATION
;;  -------------

;;
;; configuration:keymaps
;;

;; we need to define our default keymaps here,
;; so we can reach them from our defcustom clauses

(defvar sticky-button-keymap
  (bind-keys (make-keymap)
             "Button1-Off" 'toggle-window-sticky))

(defvar previous-button-keymap
  (bind-keys (make-keymap)
             "Button3-Off" '(command-sequence
                             `((send-to-workspace
                                ,(1+ (car (workspace-limits))))
                               (activate-workspace
                                ,(1+ (car (workspace-limits))))))
             "Button2-Click" 'popup-workspace-list
             "Button1-Off" 'send-to-previous-workspace))

(defvar next-button-keymap
  (bind-keys (make-keymap)
             "Button3-Off" '(command-sequence
                             `((send-to-workspace
                                ,(1+ (cdr (workspace-limits))))
                               (activate-workspace
                                ,(1+ (cdr (workspace-limits))))))
             "Button2-Click" 'popup-workspace-list
             "Button1-Off" 'send-to-next-workspace))

(defvar configure-button-keymap
  (bind-keys (make-keymap)
             "Button1-Off" 'customize
             "Button3-Off" '(call-command
                             (lambda ()
                               (if mxflat:custom-title
                                   (setq mxflat:custom-title nil)
                                 (setq mxflat:custom-title t))
                               ;;(reframe-windows-with-style 'mxflat)
                               (map-windows refresh-window)))))

(defvar always-on-top-button-keymap
  (bind-keys (make-keymap)
             "Button1-Off" '(call-command
                             (lambda ()
                               (if (> (window-get (current-event-window)
                                                  'depth)
                                      0)
                                   (window-put (current-event-window)
                                               'depth 0)
                                 (window-put (current-event-window)
                                             'depth
                                             mxflat:always-on-top-depth))
                               (reframe-window (current-event-window))))))

(defvar lock-button-keymap
  (bind-keys (make-keymap)
             "Button1-Off" '(call-command
                             (lambda ()
                               (if (window-get (current-event-window)
                                               'fixed-position)
                                   (window-put (current-event-window)
                                               'fixed-position nil)
                                 (window-put (current-event-window)
                                             'fixed-position t))
                               (reframe-window (current-event-window))))))


;;
;; configuration:groups
;;

;; there are just too many options
;; so we put them into several groups/tabs
(defgroup mxflat:group
  "mxflat"
  :group appearance)

(defgroup mxflat:title-group
  "title"
  :group (appearance mxflat:group))

(defgroup mxflat:title-basic-group
  "basic"
  :group (appearance mxflat:group mxflat:title-group))

(defgroup mxflat:title-gradient-group
  "gradient"
  :group (appearance mxflat:group mxflat:title-group))

(defgroup mxflat:title-colors-group
  "colors"
  :group (appearance mxflat:group mxflat:title-group))

(defgroup mxflat:buttons-group
  "buttons"
  :group (appearance mxflat:group))

(defgroup mxflat:buttons-basic-group
  "basic"
  :group (appearance mxflat:group mxflat:buttons-group))

(defgroup mxflat:buttons-positions1-group
  "positions 1"
  :group (appearance mxflat:group mxflat:buttons-group))

(defgroup mxflat:buttons-positions2-group
  "positions 2"
  :group (appearance mxflat:group mxflat:buttons-group))

(defgroup mxflat:buttons-keymaps1-group
  "keymaps 1"
  :group (appearance mxflat:group mxflat:buttons-group))

(defgroup mxflat:buttons-keymaps2-group
  "keymaps 2"
  :group (appearance mxflat:group mxflat:buttons-group))

(defgroup mxflat:button-colors-group
  "colors"
  :group (appearance mxflat:group mxflat:buttons-group))

(defgroup mxflat:border-group
  "borders"
  :group (appearance mxflat:group))

(defgroup mxflat:border1-group
  "basic"
  :group (appearance mxflat:group mxflat:border-group))

(defgroup mxflat:border-extended-group
  "extended"
  :group (appearance mxflat:group mxflat:border-group))

(defgroup mxflat:border-colors-group
  "colors"
  :group (appearance mxflat:group mxflat:border-group))


;;
;; configuration:title-basic
;;

(defcustom mxflat:decoration-mode
  'normal
  "decoration mode"
  :tooltip "Set the mode 'when to draw a titlebar and window \
buttons'.  The decoration mode 'all' means that all windows, even \
transients/dialogs, will get a titlebar and buttons.  None means no \
window will get any :-).  Usually transients only have a \
contents-border.  This option is for reimplementing a sawfish feature \
that is gone in the latest version."
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type symbol
  :options (normal all none)
  :after-set recreate-all-frames)

(defcustom mxflat:title-height
  16
  "titlebar height (9 - 1000 pixel)"
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type (number 9 1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:custom-title
  nil
  "customize title string"
  :tooltip "See the tooltip for the 'title string' option below for \
more information.  (NOTE: I hope the redrawing problems with this \
option are solved.  If not, write an email please!)"
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type boolean
  :after-set refresh-all-frames)

(defcustom mxflat:title-string
  "$fullname |$widthx$height-($x-pos,$y-pos)@$current-ws/$last-ws-$(DISPLAY)"
  "title string"
  :tooltip "The string you insert here will be your new title \
string.  Variables always start with a dollar-sign ($) and are \
replaced with their actual values.  To get the value of an environment \
variable use $(variable), eg $(DISPLAY).  To get the value of an \
rep/sawfish function use $[fuction], eg $[current-time-string].  The \
following vars are built-in: $name, $fullname, $width, $width-content, \
$height, $height-content, $depth, $x-pos, $y-pos, $current-ws and \
$last-ws. (NOTE: I hope the redrawing problems with this option are \
solved.  If not, write an email please!"
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type string
  :depends mxflat:custom-title
  :after-set refresh-all-frames)

(defcustom mxflat:title-periodic-update
  nil
  "update title string periodically"
  :tooltip "This is useful if your title string changes without user \
interaction (eg because it displays the current time or something like \
that)."
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type boolean
  :depends mxflat:custom-title
  :after-set after-set-title-periodic-update)

(defcustom mxflat:title-update-interval
  1000
  "title string update interval (1 - 1000000 s)"
  :tooltip "This is useful if your title string changes without user \
interaction (e.g., because it displays the current time or something \
like that)."
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type (number 1 1000000)
  :depends mxflat:custom-title
  :after-set after-set-title-update-interval)

(defcustom mxflat:title-font
  default-font
  "title string font"
  :tooltip "You should really choose a better font.  Verdana bold 8 \
pt looks nice."
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type font
  :after-set rebuild-all-frames)

(defcustom mxflat:customize-title-position
  nil
  "customize title position"
  :tooltip "By default the title string is centered horizontally and \
vertically."
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type boolean
  :after-set rebuild-all-frames)

(defcustom mxflat:title-x-justify
  'center
  "title string horizontal alignment"
  :tooltip "'real-center' means to take the whole titlebar into \
account while centering the string. when selecting 'center' only the \
distance between the buttons is taken into account."
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type symbol
  :depends mxflat:customize-title-position
  :options (left center real-center right)
  :after-set rebuild-all-frames)

(defcustom mxflat:title-x-adjustment
  0
  "title string horizontal align adjustment (-10000 - 10000 pixel)"
  :tooltip "The horizontal position of the title string can only be \
adjusted if the horizontal alignment is not 'center'.  The range also \
depends on the setting of the option above.  It the title string is \
already aligned left, you cannot adjust it to be even further left \
because there are button in the way."
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type (number -10000 10000)
  :depends mxflat:customize-title-position
  :after-set rebuild-all-frames)

(defcustom mxflat:title-y-justify
  'center
  "title string vertical alignment"
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type symbol
  :depends mxflat:customize-title-position
  :options (top center bottom)
  :after-set rebuild-all-frames)

(defcustom mxflat:title-y-adjustment
  0
  "title string vertical align adjustment (-10000 - 10000 pixel)"
  :group (appearance mxflat:group mxflat:title-group mxflat:title-basic-group)
  :type (number -10000 10000)
  :depends mxflat:customize-title-position
  :after-set rebuild-all-frames)

;;
;; configuration:title-gradient
;;

(defcustom mxflat:title-gradient
  nil
  "gradient titlebar background (experimental)"
  :tooltip "When activating the gradient background the normal \
titlebar background colors will be ignored.  Besides the button \
background colors will be ignored.  (NOTE: This can be very cpu \
intensive if you set the quality to a low value, i.e., high quality.)"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:gradient-direction
  'horizontal
  "gradient mode"
  :tooltip "With gradient mode vertical or diagonal only the first \
or last (reversed) 2 colors in the list are used."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :options (none horizontal vertical diagonal)
  :depends mxflat:title-gradient
  :type symbol
  :after-set recreate-all-frames
  )

(defcustom mxflat:gradient-scale
  3
  "gradient quality (1 best - 20 worst)"
  :tooltip "The value here is the factor the calculated gradient image \
will be scaled down.  This increases speed but decreases quality."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :depends mxflat:title-gradient
  :type (number 1 20)
  :after-set recreate-all-frames)

(defcustom mxflat:titlebar-gradient-color-mode
  'rainbow
  "gradient color mode"
  :tooltip "the custom mode will respect the colors set in the list below."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :options (rainbow jamaica rgb custom)
  :depends mxflat:title-gradient
  :type symbol
  :after-set recreate-all-frames)

(defcustom mxflat:titlebar-gradient-bgcolors
  nil
  nil
  :tooltip "With gradient mode vertical or diagonal only the first or \
last (reversed) 2 colors in the list are used.  Besides the gradient \
color mode must be set to custom to activate this list."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :widget-flags (expand-horizontally)
  :depends mxflat:title-gradient
  :type* `(list color "titlebar background colors")
  :after-set recreate-all-frames)

(defcustom mxflat:titlebar-gradient-bgcolors-reverse
  nil
  "reverse colors"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :depends mxflat:title-gradient
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:color-hue
  0
  "unfocused/highlighted titlebar colors hue (0 - 360)"
  :tooltip "This value changes the hue of a color, by moving (0 - 360) \
degress around the color cycle...180 for example inverts the color"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :depends mxflat:title-gradient
  :type (number 0 360)
  :after-set recreate-all-frames)

(defcustom mxflat:color-saturation
  50
  "unfocused/highlighted titlebar colors saturation (0 - 100 percent of the \
focused color)"
  :tooltip "The saturation of a color is a measurement of the color \
intensity"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :depends mxflat:title-gradient
  :type (number 0 100)
  :after-set recreate-all-frames)

(defcustom mxflat:color-brightness
  90
  "unfocused/highlighted titlebar colors brightness (0 - 100 percent of the \
focused color)"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-gradient-group)
  :depends mxflat:title-gradient
  :type (number 0 100)
  :after-set recreate-all-frames)

;;
;; configuration:title-colors
;;

(defcustom mxflat:titlebar-bgcolor-focused
  (get-color "#fffbde")
  "  focused titlebar color"
  :tooltip "This is one of the 4 base colors.  You can set these 4 \
colors and keep the extended colors deactivated.  All other color \
options of the theme will then depend on these 4 colors."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:titlebar-bgcolor-unfocused
  (get-color "#cedfef")
  "unfocused titlebar color"
  :tooltip "This is one of the 4 base colors.  You can set these 4 \
colors and keep the extended colors deactivated.  All other color \
options of the theme will then depend on these 4 colors."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:text-color-focused
  (get-color "#000000")
  "  focused title string color"
  :tooltip "This is one of the 4 base colors.  You can set these 4 \
colors and keep the extended colors deactivated.  All other color \
options of the theme will then depend on these 4 colors."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :after-set recreate-all-frames-recolor-all)

(defcustom mxflat:text-color-unfocused
  (get-color "#838383")
  "unfocused title string color"
  :tooltip "This is one of the 4 base colors.  You can set these 4 \
colors and keep the extended colors deactivated.  All other color \
options of the theme will then depend on these 4 colors."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :after-set recreate-all-frames-recolor-all)

(defcustom mxflat:customize-extended-text-colors
  nil
  "customize extended title string colors"
  :tooltip "You can customize additional colors of the title string.  \
If you deactivate this, these colors will be set to the focused title \
string color."
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:text-color-highlighted
  (get-color "#000000")
  "  focused highlighted title string color"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :depends mxflat:customize-extended-text-colors
  :after-set recreate-all-frames)

(defcustom mxflat:text-color-inactive-highlighted
  (get-color "#000000")
  "unfocused highlighted title string color"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :depends mxflat:customize-extended-text-colors
  :after-set recreate-all-frames)

(defcustom mxflat:text-color-clicked
  (get-color "#000000")
  "  focused clicked title string color"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :depends mxflat:customize-extended-text-colors
  :after-set recreate-all-frames)

(defcustom mxflat:text-color-inactive-clicked
  (get-color "#000000")
  "unfocused clicked title string color"
  :group (appearance mxflat:group mxflat:title-group
                     mxflat:title-colors-group)
  :type color
  :depends mxflat:customize-extended-text-colors
  :after-set recreate-all-frames)


;; configuration:buttons-basic
;;

(defcustom mxflat:button-icon-set
  'default-9
  "button icon set (experimental)"
  :tooltip "The number at the end is the width/height of the button \
icons in the icon set.  (NOTE: YOU NEED TO RESTART SAWFISH AFTER \
CHANGING THE BUTTON ICON SET!)"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :options (default-9 default-12 square-6 square-9 square-12
             square-15 square-18 ball-9 ball-12)
  :type symbol
  :after-set recreate-all-frames-clear-cache)

(defcustom mxflat:customize-button-size
  nil
  "customize button size"
  :tooltip "By default the button size follows the size of the \
titlebar, but by activating this you can set it to something \
different."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :type boolean
  :after-set rebuild-all-frames)

(defcustom mxflat:button-size
  14
  "buttons size (1 - 1000 pixel)"
  :tooltip "A buttons size that is bigger than your current titlebar \
height is ignored."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :depends mxflat:customize-button-size
  :type (number 1 1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:button-y-alignment
  'center
  "button vertical alignment"
  :tooltip "If a button is smaller than the titlebar you might want to \
set its vertical alignment."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :depends mxflat:customize-button-size
  :options (top center bottom)
  :type symbol
  :after-set rebuild-all-frames)

(defcustom mxflat:scale-buttons
  nil
  "scale button icons (experimental)"
  :tooltip "In case you don't like the default size of 9 pixel you \
might activate button-scaling.  YOU NEED TO RESTART SAWFISH AFTER \
CHANGING THE BUTTON SCALE!"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :type boolean
  :after-set recreate-all-frames-clear-cache)

(defcustom mxflat:button-scale
  9
  "button icon scale; changes here need a restart of sawfish (0 - 1000 \
pixel)"
  :tooltip "in case you don't like the default size of 9 pixel you \
might activate button-scaling. (NOTE: YOU NEED TO RESTART SAWFISH \
AFTER CHANGING THE BUTTON SCALE!)"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :depends mxflat:scale-buttons
  :type (number 0 1000)
  :after-set recreate-all-frames-clear-cache)

(defcustom mxflat:menu-button-icon
  nil
  "display window icon on menu button"
  :tooltip "Many people like having the window-icon displayed on the \
menu-button.  This icon is defined by the application itself and is \
also shown in your pager and task-list."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:menu-button-scale
  12
  "menu icon scale (0 - 1000 pixel)"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-basic-group)
  :depends mxflat:menu-button-icon
  :type (number 0 1000)
  :after-set recreate-all-frames-clear-cache)

;;
;; configuration:buttons-position1
;;

(defcustom mxflat:button-distance
  3
  "button distance (0 - 1000 pixel)"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions1-group)
  :type (number 0  1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:button-left-offset
  1
  "buttons left offset (0 - 1000 pixel)"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions1-group)
  :type (number 0  1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:button-right-offset
  1
  "buttons right offset (0 - 1000 pixel)"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions1-group)
  :type (number 0  1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:customize-buttons
  nil
  "customize button position (-1 -2 -3 .. title string .. +3 +2 +1)"
  :tooltip "Enabling this will allow you to set the positon of each \
button in the titlebar manually.  A negative/positiv value means the \
button will be drawn on left/right side of the titlebar."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions1-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:button-position-model
  'title-height
  "button position model"
  :tooltip "'title-height' means calculate position as multiple of \
title height. when selecting 'pixel' you can adjust the button \
position pixelwise, but keep in mind that several buttons might \
slide on top of each other. 'none' means no buttons at all."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions1-group)
  :options (title-height pixel none)
  :depends mxflat:customize-buttons
  :type symbol
  :after-set rebuild-all-frames)

;;
;; configuration:buttons-position2
;;

(defcustom mxflat:menu-button-position
  -1
  "menu button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:shade-button-position
  -2
  "shade button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:iconify-button-position
  +3
  "iconify button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:maximize-button-position
  +2
  "maximize button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:close-button-position
  +1
  "close button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:sticky-button-position
  -3
  "sticky button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:previous-button-position
  -5
  "previous workspace button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:next-button-position
  -6
  "next workspace button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:configure-button-position
  0
  "configure button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:info-button-position
  0
  "info button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:always-on-top-button-position
  0
  "always-on-top button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

(defcustom mxflat:lock-button-position
  0
  "lock button position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-positions2-group)
  :depends mxflat:customize-buttons
  :type (number -10000 10000)
  :after-set rebuild-all-frames)

;;
;; configuration:buttons-keymaps1
;;

(defcustom mxflat:customize-sticky-keymap
  nil
  "customize sticky button keymap"
  :tooltip "By default a button will work as it's name suggests.
Button1: toggle sticky state of window"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps1-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:sticky-button-keymap
  nil
  nil
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps1-group)
  :widget-flags (expand-horizontally)
  :depends mxflat:customize-sticky-keymap
  :type keymap)

(defcustom mxflat:customize-previous-keymap
  nil
  "customize previous button keymap"
  :tooltip "By default a button will work as it's name suggests. \
Button1: send window to previous workspace - button2 workspace-list \
- button3 send window to first workspace - button6 pack window left"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps1-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:previous-button-keymap
  nil
  nil
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps1-group)
  :widget-flags (expand-horizontally)
  :depends mxflat:customize-previous-keymap
  :type keymap)

(defcustom mxflat:customize-next-keymap
  nil
  "customize next button keymap"
  :tooltip "By default a button will work as it's name suggests. \
Button1: send window to next workspace - button2 workspace-list - \
button3 send window to last workspace - button6 pack window right"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps1-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:next-button-keymap
  nil
  nil
  :tooltip "button1: send window to next workspace - button2 \
workspace-list - button3 send window to last workspace - button6 \
pack window right"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps1-group)
  :widget-flags (expand-horizontally)
  :depends mxflat:customize-next-keymap
  :type keymap)

;;
;; configuration:buttons-keymaps2
;;

(defcustom mxflat:customize-configure-keymap
  nil
  "customize configure button keymap"
  :tooltip "By default a button will work as it's name suggests. \
Button1: start sawfish-config - button3: toggle custom title string"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:configure-button-keymap
  nil
  nil
  :tooltip "Button1: start sawfish-config - button3: toggle custom \
title string"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :widget-flags (expand-horizontally)
  :depends mxflat:customize-configure-keymap
  :type keymap)

(defcustom mxflat:customize-info-keymap
  nil
  "customize info button keymap"
  :tooltip "By default a button will work as it's name suggests. \
Button1: window list - button3 window snooper (not included by \
default)"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:info-button-keymap
  nil
  nil
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :widget-flags (expand-horizontally)
  :depends mxflat:customize-info-keymap
  :type keymap)

(defcustom mxflat:always-on-top-depth
  1
  "always on top depth (-1000 - 1000)"
  :tooltip "A window will have this depth after you have pressed the \
always-on-top button.  0 is the normal window level.  Every positive \
value means above other windows, a negative value means below.  Keep \
it at a positive value in order to have the always-on-top button do \
it's job."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :type (number -1000 1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:customize-always-on-top-keymap
  nil
  "customize always-on-top button keymap"
  :tooltip "By default a button will work as it's name suggests. \
Button1: sets window depth to the adjusted value."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:always-on-top-button-keymap
  nil
  nil
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :depends mxflat:customize-always-on-top-keymap
  :widget-flags (expand-horizontally)
  :type keymap)

(defcustom mxflat:customize-lock-keymap
  nil
  "customize lock button keymap"
  :tooltip "By default a button will work as it's name suggests. \
Button1: lock window position"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:lock-button-keymap
  nil
  nil
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:buttons-keymaps2-group)
  :depends mxflat:customize-lock-keymap
  :widget-flags (expand-horizontally)
  :type keymap)

;;
;; configuration:button-colors
;;

(defcustom mxflat:custom-button-colors
  nil
  "customize button-colors"
  :tooltip "When you active this you will be able to set custom \
button colors.  By default the button colors depend on the titlebar \
colors."
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :type boolean
  :after-set recreate-all-frames-recolor-all)

(defcustom mxflat:button-bgcolor-focused
  (get-color "#fffbde")
  "  focused button background color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:button-bgcolor-unfocused
  (get-color "#cedfef")
  "unfocused button background color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:button-bgcolor-highlighted
  (get-color "#def3ff")
  "  focused highlighted button background color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:button-bgcolor-inactive-highlighted
  (get-color "#fffbde")
  "unfocused highlighted button background color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:button-bgcolor-clicked
  (get-color "#cecece")
  "  focused clicked button background color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:button-bgcolor-inactive-clicked
  (get-color "#cecece")
  "unfocused clicked button background color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames)

(defcustom mxflat:button-fgcolor-focused
  (get-color "#000000")
  "  focused button foreground color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames-recolor-all)

(defcustom mxflat:button-fgcolor-inactive
  (get-color "#838383")
  "unfocused button foreground color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames-recolor-all)

(defcustom mxflat:button-fgcolor-highlighted
  (get-color "#000000")
  "  focused highlighted button foreground color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames-recolor-all)

(defcustom mxflat:button-fgcolor-clicked
  (get-color "#000000")
  "  focused clicked button foreground color"
  :group (appearance mxflat:group mxflat:buttons-group
                     mxflat:button-colors-group)
  :depends mxflat:custom-button-colors
  :type color
  :after-set recreate-all-frames-recolor-all)

;;
;; configuration:border-basic
;;

(defcustom mxflat:border-width-unmaximized
  1
  "border width (0 - 1000 pixel)"
  :tooltip "set the width border around the window."
  :group (appearance mxflat:group mxflat:border-group mxflat:border1-group)
  :type (number 0 1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:border-width-maximized
  1
  "border width for maximized windows (0 - 1000 pixel)"
  :tooltip "When you like having a big border around your windows you \
might have noticed that sawfish still draws that big border when the \
window is maximized.  If you don't want that, set the border width for \
maximized windows smaller than the border width for unmaximized \
windows.  (NOTE: this feature is still buggy as sawfish does not \
resize the windows correctly yet, so the best might be to set it to \
the normal border width.)"
  :group (appearance mxflat:group mxflat:border-group mxflat:border1-group)
  :type (number 0 1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:custom-border-focused
  nil
  "customize border for focused windows (experimental)"
  :tooltip "(NOTE: this works but it is VERY cpu intensive and might \
slow down your computer noticably.  So turn it on at your own risk.)"
  :group (appearance mxflat:group mxflat:border-group mxflat:border1-group)
  :type boolean
  :after-set rebuild-all-frames)

(defcustom mxflat:border-width-focused
  1
  "border width for focused windows (0 - 1000 pixel)"
  :tooltip "(NOTE: this works but it is VERY cpu intensive and might \
slow down your computer noticably.  So turn it on at your own risk.)"
  :group (appearance mxflat:group mxflat:border-group mxflat:border1-group)
  :type (number 0 1000)
  :depends mxflat:custom-border-focused
  :after-set rebuild-all-frames)

(defcustom mxflat:border-width-titlebar-contents
  1
  "title-contents border width (0 - 1000 pixel)"
  :tooltip "Sets the widths of the line between titlebar and contents \
window"
  :group (appearance mxflat:group mxflat:border-group mxflat:border1-group)
  :type (number 0 1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:border-offset
  -1
  "border offset (-1000 - 1000 pixel)"
  :tooltip "Adjust the point where the border actually starts.  Many \
gtk2 apps draw a small black border around their contents window and \
this is just ugly when sawfish draws another border around the window. \
With a border offset of -1 the sawfish window border will overlay the \
gtk2 border exactly."
  :group (appearance mxflat:group mxflat:border-group mxflat:border1-group)
  :type (number -1000 1000)
  :after-set rebuild-all-frames)

(defcustom mxflat:keep-borders-when-shaded
  nil
  "keep window borders and corners at their place when shading a window"
  :tooltip "When shading a window borders and corners will stay where \
they were and not merge with the titlebar."
  :group (appearance mxflat:group mxflat:border-group mxflat:border1-group)
  :type boolean
  :after-set recreate-all-frames)

;;
;; configuration:border-extended
;;

(defcustom mxflat:show-corners
  t
  "show window corners"
  :tooltip "The corners are used to resize a window in two directions \
at the same time.  Usually the width and length of window corners are \
determined by the border width but you can adjust them below."
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:corner-length-adjustment
  20
  "corners length adjustement (-1 - 100 pixel)"
  :tooltip "Increasing the corner length will grow the window corners \
along the window border (i.e. make them longer).  With a border width \
of 1 pixel it is hard to grab the 1 pixel corner to be able to resize \
the window in two directions; thus, make it bigger."
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type (number -1 100)
  :depends mxflat:show-corners
  :after-set rebuild-all-frames)

(defcustom mxflat:corner-direction
  'outside
  "corner direction"
  :"inside or outside the window border"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type symbol
  :options (outside inside)
  :depends mxflat:show-corners
  :after-set recreate-all-frames)

(defcustom mxflat:corner-width-adjustment
  1
  "corners width adjustement (0 - 100 pixel)"
  :tooltip "This option makes the window corners broader. with a \
border width of 1 pixel it is hard to grab this 1 pixel corner to be \
able to resize the window in two directions; thus, make it bigger."
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type (number 0 100)
  :depends mxflat:show-corners
  :after-set rebuild-all-frames)

(defcustom mxflat:corner-width-adjustment-maximized
  0
  "corners width adjustement for maximized windows (0 - 100 pixel)"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type (number 0 100)
  :depends mxflat:show-corners
  :after-set rebuild-all-frames)

(defcustom mxflat:fake-shadow
  nil
  "fake shadow (experimental)"
  :tooltip "Draw a small additional border around your windows \
emulating a solid shadow effect.  Use the options below to adjust its \
width and direction.  (NOTE: this is VERY cpu intensive and might slow \
down your computer noticably.  So turn it on at your own risk.)"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type boolean
  :after-set recreate-all-frames)

(defcustom mxflat:fake-shadow-focused-only
  nil
  "fake shadow only on focused windows"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type boolean
  :depends mxflat:fake-shadow
  :after-set recreate-all-frames)

(defcustom mxflat:shadow-light-source
  'north-west
  "fake shadow light source"
  :tooltip "Set the direction of the fake shadow.  Usually only 2 or 3 \
borders of your window have a fake shadow.  This depends on where the \
light comes from."
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type symbol
  :options (north east south west north-east north-west
                  south-east south-west)
  :depends mxflat:fake-shadow
  :after-set after-set-shadow-light-source)

(defcustom mxflat:shadow-light-distance
  3
  "fake shadow light distance (0 - 100)"
  :tooltip "This option has only an effect on the north, east, south \
and west light sources.  The further away the light source is, the \
smaller will the two 'side' shadows be."
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type (number 1 100)
  :depends mxflat:fake-shadow
  :after-set rebuild-all-frames)

(defcustom mxflat:shadow-width
  4
  "fake shadow width (0 - 100 pixel)"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type (number 0 100)
  :depends mxflat:fake-shadow
  :after-set rebuild-all-frames)

(defcustom mxflat:shadow-color
  (get-color "#838383")
  "shadow color"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-extended-group)
  :type color
  :depends mxflat:fake-shadow
  :after-set rebuild-all-frames)

;;
;; configuration:border-colors
;;

(defcustom mxflat:customize-border-colors
  nil
  "customize border colors"
  :tooltip "You can customize your border colors here. If you \
deactivate this, these colors will be set to the focused title \
string color."
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-colors-group)
  :type boolean
  :after-set rebuild-all-frames)

(defcustom mxflat:border-color-focused
  (get-color "#000000")
  "  focused border color"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-colors-group)
  :type color
  :depends mxflat:customize-border-colors
  :after-set rebuild-all-frames)

(defcustom mxflat:border-color-unfocused
  (get-color "#000000")
  "unfocused border color"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-colors-group)
  :type color
  :depends mxflat:customize-border-colors
  :after-set rebuild-all-frames)

(defcustom mxflat:border-color-highlighted
  (get-color "#000000")
  "  focused highlighted border color"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-colors-group)
  :type color
  :depends mxflat:customize-border-colors
  :after-set rebuild-all-frames)

(defcustom mxflat:border-color-inactive-highlighted
  (get-color "#000000")
  "unfocused highlighted border color"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-colors-group)
  :type color
  :depends mxflat:customize-border-colors
  :after-set rebuild-all-frames)

(defcustom mxflat:border-color-clicked
  (get-color "#000000")
  "  focused clicked border color"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-colors-group)
  :type color
  :depends mxflat:customize-border-colors
  :after-set rebuild-all-frames)

(defcustom mxflat:border-color-inactive-clicked
  (get-color "#000000")
  "unfocused clicked border color"
  :group (appearance mxflat:group mxflat:border-group
                     mxflat:border-colors-group)
  :type color
  :depends mxflat:customize-border-colors
  :after-set rebuild-all-frames)

;;  ----
;;; MISC
;;  ----

(define (get-last-workspace)
  (aref (nth 2 (get-x-property 'root '_NET_NUMBER_OF_DESKTOPS))
        0))

(define (get-first-workspace) 1)

(defmacro window-in-workspace-p (w space)
  `(memq ,space (window-get ,w 'workspaces)))

(define (window-maximized-p w)
  (window-get w 'unmaximized-geometry))

(define (window-maximized-horizontally-p w)
  (window-get w 'maximized-horizontally))

(define (window-maximized-vertically-p w)
  (window-get w 'maximized-vertically))

;; constant accuracy factor since we ???are not allowed to  use floats
;; in themes???
;; / seems to be gaoled
(defvar afactor 10000000000000)
(defvar afactor360 (* 360 afactor))

;;
;; misc:color manipulation
;;

;; returns a list containing hue saturation and value of color
(define (rgb2hsv color)
  (let* (
         ;; get rgb ints for the color
         (rgbchannels (color-rgb color))

         ;; get red, green and blue ints from rgbchannels
         (red   (nth 0 rgbchannels))
         (green (nth 1 rgbchannels))
         (blue  (nth 2 rgbchannels))

         ;; channnel with the biggest int value
         (value (apply max (list red green blue)))
         ;; delta
         (max-min (- value (apply min (list red green blue))))

         ;; saturation: 1 - min / max  =  (max - min) / max  =  delta / max
         ;; saturation * afactor - acutally the saturation
         ;; is a float between 0 and 1 but as it seems
         ;; we cannot use them in themes
         (saturation
          (if (= max-min 0)
              0
            (/ (* max-min afactor) value)))
         ;; value between 0 and 360 degree * afactor
         ;; 0 = red, 120 = green, 240 = blue
         (hue (if (= max-min 0)
                  0
                (/ (* 60 afactor (cond ((= value red)
                                        (- green blue))
                                       ((= value green)
                                        (+ (* 2 max-min) (- blue red)))
                                       (t ; (= value blue)
                                        (+ (* 4 max-min) (- red green)))))
                   max-min))))
    (list (if (< hue 0) (+ hue afactor360) hue) saturation value)))

;; translates a rgb-color from the ints of the list, which contains
;; hue, saturation and value
(define (hsv2rgb hsv)
  (let* ((hue (nth 0 hsv))
         (saturation (nth 1 hsv))
         (value (nth 2 hsv)))

    ;; value zero -> color black
    (cond ((= value 0)
           (get-color-rgb 0 0 0))
          ((= saturation 0)

           ;; in this case the color is grey and
           ;; thus its rgb value is:
           (get-color-rgb value value value))
          (t
           ;; else we need to do some calculations -
           ;; the hue and saturation are aleady
           ;; multiplied with afactor
           (let* (;; we need it often, so we define
                  ;; it once here
                  (afactor2 (* afactor afactor))

                  ;; kind of an 'compensation delta'
                  (f (if (or (= hue 0)
                             (= hue afactor360))
                         0
                       (/ (mod hue (* 60 afactor)) 60)))

                  ;; p is the lowest color int
                  (p (/ (*  value
                            (- afactor saturation))
                        afactor))

                  ;; tt is a medium color int
                  (q (/ (* value
                           (- afactor2 (* saturation f)))
                        afactor2))

                  ;; tt is a medium color int
                  (tt (/ (* value
                            (- afactor2
                               (* saturation (- afactor f))))
                         afactor2))

                  ;; area on the color circle (0 .. 5)
                  (circle-part
                   (if (>= hue afactor360)
                       0
                     (/ (/ hue afactor)
                        60))))
             ;; arrange the right vars for the current
             ;; color circle part
             (case circle-part
               ((0) (get-color-rgb value tt    p))
               ((1) (get-color-rgb q     value p))
               ((2) (get-color-rgb p     value tt))
               ((3) (get-color-rgb p     q     value))
               ((4) (get-color-rgb tt    p     value))
               ((5) (get-color-rgb value p     p))))))))

;; adjusts the hue, brightness and saturation of color
(define (adjust-color color)
  (let* (;; hue saturation value list
         (hsv (rgb2hsv color))
         ;; add used configured adjustment to
         ;; the hue of the color
         (a-hue (+ (nth 0 hsv)
                   (* mxflat:color-hue afactor)))
         ;; adjust saturation with
         ;; user configured factor
         (a-saturation (/ (* (nth 1 hsv)
                             mxflat:color-saturation)
                          100))
         ;; adjust value/brightness with
         ;; user configured factor
         (a-value (/ (* (nth 2 hsv)
                        mxflat:color-brightness)
                     100)))
    (hsv2rgb (list
              ;; adjust the hue if necessary
              ;; (ie it is >360 degree)
              (if (> a-hue afactor360)
                  (- a-hue afactor360)
                a-hue)
              a-saturation
              a-value))))

;;
;; misc:hook-ins
;;

;; when the window title or some other wm_hints attribute changes,
;; redraw the window
(call-after-property-changed (list 'WM_NAME)
                             (lambda (w)
                               (if (eq mxflat:title-x-justify 'real-center)
                                   (rebuild-frame w)
                                 (refresh-one-frame w))))

;; when we set a window sticky we have to redraw it, the sticky button
;; has changed
(call-after-state-changed '(sticky) reframe-window)

;; when the window is sent to another workspace redraw it to update
;; the buttons and titlebar if necessary
(add-hook 'remove-from-workspace-hook
          (lambda (w)
            (reframe-window w)))

;; not necessary anymore?
;;(add-hook 'after-resize-hook refresh-one-frame)

(add-hook 'focus-in-hook
          (lambda (w)
            ;; if 'custom borders for focused windows' -> redraw
            (if (or mxflat:custom-border-focused
                    (and mxflat:fake-shadow
                         mxflat:fake-shadow-focused-only))
                (reframe-window w))))

(add-hook 'focus-out-hook
          (lambda (w)
            ;; if 'custom borders for focused windows' -> redraw
            (if (or mxflat:custom-border-focused
                    (and mxflat:fake-shadow
                         mxflat:fake-shadow-focused-only))
                (reframe-window w))))

;;
;; misc:functions
;;

(define (get-border-offset)
  ;; we use the negative value to make the defcustom option work more
  ;; like one would expect, ie a positive value means the border will
  ;; have a bigger distance to the contents window
  (- mxflat:border-offset))

(define (get-button-size)
  (if (and mxflat:customize-button-size
           (< mxflat:button-size mxflat:title-height))
      mxflat:button-size
    mxflat:title-height))

;;  -----------
;;; FAKE SHADOW
;;  -----------

;; this list defines which window sides have shadows for which
;; direction
(defvar shadow-sides
  `((north . (bottom left right))
    (east . (top bottom left))
    (west . (top bottom right))
    (south . (top left right))
    (north-east . (bottom left))
    (north-west . (bottom right))
    (south-west . (top right))
    (south-east . (top left))))

;; selects the right shadow-side from the shadow-sides list
(define (get-shadow-side)
  (mapc (lambda (ss)
          (if (eq (car ss) mxflat:shadow-light-source)
              (setq shadow-side (cdr ss))))
        shadow-sides))

(define (get-shadow-color) mxflat:shadow-color)

(define (get-shadow-width w side)
  (if (member side shadow-side)
      (get-neg-border-width w side)
    mxflat:shadow-width))

(define (get-shadow-offset w side)
  ;; the shadow is only not visible on all window edges
  (if (not (window-maximized-p w))
      (if (or (and (eq mxflat:shadow-light-source 'north)
                   (not (eq side 'bottom)))
              (and (eq mxflat:shadow-light-source 'south)
                   (not (eq side 'top)))
              (and (eq mxflat:shadow-light-source 'east)
                   (not (eq side 'left)))
              (and (eq mxflat:shadow-light-source 'west)
                   (not (eq side 'right))))
          (/ mxflat:shadow-width mxflat:shadow-light-distance)
        (if (member side shadow-side)
            mxflat:shadow-width
          0))
    0))

;;  --------
;;; TITLEBAR
;;  --------

;;
;; titlebar:colors
;;

(define (get-text-colors)
  (if mxflat:customize-extended-text-colors
      `((inactive             . ,mxflat:text-color-unfocused)
        (focused              . ,mxflat:text-color-focused)
        (highlighted          . ,mxflat:text-color-highlighted)
        (inactive-highlighted . ,mxflat:text-color-inactive-highlighted)
        (clicked              . ,mxflat:text-color-clicked)
        (inactive-clicked     . ,mxflat:text-color-inactive-clicked))

    `((inactive             . ,mxflat:text-color-unfocused)
      (focused              . ,mxflat:text-color-focused)
      (highlighted          . ,mxflat:text-color-focused)
      (inactive-highlighted . ,mxflat:text-color-focused)
      (clicked              . ,mxflat:text-color-focused)
      (inactive-clicked     . ,mxflat:text-color-focused))))

(define (titlebar-gradient-colors-helper)
  (case mxflat:titlebar-gradient-color-mode
    ((custom)
     (if (> (length mxflat:titlebar-gradient-bgcolors) 0)
         mxflat:titlebar-gradient-bgcolors
       (list (get-color "#ffffffffffff"))))
    ((rainbow)
     (list (get-color "#ffff0000153b")
           (get-color "#ffff9b700000")
           (get-color "#fffff92d0000")
           (get-color "#3661ffff0000")
           (get-color "#00004c0dffff")
           (get-color "#b7ad0000ffff")))
    ((jamaica)
     (list (get-color "#7899f1340000")
           (get-color "#ffffffff0000")
           (get-color "#ffff37030000")))
    ((rgb)
     (list (get-color "#ffff00000000")
           (get-color "#0000ffff0000")
           (get-color "#00000000ffff")))))

(define (titlebar-gradient-colors)
  (if mxflat:titlebar-gradient-bgcolors-reverse
      (reverse (titlebar-gradient-colors-helper))
    (titlebar-gradient-colors-helper)))

;; changes the brightness and saturation for the titlebar gradient
;; color with index i
(define (adjust-titlebar-bgcolor i)
  ;; get rgb ints for the color with index i
  (adjust-color (nth i (titlebar-gradient-colors))))

(define (get-titlebar-bgcolors #!optional side)
  (cond ((and mxflat:title-gradient side)
         (let* ((index
                 (if (eq side 'right)
                     (1- (length (titlebar-gradient-colors)))
                   0)))
           `((inactive . ,(adjust-titlebar-bgcolor index))
             (focused  . ,(nth index (titlebar-gradient-colors))))))
        (t
         `((inactive    . ,mxflat:titlebar-bgcolor-unfocused)
           (focused     . ,mxflat:titlebar-bgcolor-focused)))))

;;
;; titlebar:bg rendering
;;

(define (num-titlebar-parts)
  (if (or (= (length (titlebar-gradient-colors)) 1)
          (eq mxflat:gradient-direction 'none))
      (length (titlebar-gradient-colors))
    (1- (length (titlebar-gradient-colors)))))

;; render frame-part background
;; this function takes the background img and uses many single 2-color
;; gradients to fill it
(define (render-bg img state)
  (let* (;; width and height of the titlebar part we have to fill
         (width (car (image-dimensions img)))
         (height (cdr (image-dimensions img)))

         ;; 2-color gradient width
         (part-width (/ width (num-titlebar-parts)))
         (rest-width (modulo width (num-titlebar-parts)))

         ;; the offset in the img that we've already reached
         ;; with our 2-color gradients
         (offset 0)

         ;; get the width of the 2-color gradient with index i
         (titlebar-parts-width-helper (lambda (i)
                                        (if (= i (1- (num-titlebar-parts)))
                                            (+ part-width rest-width)
                                          part-width)))

         ;; check the 2-color gradient width if it is > 0
         (titlebar-parts-width (lambda (i)
                                 (let ((val (titlebar-parts-width-helper i)))
                                   (if (< val 1)
                                       1
                                     val))))

         ;; 2-color gradient image
         (img2 nil))

    (case mxflat:gradient-direction
      ((horizontal none)
       (do ((i 0 (1+ i)))
           ((= i (num-titlebar-parts)))

         ;; create the 2-color gradient image and fill it with a
         ;; background color
         (setq img2 (make-sized-image (titlebar-parts-width i)
                                      height
                                      (if state
                                          (nth i (titlebar-gradient-colors))
                                        (adjust-titlebar-bgcolor i))))

         ;; fill the img2 with the 2-color gradient if necessary
         (if (not (eq mxflat:gradient-direction 'none))
             (apply draw-horizontal-gradient
                    img2
                    (if state
                        (list (nth i (titlebar-gradient-colors))
                              (if (or
                                   (= (length  (titlebar-gradient-colors)) 1)
                                   (eq mxflat:gradient-direction 'none))
                                  (nth i (titlebar-gradient-colors))
                                (nth (1+ i) (titlebar-gradient-colors))))
                      (list (adjust-titlebar-bgcolor i)
                            (if (or
                                 (= (length (titlebar-gradient-colors)) 1)
                                 (eq mxflat:gradient-direction 'none))
                                (adjust-titlebar-bgcolor i)
                              (adjust-titlebar-bgcolor (1+ i)))))))

         ;; paste the current 2-color gradient into the titlebar
         ;; background image
         (composite-images img img2 offset 0)
         (setq offset (+ offset (titlebar-parts-width i)))))

      ((vertical diagonal)
       (apply
        (if (eq mxflat:gradient-direction 'vertical)
            draw-vertical-gradient
          draw-diagonal-gradient)
        img
        (if state
            (list (nth 0 (titlebar-gradient-colors))
                  (if (= (length  (titlebar-gradient-colors)) 1)
                      (nth 0 (titlebar-gradient-colors))
                    (nth 1 (titlebar-gradient-colors))))
          (list (adjust-titlebar-bgcolor 0)
                (if (= (length (titlebar-gradient-colors)) 1)
                    (adjust-titlebar-bgcolor 0)
                  (adjust-titlebar-bgcolor 1)))))))
    img))

;;
;; titlebar:string
;;

(define (get-title-font) mxflat:title-font)

;; replace vars in the title string with their actual values and
;; return the  new title string
(define (get-title-string w)
  (if mxflat:custom-title
      (let* ((title mxflat:title-string)
             (update-title (lambda (match-str replace-str)
                             (while (string-match match-str title)
                               (setq title
                                     (concat
                                      (expand-last-match "\\1")
                                      (if (numberp replace-str)
                                          (number->string replace-str)
                                        replace-str)
                                      (expand-last-match "\\3")))))))
        ;; some built-in vars
        (update-title "(.*)(\\$name)(.*)"
                      (window-name w))
        (update-title "(.*)(\\$fullname)(.*)"
                      (window-full-name w))
        (update-title "(.*)(\\$width)(.*)"
                      (car (window-frame-dimensions w)))
        (update-title "(.*)(\\$height)(.*)"
                      (cdr (window-frame-dimensions w)))
        (update-title "(.*)(\\$width-content)(.*)"
                      (car (window-dimensions w)))
        (update-title "(.*)(\\$height-content)(.*)"
                      (cdr (window-dimensions w)))
        (update-title "(.*)(\\$x-pos)(.*)"
                      (car (window-position w)))
        (update-title "(.*)(\\$y-pos)(.*)"
                      (cdr (window-position w)))
        (update-title "(.*)(\\$id)(.*)"
                      (window-id w))
        (update-title "(.*)(\\$depth)(.*)"
                      (window-get w 'depth))
        (update-title "(.*)(\\$gid)(.*)"
                      (window-group-id w))
        (update-title "(.*)(\\$last-ws)(.*)"
                      (get-last-workspace))
        (update-title "(.*)(\\$current-ws)(.*)"
                      (let ((workspaces (window-get w 'workspaces)))
                        (if workspaces
                            (1+ (nth 0 workspaces))
                          "*")))
        ;; replace environment vars
        (while (string-match "(.*)\\$\\((.+?)\\)(.*)" title)
          (let ((part1 (expand-last-match "\\1"))
                (part2 (expand-last-match "\\3"))
                (envvar (getenv (expand-last-match "\\2"))))
            (if (not envvar)
                (setq envvar (concat "$?(" (expand-last-match "\\2") ")")))
            (setq title (concat part1 envvar part2))))
        ;; replace rep functions
        ;; XXX: no arguments to the functions possible yet, does
        ;; someone need this?
        (while (string-match "(.*)\\$\\[(.+?)\\](.*)" title)
          (let ((part1 (expand-last-match "\\1"))
                (part2 (expand-last-match "\\3"))
                (symb (intern (expand-last-match "\\2")))
                (func nil)
                (value (concat "$?[" (expand-last-match "\\2") "]")))
            (if (boundp symb)
                (setq func (symbol-value symb)))
            (if (functionp func)
                (setq value (funcall func)))
            (setq title (concat part1 value part2))))
        title)
    (window-name w)))

;;
;; titlebar:position
;;

;; calculate the leftmost x position of an object on the titlebar
;; this function is also used for button positions on the titlebar
(define (calculate-position pos)
  (if (and mxflat:customize-buttons
           (eq mxflat:button-position-model 'pixel))
      ;; if the button-position-model is pixel we dont
      ;; need to calculate anything but just return the
      ;; argument value
      pos
    ;; actually calculate the positon
    (* (1- pos)
       (+ (get-button-size)
          mxflat:button-distance))))

(define (get-title-height)
  (+ mxflat:title-height
     mxflat:border-width-titlebar-contents))

;; not really the neg value since we add the border offset
;; but thats the easiest place for doing so
(define (get-neg-title-height)
  (- (get-border-offset)
     (get-title-height)))

(define (get-titlebar-height w)
  (+ (get-title-height)
     (get-border-width w 'top)))

;; not really the neg value since we add the border offset
;; but thats the easiest place for doing so
(define (get-neg-titlebar-height w)
  (- (get-border-offset)
     (get-titlebar-height w)))

;; get the 1+ position of the leftmost or rightmost button
(define (get-last-button-position left-right)
  (let ((pos
         (if mxflat:customize-buttons
             (abs (apply
                   (if (eq left-right 'left)
                       min
                     max)
                   (list mxflat:menu-button-position
                         mxflat:shade-button-position
                         mxflat:sticky-button-position
                         mxflat:previous-button-position
                         mxflat:next-button-position
                         mxflat:configure-button-position
                         mxflat:info-button-position
                         mxflat:iconify-button-position
                         mxflat:maximize-button-position
                         mxflat:close-button-position
                         mxflat:always-on-top-button-position
                         mxflat:lock-button-position)))
           (if (eq left-right 'left)
               3
             2))))
    (if (or (eq mxflat:button-position-model 'title-height)
            (not mxflat:customize-buttons))
        (calculate-position (1+ pos))
      (if (eq mxflat:button-position-model 'pixel)
          (+ pos
             mxflat:title-height)1))))

;; by default the title is vertically centered and pixelwise
;; adjusted through mxflat:title-y-adjustment
(define (get-title-y-alignment)
  (if mxflat:customize-title-position
      (case mxflat:title-y-justify
        ((top)
         ;; sanity check on the value
         ;; else the title string would not be visible
         (if (> mxflat:title-y-adjustment 0)
             mxflat:title-y-adjustment
           0))

        ;; we have to calculate it manually to be able to take
        ;; the y align adjustment into account
        ((center)
         (+ mxflat:title-y-adjustment
            (/ (- mxflat:title-height
                  (font-height mxflat:title-font))
               2)))

        ;; bottom
        (t
         ;; sanity check on the value
         ;; else the title string would not be visible
         (if (< mxflat:title-y-adjustment 0)
             mxflat:title-y-adjustment
           -1)))
    'center))

(define (get-title-x-alignment w)
  (if mxflat:customize-title-position
      (case mxflat:title-x-justify
        ((left)
         ;; sanity check on the value
         ;; else the title string would not be visible
         (if (< mxflat:title-x-adjustment 0)
             0
           mxflat:title-x-adjustment))
        ((center)
         'center)
        ((right)
         ;; sanity check on the value
         ;; else the title string would not be visible
         (if (> mxflat:title-x-adjustment -1)
             -1
           mxflat:title-x-adjustment))
        ;; 'real-center' means to take the whole titlebar into account
        ;; while centering the title string
        ((real-center)
         (let ((real-title-offset
                (/ (- (car (window-dimensions w))
                      (text-width (get-title-string w) (get-title-font)))
                   2))
               (title-offset
                (- (get-last-button-position 'left)
                   mxflat:title-x-adjustment)))
           (if (> title-offset real-title-offset)
               0
             (- real-title-offset title-offset)))))
    'center))

(define (left-buttons-end-offset)
  (get-last-button-position 'left))

(define (right-buttons-end-offset)
  (get-last-button-position 'right))

;;
;; titlebar:creation
;;

(define (titlebar-center backgroun backgroundd)
  `(((class        . title)
     (,backgroun   . ,backgroundd)
     (render-scale . ,mxflat:gradient-scale)
     (foreground   . ,get-text-colors)
     (font         . ,get-title-font)
     (text         . ,get-title-string)
     (x-justify    . ,get-title-x-alignment)
     (y-justify    . ,get-title-y-alignment)
     (left-edge    . ,(lambda ()
                        (+ (left-buttons-end-offset)
                           (get-border-offset))))
     (right-edge   . ,(lambda ()
                        (+ (right-buttons-end-offset)
                           (get-border-offset))))
     (top-edge     . ,get-neg-title-height)
     (height       . ,(lambda () mxflat:title-height)))))

(define (titlebar-left-right backgroun backgroundd edg1 edgee1 edg2 edgee2)
  ;; left part of the titlebar
  `(((class        . title)
     (,backgroun   . ,backgroundd)
     (render-scale . ,mxflat:gradient-scale)
     (,edg1        . ,edgee1)
     (,edg2        . ,edgee2)
     (top-edge     . ,get-neg-title-height)
     (height       . ,(lambda () mxflat:title-height)))))

(define (titlebar)
  (append
   (if (and (or (eq mxflat:gradient-direction 'vertical)
                (eq mxflat:gradient-direction 'diagonal))
            mxflat:title-gradient)
       (append
        (titlebar-left-right
         'renderer render-bg
         'left-edge get-border-offset
         'width left-buttons-end-offset)
        (titlebar-left-right
         'renderer render-bg
         'width right-buttons-end-offset
         'right-edge get-border-offset))
     (append
      (titlebar-left-right
       'background (lambda () (get-titlebar-bgcolors 'left))
       'left-edge get-border-offset
       'width left-buttons-end-offset)
      (titlebar-left-right
       'background (lambda () (get-titlebar-bgcolors 'right))
       'width right-buttons-end-offset
       'right-edge get-border-offset)))
   (if mxflat:title-gradient
       (titlebar-center 'renderer render-bg)
     (titlebar-center 'background get-titlebar-bgcolors))))

;;  -------
;;; BORDERS
;;  -------

;;
;; borders:colors
;;

(define (get-border-color)
  (if mxflat:customize-border-colors
      `((inactive             . ,mxflat:border-color-unfocused)
        (focused              . ,mxflat:border-color-focused)
        (highlighted          . ,mxflat:border-color-highlighted)
        (inactive-highlighted . ,mxflat:border-color-inactive-highlighted)
        (clicked              . ,mxflat:border-color-clicked)
        (inactive-clicked     . ,mxflat:border-color-inactive-clicked))
    `((inactive . ,mxflat:text-color-unfocused)
      (focused  . ,mxflat:text-color-focused))))

(define (get-border-color-titlebar-contents)
  (if mxflat:customize-border-colors
      `((inactive . ,mxflat:border-color-unfocused)
        (focused  . ,mxflat:border-color-focused))
    `((inactive . ,mxflat:text-color-unfocused)
      (focused  . ,mxflat:text-color-focused))))

;;
;; borders:width
;;

(define (get-border-width w #!optional side)
  (cond ((or (and (window-maximized-vertically-p w)
                  (or (eq side 'top)
                      (eq side 'bottom)))
             (and (window-maximized-horizontally-p w)
                  (or (eq side 'left)
                      (eq side 'right))))
         mxflat:border-width-maximized)
        ((and mxflat:custom-border-focused
              (eq w (input-focus)))
         mxflat:border-width-focused)
        (t
         mxflat:border-width-unmaximized)))

(define (get-neg-border-width w #!optional side)
  (- (get-border-offset)
     (get-border-width w side)))

;;
;; borders:creation
;;

(define (create-border-frame-parts color offset widthh
                                   #!optional mode border-list)
  (let* ((get-pos (lambda (w side)
                    (- (offset w side)
                       (widthh w side))))
         ;; bottom edge of the frame
         (bottom-edgee1 'bottom-edge)
         (bottom-edgee2 'bottom-edge)
         (bottom-edg1 (lambda (w)
                        (offset w 'bottom)))
         (bottom-edg2 (lambda (w)
                        (get-pos w 'bottom)))
         (top-edg1 (lambda (w)
                     (- (offset w 'top)
                        (get-title-height))))
         (top-edg2 (lambda (w)
                     (- (offset w 'top)
                        (+ (get-title-height)
                           (widthh w 'top)))))
         (left-edg (lambda (w)
                     (get-pos w 'left)))
         (right-edg (lambda (w)
                      (get-pos w 'right)))
         (right-borderr (if border-list
                            'shadow
                          'right-border))
         (left-borderr (if border-list
                           'shadow
                         'left-border))
         (top-borderr (if border-list
                          'shadow
                        'top-border))
         (bottom-borderr (if border-list
                             'shadow
                           'bottom-border)))
    (case mode
      ('transient
       (setq top-edg1 (lambda (w) (offset w 'top)))
       (setq top-edg2 (lambda (w) (get-pos w 'top))))
      ('shaded
       (setq bottom-edgee2 'top-edge)
       (setq bottom-edg2 (lambda (w)
                           (- (offset w 'bottom)
                              mxflat:border-width-titlebar-contents)))
       ;; when the window is shaded we rather define the borders by
       ;; top-edge and height nstead of using top- and bottom-edge
       (setq bottom-edgee1 'height)
       (setq bottom-edg1 (lambda (w)
                           (get-titlebar-height w)))))
    (append
     ;; draw the right border
     (if (or (not border-list)
             (member 'right border-list))
         `(((class          . ,right-borderr)
            (background     . ,color)
            (width          . ,(lambda (w) (widthh w 'right)))
            (right-edge     . ,right-edg)
            (top-edge       . ,top-edg1)
            (,bottom-edgee1 . ,bottom-edg1))))

     ;; draw the left border
     (if (or (not border-list)
             (member 'left border-list))
         `(((class          . ,left-borderr)
            (background     . ,color)
            (width          . ,(lambda (w) (widthh w 'left)))
            (left-edge      . ,left-edg)
            (top-edge       . ,top-edg1)
            (,bottom-edgee1 . ,bottom-edg1))))

     ;; draw the top border of the title frame
     (if (or (not border-list)
             (member 'top border-list))
         `(((class      . ,top-borderr)
            (background . ,color)
            (height     . ,(lambda (w) (widthh w 'top)))
            (left-edge  . ,left-edg)
            (right-edge . ,right-edg)
            (top-edge   . ,top-edg2))))

     ;; draw the bottom border
     (if (or (not border-list)
             (member 'bottom border-list))
         `(((class          . ,bottom-borderr)
            (background     . ,color)
            (height         . ,(lambda (w) (widthh w 'bottom)))
            (left-edge      . ,left-edg)
            (right-edge     . ,right-edg)
            (,bottom-edgee2 . ,bottom-edg2)))))))

;; this function appends the window border (frame) and its shadow
(define (create-borders w #!optional mode)
  (when (= (length shadow-side) 0)
    (get-shadow-side))
  (append
   ;; create normal window border
   (create-border-frame-parts get-border-color
                              get-border-offset
                              get-border-width
                              mode)
   ;; create fake shadow if necessary
   (when (and mxflat:fake-shadow
              (or (not mxflat:fake-shadow-focused-only)
                  (eq w (input-focus))))
     (create-border-frame-parts get-shadow-color
                                get-shadow-width
                                get-shadow-offset
                                mode
                                shadow-side))))

;; create the (small) line between titlebar and contents window
(define (create-titlebar-contents-border)
  `(((class      . middle-border)
     (background . ,get-border-color-titlebar-contents)
     (height     . ,(lambda ()
                      mxflat:border-width-titlebar-contents))
     (top-edge   . ,(lambda ()
                      (- (get-border-offset)
                         mxflat:border-width-titlebar-contents)))
     (left-edge  . ,get-border-offset)
     (right-edge . ,get-border-offset))))

;; return a top border (for shaded-transient)
(define (create-top-border)
  `(((class      . top-border)
     (background . ,get-border-color)
     (height     . ,(lambda (w) (get-border-width w 'top)))
     (top-edge   . ,(lambda (w) (get-neg-border-width w 'top)))
     (left-edge  . 0)
     (right-edge . 0))))

;;  -------
;;; BUTTONS
;;  -------


;; create, scale and cache an image
(define (get-image img)
  (or (table-ref icon-cache img)
      (let ((image
             (if mxflat:scale-buttons
                 (scale-image (make-image img) mxflat:button-scale
                              mxflat:button-scale)
               (make-image img))))
        (table-set icon-cache img image)
        image)))

;;
;; buttons:colors
;;

(define (get-button-bgcolor #!optional edge)
  (cond (mxflat:title-gradient
         (let* ((index
                 (if (eq edge 'right-edge)
                     (1- (length (titlebar-gradient-colors)))
                   0)))
           `((inactive             . ,(adjust-titlebar-bgcolor index))
             (focused              . ,(nth index (titlebar-gradient-colors)))
             (highlighted          . ,(adjust-titlebar-bgcolor index))
             (inactive-highlighted . ,(nth index (titlebar-gradient-colors)))
             (clicked              . ,(adjust-titlebar-bgcolor index))
             (inactive-clicked     . ,(nth index
                                           (titlebar-gradient-colors))))))
        (mxflat:custom-button-colors
         `((inactive             . ,mxflat:button-bgcolor-unfocused)
           (focused              . ,mxflat:button-bgcolor-focused)
           (highlighted          . ,mxflat:button-bgcolor-highlighted)
           (inactive-highlighted . ,mxflat:button-bgcolor-inactive-highlighted)
           (clicked              . ,mxflat:button-bgcolor-clicked)
           (inactive-clicked     . ,mxflat:button-bgcolor-inactive-clicked)))
        (t
         `((inactive             . ,mxflat:titlebar-bgcolor-unfocused)
           (focused              . ,mxflat:titlebar-bgcolor-focused)
           (highlighted          . ,mxflat:titlebar-bgcolor-unfocused)
           (inactive-highlighted . ,mxflat:titlebar-bgcolor-focused)
           (clicked              . ,mxflat:text-color-unfocused)
           (inactive-clicked     . ,mxflat:text-color-unfocused)))))


;;
;; buttons:icons
;;

;; return a list of 4 different image objects as needed for the button
;; fg images/icons
(define (get-images img)
  (let ((image (get-image (concat "button-icons/"
                                  (symbol-name mxflat:button-icon-set)
                                  "/"
                                  img))))
    (list image
          (copy-image image)
          (copy-image image)
          (copy-image image))))

(defvar get-menu-button1 (get-images "button-menu.png"))
(define (get-menu-button w)
  (if mxflat:menu-button-icon
      (or (table-ref icon-cache w)
          (or (let ((icon (window-icon-image w)))
                (when icon
                  (let ((scaled
                         (scale-image icon mxflat:menu-button-scale
                                      mxflat:menu-button-scale)))
                    (table-set icon-cache w scaled)
                    scaled)))
              get-menu-button1))
    get-menu-button1))

(defvar get-shade-button1 (get-images "button-shade1.png"))
(defvar get-shade-button2 (get-images "button-shade2.png"))
(define (get-shade-button w)
  (if (window-get w 'shaded)
      get-shade-button2
    get-shade-button1))

(defvar get-sticky-button1 (get-images "button-sticky1.png"))
(defvar get-sticky-button2 (get-images "button-sticky2.png"))
(define (get-sticky-button w)
  (if (window-get w 'sticky)
      get-sticky-button2
    get-sticky-button1))

(defvar get-iconify-button (get-images "button-minimize.png"))

(defvar get-maximize-button1 (get-images "button-maximize1.png"))
(defvar get-maximize-button2 (get-images "button-maximize2.png"))
(define (get-maximize-button w)
  (if (window-maximized-p w)
      get-maximize-button2
    get-maximize-button1))

(defvar get-close-button (get-images "button-close.png"))

(defvar get-previous-button1 (get-images "button-previous1.png"))
(defvar get-previous-button2 (get-images "button-previous2.png"))
(define (get-previous-button w)
  (if (or (window-in-workspace-p w (- (get-first-workspace) 1))
          (window-get w 'sticky))
      get-previous-button2
    get-previous-button1))

(defvar get-next-button1 (get-images "button-next1.png"))
(defvar get-next-button2 (get-images "button-next2.png"))
(define (get-next-button w)
  (if (or (window-in-workspace-p w (- (get-last-workspace) 1))
          (window-get w 'sticky))
      get-next-button2
    get-next-button1))

(defvar get-configure-button (get-images "button-configure.png"))

(defvar get-info-button (get-images "button-info.png"))

(defvar get-always-on-top-button1 (get-images "button-a1.png"))
(defvar get-always-on-top-button2 (get-images "button-a2.png"))
(define (get-always-on-top-button w)
  (if (> (window-get w 'depth) 0)
      get-always-on-top-button2
    get-always-on-top-button1))

(defvar get-lock-button1 (get-images "button-lock1.png"))
(defvar get-lock-button2 (get-images "button-lock2.png"))
(define (get-lock-button w)
  (if (window-get w 'fixed-position)
      get-lock-button2
    get-lock-button1))

;;
;; buttons:fg recolor
;;

;; this function is stolen from the crux sawfish theme (the theme is
;; included in the original sawfish package)
(setq recolor-all
      (lambda ()
        (let* ((recolorer (lambda (color1 color2)
                            (make-image-recolorer
                             (if mxflat:custom-button-colors
                                 color1
                               color2)
                             #:zero-channel blue-channel
                             #:index-channel red-channel)))

               ;; unfocused
               (recolorer1 (recolorer
                            mxflat:button-fgcolor-inactive
                            mxflat:text-color-unfocused))
               ;; focused
               (recolorer2 (recolorer
                            mxflat:button-fgcolor-focused
                            mxflat:text-color-focused))
               ;; highlighted
               (recolorer3 (recolorer
                            mxflat:button-fgcolor-highlighted
                            mxflat:text-color-unfocused))
               ;; clicked
               (recolorer4 (recolorer
                            mxflat:button-fgcolor-clicked
                            mxflat:text-color-focused)))
          (mapc (lambda (x)
                  (apply recolorer1 x)
                  (apply recolorer2 (cdr x))
                  (if (> (length x) 2)
                      (apply recolorer3 (nthcdr 2 x)))
                  (if (> (length x) 3)
                      (apply recolorer4 (nthcdr 3 x))))

                ;; list of all button fg icon lists that we have to recolor
                (list get-menu-button1 get-shade-button1
                      get-shade-button2 get-sticky-button1
                      get-sticky-button2 get-iconify-button
                      get-maximize-button1 get-maximize-button2
                      get-close-button get-previous-button1
                      get-previous-button2 get-next-button1
                      get-next-button2 get-configure-button
                      get-info-button get-always-on-top-button1
                      get-always-on-top-button2 get-lock-button1
                      get-lock-button2)))))

;;
;; buttons:define classes
;;

;; missing button frame part classes
;; all other classes are defined by sawfish by default - it are always
;; 2 classes, one with the default key binding and one with the custom
;; (user defined) one

(define-frame-class 'configure-button-custom
  '((keymap . mxflat:configure-button-keymap)))

(define-frame-class 'configure-button
  '((keymap . configure-button-keymap)))

(define-frame-class 'info-button-custom
  '((keymap . mxflat:info-button-keymap)))

(define-frame-class 'info-button
  '((keymap . info-button-keymap)))

(define-frame-class 'next-button-custom
  '((keymap . mxflat:next-button-keymap)))

(define-frame-class 'next-button
  '((keymap . next-button-keymap)))

(define-frame-class 'previous-button-custom
  '((keymap . mxflat:previous-button-keymap)))

(define-frame-class 'previous-button
  '((keymap . previous-button-keymap)))

(define-frame-class 'sticky-button-custom
  '((keymap . mxflat:sticky-button-keymap)))

(define-frame-class 'sticky-button
  '((keymap . sticky-button-keymap)))

(define-frame-class 'always-on-top-button-custom
  '((keymap . mxflat:always-on-top-button-keymap)))

(define-frame-class 'always-on-top-button
  '((keymap . always-on-top-button-keymap)))

(define-frame-class 'lock-button-custom
  '((keymap . mxflat:lock-button-keymap)))

(define-frame-class 'lock-button
  '((keymap . lock-button-keymap)))

;;
;; buttons:creation
;;

;; this function actually creates a button frame part and returns it
(define (create-button-frame-part classs fg edge x-pos y-pos backgroun
                                  backgroundd)
  ;; button frame part definition
  `((class        . ,classs)
    (,backgroun   . ,backgroundd)
    (render-scale . ,mxflat:gradient-scale)
    (foreground   . ,fg)
    (x-justify    . center)
    (y-justify    . center)
    (,edge        . ,x-pos)
    (top-edge     . ,y-pos)
    (height       . ,get-button-size)
    (width        . ,get-button-size)))

;; gather all necessary arguments and then call create-button-frame-part
;; to actually create the button
(define (create-button button-type #!optional x-pos-default)
  (let ((x-pos-raw
         ;; raw position, creatue-button-frame-part does some more
         ;; calculations on it
         (if mxflat:customize-buttons
             (check-symbol "mxflat:" button-type "-button-position")
           (if x-pos-default x-pos-default 0))))

    ;; position = 0 means the button is not displayed so we can save
    ;; some time be not doing all these evaluations
    (if (not (= x-pos-raw 0))
        (let* (;; if the position parameter (x-pos-raw) for the
               ;; current button is positive it starts on the right
               ;; side
               (edge (if (> x-pos-raw 0) 'right-edge 'left-edge))

               ;; default button background mode (ie static color)
               (backgroun 'background)
               (backgroundd (get-button-bgcolor edge))

               ;; button x offset from the left/right titlebar edge
               (x-pos (lambda ()
                        (+ (- (calculate-position (abs x-pos-raw))
                              mxflat:border-offset)
                           (if (> x-pos-raw 0)
                               mxflat:button-right-offset
                             mxflat:button-left-offset))))

               ;; different top positions for different
               ;; mxflat:button-y-alignment but only in case
               ;; mxflat:customize-button-size is true because in the
               ;; other case the buttons height and width is equal to
               ;; the titlebar height
               (y-pos (lambda ()
                        (if mxflat:customize-button-size
                            (case mxflat:button-y-alignment
                              ((top)
                               (get-neg-title-height))
                              ((bottom)
                               (+ (get-neg-title-height)
                                  (- mxflat:title-height
                                     (get-button-size))))
                              (t  ; this is the center case
                               (+ (get-neg-title-height)
                                  (/ (- mxflat:title-height
                                        (get-button-size))
                                     2))))
                          ;; in case the size is the default we start
                          ;; on top of the titlebar
                          (get-neg-title-height)))))

          ;; in case of a gradient titlebar bg change the button bg to
          ;; rendering
          (cond ((and mxflat:title-gradient
                      (or (eq mxflat:gradient-direction 'vertical)
                          (eq mxflat:gradient-direction 'diagonal)))
                 (setq backgroun 'renderer)
                 (setq backgroundd render-bg)))

          (create-button-frame-part
           ;; decide which frame part (and indirectly which keymap) to
           ;; use
           (if (check-symbol "mxflat:customize-" button-type "-keymap")
               (get-symbol button-type "-button-custom")
             (get-symbol button-type "-button"))
           ;; button fg images/icons
           (check-symbol "get-" button-type "-button")
           edge
           x-pos
           y-pos
           backgroun
           backgroundd)))))

;; create a list of button frame parts and return it
(define (create-buttons)
  ;; create a list of all custom buttons if the button
  ;; position model is not none (none means no buttons)
  (if (not (eq mxflat:button-position-model 'none))
      (list
       (create-button "menu" -1)
       (create-button "shade" -2)
       (create-button "iconify" +3)
       (create-button "maximize" +2)
       (create-button "close" +1)
       (create-button "sticky")
       (create-button "next")
       (create-button "previous")
       (create-button "configure")
       (create-button "info")
       (create-button "always-on-top")
       (create-button "lock"))))

;;  -------
;;; CORNERS
;;  -------

;;
;; corners:size
;;

(define (get-corner-width-adjustment w #!optional side)
  (if (or (and (window-maximized-vertically-p w)
               (or (eq side 'top)
                   (eq side 'bottom)))
          (and (window-maximized-horizontally-p w)
               (or (eq side 'left)
                   (eq side 'right))))
      mxflat:corner-width-adjustment-maximized
    mxflat:corner-width-adjustment))

(define (get-corner-width w #!optional frame-type side)
  (if (and (eq frame-type 'shaded)
           (> (get-corner-width-adjustment w side)
              (/ (get-title-height) 2))
           (eq mxflat:corner-direction 'inside))
      (/ (1+ (get-title-height)) 2)
    (+ (get-corner-width-adjustment w side)
       (get-border-width w side))))

(define (get-corner-length w #!optional frame-type)
  (if (= mxflat:corner-length-adjustment -1)
      0
    (+ (+ (if (and (eq frame-type 'shaded)
                   (> mxflat:corner-length-adjustment
                      (/ (get-title-height) 2)))
              (/ (1+ (get-title-height)) 2)
            mxflat:corner-length-adjustment)
          (get-border-width w 'top))
       (if (eq mxflat:corner-direction 'inside)
           0
         (get-corner-width-adjustment w)))))

;;
;; corners:position
;;

;; get top or bottom position
(define (get-corner-1-pos w side part #!optional frame-type)
  (- (if (eq side 'bottom)
         (if (eq frame-type 'shaded)
             (+ (if (eq part 'b)
                    (+ (- (get-corner-length w frame-type))
                       (get-corner-width w frame-type side))
                  0)
                (- (get-border-offset)
                   mxflat:border-width-titlebar-contents))
           (get-neg-border-width w side))
       (if (eq frame-type 'transient)
           (get-neg-border-width w 'top)
         (get-neg-titlebar-height w)))
     (if (and (eq frame-type 'shaded)
              (eq side 'bottom))
         (if (eq mxflat:corner-direction 'inside)
             (get-corner-width-adjustment w side)
           0)
       (if (eq mxflat:corner-direction 'inside)
           0
         (get-corner-width-adjustment w side)))))

;; get left or right position
(define (get-corner-2-pos w side)
  (- (get-neg-border-width w side)
     (if (eq mxflat:corner-direction 'inside)
         0
       (get-corner-width-adjustment w side))))

;;
;; corners:creation
;;

;; create the two corner lists a real corner consists of
(define (create-corner classs edgee1 side1 edgee2 side2 frame-type)
  `(((class      . ,classs)
     (background . ,get-border-color)
     (width      . ,get-corner-length)
     (height     . ,(lambda (w) (get-corner-width w frame-type side1)))
     (,edgee1    . ,(lambda (w) (get-corner-1-pos w side1 'a frame-type)))
     (,edgee2    . ,(lambda (w) (get-corner-2-pos w side2))))
    ((class      . ,classs)
     (background . ,get-border-color)
     (width      . ,(lambda (w) (get-corner-width w frame-type side2)))
     (height     . ,(lambda (w) (get-corner-length w frame-type)))
     (,edgee1    . ,(lambda (w) (get-corner-1-pos w side1 'b frame-type)))
     (,edgee2    . ,(lambda (w) (get-corner-2-pos w side2))))))

;; this function actually creates the 4 window corners and returns
;; them
(define (create-corners #!optional frame-type)
  (if mxflat:show-corners
      (let ((edgee1 'top-edge)
            (edgee2 'bottom-edge))
        (if (eq frame-type 'shaded)
            (setq edgee2 'top-edge))
        (append
         (create-corner 'top-left-corner edgee1 'top 'left-edge
                        'left frame-type)
         (create-corner 'top-right-corner edgee1 'top 'right-edge
                        'right frame-type)
         (create-corner 'bottom-left-corner edgee2 'bottom 'left-edge
                        'left frame-type)
         (create-corner 'bottom-right-corner edgee2 'bottom 'right-edge
                        'right frame-type)))))

;;  --------------
;;; REGISTER THEME
;;  --------------
;;
;; that is from the manual
;; (http://sawmill.sourceforge.net/prog-manual.html)
;;
;; Frame Types
;;
;; * default
;; The normal frame type. Includes all decorations, both borders and
;; the title bar.
;;
;; * transient
;; The frame for a transient window. This usually does not include a
;; title bar, but does have all four borders.
;;
;; * shaped
;; Shaped windows are normally decorated with only a title-bar, since
;; their boundary is not rectangular it makes no sense to surround
;; them with a rectangular border.
;;
;; * shaped-transient
;; A combination of the shaped and transient types, normally just a
;; very small title border with no text.
;;
;; * shaded
;; A shaded window (normally just the title bar).
;;
;; * shaded-transient
;; A shaded transient window.
;;
;; * unframed
;; No frame at all, just the client window itself. The predefined
;; nil-frame variable contains a null frame that may be used for this
;; frame type.
;;
;; "Normal" 'default
;; "Title-only" 'shaped
;; "Border-only" 'transient
;; "Top-border" 'shaped-transient
;; "None" 'unframed
;;

;; return the window frame and decorations
(define (get-frame w frame-type)
  (let* (;; window with all decorations
         (defaul (lambda ()
                   (append (titlebar)
                           (create-titlebar-contents-border)
                           (create-borders w)
                           (create-buttons)
                           (create-corners))))

         ;; window with contents frame only
         (transien (lambda ()
                     (append (create-borders w 'transient)
                             (create-corners 'transient))))

         (titleba (lambda (#!optional mode)
                    (if mode
                        (titlebar)
                      (append (titlebar) (create-buttons)))))

         ;; window that is shaded
         (shade (lambda (#!optional titlebar-mode)
                  (if mxflat:keep-borders-when-shaded
                      (append (titleba titlebar-mode)
                              (create-titlebar-contents-border)
                              (create-borders w)
                              (create-corners))
                    (append (titleba titlebar-mode)
                            (create-borders w 'shaded)
                            (create-corners 'shaded)))))

         (check (lambda (mode)
                  (cond
                   ((eq mxflat:decoration-mode 'none)
                    (transien))
                   ((eq mxflat:decoration-mode 'all)
                    (defaul))
                   (t
                    (mode))))))

    (case frame-type
      ((default)
       (check defaul))
      ((transient)
       (check transien))
      ((shaped shaded)
       (if (eq mxflat:decoration-mode 'none)
           (shade 'no-buttons)
         (shade)))
      ((shaded-transient)
       (if (eq mxflat:decoration-mode 'all)
           (shade)
         (shade 'no-buttons)))
      ((shaped-transient)
       (create-top-border)))))

;; setup initial colors
(recolor-all)
(after-set-title-update-interval)
;; register theme with sawfish
(add-frame-style 'mxflat get-frame)

;;; END
