;; sawmill-gaol.jl -- protected environment for themes
;; $Id$

(require 'gaol)
(provide 'sawmill-gaol)

(defvar sawmill-safe-functions
  '(get-color-rgb get-color color-name color-rgb colorp
    get-cursor cursorp get-font font-name fontp text-width font-height
    screen-width screen-height get-x-property get-x-text-property
    list-x-properties x-atom x-atom-name make-image copy-image
    flip-image-horizontally flip-image-vertically flip-image-diagonally
    image-get image-put imagep image-dimensions image-border
    set-image-border image-shape-color set-image-shape-color
    image-modifier set-image-modifier make-sized-image bevel-image
    clear-image tile-image make-keymap bind-keys unbind-keys keymapp
    eventp window-get window-name window-full-name window-icon-name
    window-mapped-p window-frame set-window-frame rebuild-frame
    window-position window-dimensions window-frame-dimensions windowp
    managed-windows get-window-by-id stacking-order window-visibility
    window-transient-p window-shaped-p window-visible-p window-framed-p
    window-id window-group-id window-size-hints call-window-hook

    add-frame-style check-frame-availability set-window-frame-style
    set-frame-for-window reframe-one-window rebuild-frames-with-style
    reframe-windows-with-style reframe-all-windows window-type
    def-frame-class define-frame-class after-setting-frame-option

    defcustom defgroup custom-declare-variable custom-declare-group
    custom-quote-keys custom-set-property custom-set-group-property

    window-actual-group-id windows-by-group windows-in-group
    map-window-group window-group-ids

    get-window-by-name save-stacking-order uniquify-list
    call-after-property-changed

    draw-vertical-gradient draw-horizontal-gradient draw-diagonal-gradient))

(defvar sawmill-safe-specials
  '(default-foreground display-name canonical-display-name
    default-font default-frame nil-frame frame-part-classes
    decorate-transients))

(defvar sawmill-safe-features '(gtkrc gradient make-theme))

(unless batch-mode
  (mapc gaol-add-function sawmill-safe-functions)
  (mapc gaol-add-special sawmill-safe-specials)
  (mapc gaol-add-feature sawmill-safe-features))
