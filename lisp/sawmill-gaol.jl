;; sawmill-gaol.jl -- protected environment for themes
;; $Id$

(require 'gaol)
(provide 'sawmill-gaol)

(defvar sawmill-safe-functions
  '(get-color-rgb get-color color-name color-rgb colorp get-cursor
    cursorp get-font font-name fontp text-width font-height
    screen-width screen-height get-x-property get-x-text-property
    list-x-properties x-atom x-atom-name make-image copy-image
    flip-image-horizontally flip-image-vertically flip-image-diagonally
    frame-part-get frame-part-put frame-part-window frame-part-x-window
    frame-part-position frame-part-dimensions frame-part-state map-frame-parts
    refresh-frame-part refresh-window rebuild-frame-part image-get
    image-put imagep image-dimensions image-border set-image-border
    image-shape-color set-image-shape-color image-modifier
    set-image-modifier make-sized-image bevel-image clear-image
    tile-image make-keymap bind-keys unbind-keys keymapp eventp
    window-get window-name window-full-name window-icon-name
    window-mapped-p window-frame set-window-frame rebuild-frame
    window-position window-dimensions window-frame-dimensions windowp
    managed-windows get-window-by-id stacking-order window-visibility
    window-transient-p window-shaped-p window-visible-p window-framed-p
    window-id window-group-id window-size-hints call-window-hook
    window-maximized-p window-maximized-horizontally-p
    window-maximized-vertically-p input-focus

    add-frame-style check-frame-availability set-window-frame-style
    set-frame-for-window reframe-one-window rebuild-frames-with-style
    reframe-windows-with-style reframe-all-windows window-type
    def-frame-class define-frame-class after-setting-frame-option
    mark-frame-style-editable

    defcustom defgroup custom-declare-variable custom-declare-group
    custom-quote-keys custom-set-property custom-set-group-property

    window-actual-group-id windows-by-group windows-in-group
    map-window-group window-group-ids

    get-window-by-name save-stacking-order uniquify-list
    call-after-property-changed call-after-state-changed))

(defvar sawmill-safe-specials
  '(default-foreground display-name canonical-display-name
    default-font default-frame nil-frame frame-part-classes
    decorate-transients))

(defvar sawmill-safe-features '(gtkrc gradient make-theme x))

(defun gaol:require (feature)
  (unless (memq feature sawmill-safe-features)
    (error "Gaolled code trying to require %s" feature))
  (require feature)
  (gaol-rebuild-environment))

(when (>= rep-interface-id 9)
  ;; compatibility kludge..
  (defun gaol-add-function (sym)
    (gaol-replace-function sym (symbol-value sym))))

(unless batch-mode
  ;; for backwards compatibility, / is integer division in themes, use
  ;; `divide' for real division
  (cond ((>= rep-interface-id 9)
	 (gaol-replace-function 'divide /)
	 (gaol-replace-function '/ quotient)
	 (gaol-replace-function 'require gaol:require))
	(t
	 (setq gaol-safe-functions (delq '/ gaol-safe-functions))
	 (gaol-replace-function 'divide '/)
	 (gaol-replace-function '/ 'quotient)
	 (gaol-replace-function 'require 'gaol:require)))

  (mapc gaol-add-function sawmill-safe-functions)
  (mapc gaol-add-special sawmill-safe-specials)

  (eval-after-load
   "gradient" '(mapc gaol-add-function '(draw-vertical-gradient
					 draw-horizontal-gradient
					 draw-diagonal-gradient)))

  (eval-after-load
   "x" '(mapc gaol-add-function '(x-create-gc x-change-gc x-destroy-gc x-gc-p
				  x-change-window-attributes x-window-p
				  x-window-id x-window-back-buffer
				  x-window-swap-buffers x-clear-window
				  x-draw-string x-draw-line
				  x-draw-rectangle x-fill-rectangle
				  x-draw-arc x-fill-arc x-fill-polygon
				  x-copy-area x-draw-image))))
