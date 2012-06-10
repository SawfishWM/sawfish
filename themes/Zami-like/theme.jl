;; theme file, written Tue Aug  1 13:08:21 2000
;; created by sawfish-themer -- DO NOT EDIT!

(require 'make-theme)

(let
    ((patterns-alist
      '(("bottom-border"
         (inactive
          "bottom-border.png")
         (focused
          "bottom-border.png"))
        ("left-border"
         (inactive
          "left-border.png")
         (focused
          "left-border.png"))
        ("right-border"
         (inactive
          "right-border.png")
         (focused
          "right-border.png"))
        ("bottom-left-corner"
         (inactive
          "bottom-left-corner-inactive.png")
         (focused
          "bottom-left-corner.png")
         (clicked
          "bottom-left-corner-pressed.png"))
        ("bottom-right-corner"
         (inactive
          "bottom-right-corner-inactive.png")
         (focused
          "bottom-right-corner.png")
         (clicked
          "bottom-right-corner-pressed.png"))
        ("top-left-corner"
         (inactive
          "top-left-corner.png")
         (focused
          "top-left-corner.png"))
        ("top-right-corner"
         (inactive
          "top-right-corner.png")
         (focused
          "top-right-corner.png"))
        ("title-background"
         (inactive
          "title-background.png")
         (focused
          "title-background.png"))
        ("title-foreground"
         (inactive
          "title-foreground-inactive.png")
         (focused
          "title-foreground.png"))
        ("title-foreground-left"
         (inactive
          "title-foreground-left.png")
         (focused
          "title-foreground-left.png"))
        ("title-foreground-right"
         (inactive
          "title-foreground-right.png")
         (focused
          "title-foreground-right.png"))
        ("title-text-colors"
         (inactive . "#000000")
         (focused . "#ffffff"))
        ("close-button"
         (inactive
          "close-button-inactive.png")
         (focused
          "close-button.png")
         (clicked
          "close-button-pressed.png"))
        ("iconify-button"
         (inactive
          "iconify-button-inactive.png")
         (focused
          "iconify-button.png")
         (clicked
          "iconify-button-pressed.png"))
        ("maximize-button"
         (inactive
          "maximize-button-inactive.png")
         (focused
          "maximize-button.png")
         (clicked
          "maximize-button-pressed.png"))
        ("bottom-border-transient"
         (inactive
          "bottom-border-transient.png")
         (focused
          "bottom-border-transient.png"))
        ("left-border-transient"
         (inactive
          "left-border-transient.png")
         (focused
          "left-border-transient.png"))
        ("right-border-transient"
         (inactive
          "right-border-transient.png")
         (focused
          "right-border-transient.png"))
        ("top-left-corner-transient"
         (inactive
          "top-left-corner-transient-inactive.png")
         (focused
          "top-left-corner-transient.png")
         (clicked
          "top-left-corner-transient-pressed.png"))
        ("top-right-border-transient"
         (inactive
          "top-right-corner-transient-inactive.png")
         (focused
          "top-right-corner-transient.png")
         (clicked
          "top-right-corner-transient-pressed.png"))
        ("bottom-left-corner-transient"
         (inactive
          "bottom-left-corner-transient-inactive.png")
         (focused
          "bottom-left-corner-transient.png")
         (clicked
          "bottom-left-corner-transient-pressed.png"))
        ("bottom-right-corner-transient"
         (inactive
          "bottom-right-corner-transient-inactive.png")
         (focused
          "bottom-right-corner-transient.png")
         (clicked
          "bottom-right-corner-transient-pressed.png"))
        ("title-transient"
         (inactive
          "title-transient.png")
         (focused
          "title-transient.png"))
        ("title-transient-shaped"
         (inactive
          "title-transient-shaped.png")
         (focused
          "title-transient-shaped.png"))
        ("title-transient-shaped-left"
         (inactive
          "title-transient-shaped-left-inactive.png")
         (focused
          "title-transient-shaped-left.png"))
        ("title-transient-shaped-right"
         (inactive
          "title-transient-shaped-right-inactive.png")
         (focused
          "title-transient-shaped-right.png"))
        ("title-shaped"
         (inactive
          "title-shaped-inactive.png")
         (focused
          "title-shaped.png"))
        ("title-shaped-left"
         (inactive
          "title-shaped-left.png")
         (focused
          "title-shaped-left.png"))
        ("title-shpaed-right"
         (inactive
          "title-shaped-right.png")
         (focused
          "title-shaped-right.png"))))

     (frames-alist
      '(("default"
         ((cursor . left_ptr)
          (bottom-edge . -7)
          (right-edge . 23)
          (left-edge . 23)
          (background . "bottom-border")
          (class . bottom-border))
         ((cursor . left_ptr)
          (left-edge . -2)
          (bottom-edge . 0)
          (top-edge . 0)
          (background . "left-border")
          (class . left-border))
         ((cursor . left_ptr)
          (right-edge . -2)
          (bottom-edge . 0)
          (top-edge . 0)
          (background . "right-border")
          (class . right-border))
         ((cursor . left_ptr)
          (bottom-edge . -7)
          (left-edge . -2)
          (background . "bottom-left-corner")
          (class . bottom-left-corner))
         ((cursor . left_ptr)
          (right-edge . -2)
          (bottom-edge . -7)
          (background . "bottom-right-corner")
          (class . bottom-right-corner))
         ((cursor . left_ptr)
          (top-edge . -24)
          (left-edge . -2)
          (background . "top-left-corner")
          (class . top-left-corner))
         ((cursor . left_ptr)
          (right-edge . -2)
          (top-edge . -24)
          (background . "top-right-corner")
          (class . top-right-corner))
         ((cursor . left_ptr)
          (left-edge . 0)
          (right-edge . 0)
          (top-edge . -24)
          (background . "title-background")
          (class . title))
         ((top-edge . -22)
          (right-edge . 35)
          (left-edge . 21)
          (text . window-name)
          (y-justify . center)
          (x-justify . center)
          (background . "title-foreground")
          (foreground . "title-text-colors")
          (class . title))
         ((left-edge . 19)
          (background . "title-foreground-left")
          (top-edge . -22)
          (class . title))
         ((right-edge . 33)
          (top-edge . -22)
          (background . "title-foreground-right")
          (class . title))
         ((top-edge . -18)
          (left-edge . 4)
          (background . "close-button")
          (class . close-button))
         ((right-edge . 18)
          (top-edge . -18)
          (background . "iconify-button")
          (class . iconify-button))
         ((right-edge . 4)
          (top-edge . -18)
          (background . "maximize-button")
          (class . maximize-button)))
        ("shaped"
         ((cursor . left_ptr)
          (left-edge . 21)
          (right-edge . 35)
          (top-edge . -24)
          (text . window-name)
          (y-justify . center)
          (x-justify . center)
          (background . "title-shaped")
          (foreground . "title-text-colors")
          (class . title))
         ((left-edge . -2)
          (top-edge . -24)
          (background . "title-shaped-left")
          (class . title))
         ((right-edge . -2)
          (cursor . left_ptr)
          (top-edge . -24)
          (background . "title-shpaed-right")
          (class . title))
         ((top-edge . -18)
          (left-edge . 4)
          (background . "close-button")
          (class . close-button))
         ((right-edge . 18)
          (top-edge . -18)
          (background . "iconify-button")
          (class . iconify-button))
         ((right-edge . 4)
          (top-edge . -18)
          (background . "maximize-button")
          (class . maximize-button)))
        ("transient"
         ((cursor . left_ptr)
          (left-edge . 11)
          (right-edge . 11)
          (bottom-edge . -4)
          (background . "bottom-border-transient")
          (class . bottom-border))
         ((cursor . left_ptr)
          (top-edge . 0)
          (bottom-edge . 0)
          (left-edge . -4)
          (background . "left-border-transient")
          (class . left-border))
         ((cursor . left_ptr)
          (right-edge . -4)
          (background . "right-border-transient")
          (bottom-edge . 0)
          (top-edge . 0)
          (class . right-border))
         ((cursor . left_ptr)
          (bottom-edge . -4)
          (left-edge . -4)
          (background . "bottom-left-corner-transient")
          (class . bottom-left-corner))
         ((cursor . left_ptr)
          (bottom-edge . -4)
          (right-edge . -4)
          (background . "bottom-right-corner-transient")
          (class . bottom-right-corner))
         ((cursor . left_ptr)
          (top-edge . -4)
          (left-edge . -4)
          (background . "top-left-corner-transient")
          (class . top-left-corner))
         ((cursor . left_ptr)
          (top-edge . -4)
          (right-edge . -4)
          (background . "top-right-border-transient")
          (class . top-right-corner))
         ((right-edge . 11)
          (left-edge . 11)
          (top-edge . -4)
          (background . "title-transient")
          (class . title)))
        ("shaped-transient"
         ((right-edge . 11)
          (left-edge . 11)
          (top-edge . -4)
          (foreground . "#000000000000")
          (y-justify . center)
          (x-justify . center)
          (text . window-name)
          (background . "title-transient-shaped")
          (class . title))
         ((left-edge . -4)
          (background . "title-transient-shaped-left")
          (top-edge . -4)
          (class . title))
         ((top-edge . -4)
          (right-edge . -4)
          (background . "title-transient-shaped-right")
          (class . title)))))

     (mapping-alist
      '((default . "default")
        (shaped . "shaped")
        (transient . "transient")
        (shaped-transient . "shaped-transient")
        (unframed . "nil")))

     (theme-name 'Zami-like))

  (add-frame-style
   theme-name (make-theme patterns-alist frames-alist mapping-alist))
  (when (boundp 'mark-frame-style-editable)
    (mark-frame-style-editable theme-name)))
