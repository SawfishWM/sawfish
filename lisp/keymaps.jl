;; keymaps.jl -- the default keymaps
;; $Id$

(provide 'keymaps)

(defvar global-keymap (make-sparse-keymap)
  "Keymap containing bindings active anywhere.")

(defvar root-window-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the pointer is in the root window.")

(defvar title-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the title of a window is focused.")

(defvar iconify-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the iconify button of a window
is focused.")

(defvar maximize-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the maximize button of a window
is focused.")

(defvar close-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the close button of a window
is focused.")

(defvar menu-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the menu button of a window
is focused.")

(defvar window-keymap (make-sparse-keymap)
  "Keymap containing bindings active when a window is focused.")


;; Arrange for window-keymap to be set in each window

(defun keymap-add-window (w)
  (unless (window-get w 'keymap)
    (window-put w 'keymap window-keymap)))

(add-hook 'add-window-hook 'keymap-add-window)


;; some bindings

(bind-keys title-keymap
  "Button3-Off" 'raise-lower-window
  "Button1-Move" 'move-window-interactively
  "Button2-Move" 'resize-window-interactively)

(bind-keys window-keymap
  "C-M-Up" 'raise-window
  "C-M-Down" 'lower-window
  "C-M-Left" 'send-to-previous-workspace
  "C-M-Right" 'send-to-next-workspace
  "M-Button1-Click1" 'move-window-interactively
  "M-Button3-Click1" 'raise-lower-window)

(bind-keys global-keymap
  "C-Left" 'previous-workspace
  "C-Right" 'next-workspace
  "C-M-ESC" 'quit)

(bind-keys root-window-keymap
  "Button1-Click1" 'popup-root-menu)

(bind-keys close-button-keymap
  "Button1-Off" 'delete-window
  "Button3-Off" 'popup-window-menu)

(bind-keys iconify-button-keymap
  "Button1-Off" 'iconify-window
  "Button3-Off" 'delete-window)

(bind-keys menu-button-keymap
  "Button1-Off" 'popup-window-menu
  "Button3-Off" 'delete-window)
