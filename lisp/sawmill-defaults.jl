;; sawmill-default.jl -- User startup script if ~/.sawmillrc doesn't exist
;; $Id$

;; open the server, use the sawmill-client program to connect to the
;; running window manager
(server-open)

;; use sloppy-focus
(setq sloppy-focus t)

;; the font to use for otherwise unspecified text
(setq default-font (get-font "-*-lucida-medium-r-*-*-10-*-*-*-*-*-*-*"))

;; load a theme, take your pick from the few choices..
(load "absolute-e")
;(load "brushed-metal")
;(load "simple")

(defvar ignored-window-names "^(xload|xmeter|Dali Clock|xapm|xbuffy|console)$"
  "Regular expression matching windows that don't get a frame.")

(defvar sticky-window-names "^(xload|xmeter|Dali Clock|xapm|xbuffy|console)$"
  "Regular expression matching window names that exist across workspaces.")

;; this function is called for each window that is created on the display
(defun user-add-window (w)
  (when (string-match ignored-window-names (window-name w))
    (set-window-frame w nil))
  (when (string-match sticky-window-names (window-name w))
    (window-put w 'sticky t)))

;; add the above function into the `add-window-hook' hook
(add-hook 'add-window-hook 'user-add-window)
