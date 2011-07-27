;; single-window-mode.jl - pedantic implementation of single-window-mode

;; (c) 2011 Christopher Roy Bratusek <nano@tuxfamily.org>

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

(define-structure single-window-mode

  (export toggle-single-window-mode
	  single-window-on-workspace-p
	  window-in-single-window-mode-list-p
	  workspace-in-single-window-mode-list-p
	  single-window-mode-get-window-for-workspace
	  single-window-mode-get-window-for-workspace
	  single-window-mode-get-workspace-for-window)

  (open rep
	rep.system
	rep.regexp
	sawfish.wm
	sawfish.wm.misc
        sawfish.wm.commands
        sawfish.wm.commands.move-resize
        sawfish.wm.state.iconify
	sawfish.wm.state.maximize
        sawfish.wm.windows
	sawfish.wm.events
        sawfish.wm.workspace
	sawfish.wm.util.groups)

  (define-structure-alias single-window-mode sawfish.wm.ext.single-window-mode)

  ;; from edge-actions-1.9 branch (will be removed once EA is merged into head.

  (define (window-on-current-workspace-p w)                                                                                                                                                                                  
    (= (car (window-get w 'workspaces)) current-workspace))

  (defvar single-window-mode-window-fullscreen nil)
  (defvar single-window-mode-list ())

  (define (single-window-mode-list-append ws class)
    "Add a single-window to the list."
    (setq single-window-mode-list
      (append single-window-mode-list `((,ws . ,class)))))

  (define (single-window-mode-list-remove ws class)
    "Remove a single-window from the list."
    ;; not working for classes with "-"?
    (setq single-window-mode-list
      (remove `(,ws . ,class) single-window-mode-list)))

  (define (single-window-on-workspace-p ws)
    "Whether there's a single-window on workspace `ws'."
    (if (assoc ws single-window-mode-list) t nil))

  (define (window-in-single-window-mode-list-p class)
    "Whether window `class' is in single-window-mode-list."
    (if (single-window-mode-get-workspace-for-window class) t nil))

  (define (workspace-in-single-window-mode-list-p ws)
    "Whether workspace `ws' is in single-window-mode-list."
    (if (single-window-mode-get-window-class-for-workspace ws) t nil))

  (define (single-window-mode-get-window-for-workspace ws)
    "Return single-window for workspace `ws' or nil, if none."
    (let ((class (cdr (assoc ws single-window-mode-list))))
      (if class (get-window-by-class class) nil)))

  (define (single-window-mode-get-window-class-for-workspace ws)
    "Return single-window's class for workspace `ws' or nil, if none."
    (cdr (assoc ws single-window-mode-list)))

  (define (single-window-mode-get-workspace-for-window class)
    "Return workspace for single-window `win' or nil, if none."
    (car (rassoc class single-window-mode-list)))

  (define (toggle-single-window-mode #!optional w)
    "Toggle single-window-mode for window `w'."
    (let ((win (if w w (input-focus)))
	  (window-ws))
      (setq window-ws (if (window-on-current-workspace-p win)
			  current-workspace
			;; XXX 'sticky winodws?
			(car (window-workspaces win))))
      ;; not yet working
      (message (format nil "win: %s > ws: %s\n class: %s > win-class-ws: %s" win window-ws (window-class win)
			   (single-window-mode-get-window-class-for-workspace window-ws)))
      (unless (string-match "-" (window-class win))
	(if (equal (window-class win)
		   (single-window-mode-get-window-class-for-workspace window-ws))
	    (single-window-mode-stop win)
	  (if (single-window-on-workspace-p window-ws)
	      (progn
		(single-window-mode-stop (single-window-mode-get-window-for-workspace window-ws))
		(single-window-mode-start win))
	  (single-window-mode-start win)))))
    (message (format nil "%s" single-window-mode-list)))

  (define-command 'toggle-single-window-mode toggle-single-window-mode)

  (define (single-window-mode-core start w)
    (let ((iconify-group-mode 'none)
	  (raise-windows-on-uniconify nil)
	  (default-window-animator 'none))
      (if start
          (progn (message (format nil "starting"))
	  (map-other-window-groups
	    (lambda (x)
	      (when (windows-share-workspace-p w x)
		(if (or (window-get x 'ignore)
			(window-get x 'avoid))
		    (lower-window x)
		  (iconify-window x)))) w))
        (message (format nil "stopping"))
	(map-other-window-groups
	    (lambda (x)
	      (when (windows-share-workspace-p w x)
		(if (or (window-get x 'ignore)
			(window-get x 'avoid))
		    (raise-window x)
		  (uniconify-window x)))) w))))

  (define (single-window-mode-start w)
    "Start single-window-mode for window `w'."
    
    (when single-window-mode-window-fullscreen
      (maximize-window-fullscreen w))

    (single-window-mode-list-append (if (window-on-current-workspace-p w)
					current-workspace
				      (car (window-workspaces w)))
				    (window-class w))

    (single-window-mode-core t w))

  (define (single-window-mode-stop w)
    "Stop single-window-mode for window `w'."
    
    (when single-window-mode-window-fullscreen
      (unmaximize-window w))

    (single-window-mode-list-remove (if (window-on-current-workspace-p w)
					current-workspace
				      (car (window-workspaces w)))
				    (window-class w))

    (single-window-mode-core nil w))

  (define (single-window-mode-after-add-window w)
    (when (and (window-get w 'single-window)
	       ;; not yet working
	       (not (string-match "-" (window-class w))))
      (toggle-single-window-mode w)))

  (define (single-window-mode-unmap-notify w)
    (let ((class (window-class w))
	  (ws (single-window-mode-get-workspace-for-window (window-class w)))
	  (in-list (window-in-single-window-mode-list-p (window-class w))))
      (message (format nil "unmap notify: ws: %s > class: %s > in-list: %s" ws class in-list))
      (when in-list
	(single-window-mode-list-remove ws class)
	(map-windows (lambda (x)
	  (if (or (window-get x 'ignored)
		  (window-get x 'avoid))
	      (raise-window x)
	    (uniconify-window x)))
	  (workspace-windows ws)))
      (message (format nil "unmap notify: %s" single-window-mode-list))))

  (unless batch-mode
    (add-hook 'after-add-window-hook single-window-mode-after-add-window t)
    (add-hook 'unmap-notify-hook single-window-mode-unmap-notify t)))
