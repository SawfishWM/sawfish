;; prompt-wm.jl -- prompt variants for windows/workspaces
;; $Id$

;; Contributed by Dave Pearson <davep@davep.org>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'prompt)

;;;###autoload
(defun prompt-for-window (&optional title)
  "Prompt for a window title, return the window associated with that title."
  (letrec ((show-in-list-p
            (lambda (w)
              (and
               (not (window-get w 'ignored))
               (window-get w 'workspaces))))
           (window-names
            (lambda (windows)
              (when windows
                (if (show-in-list-p (car windows))
                    (cons (window-name (car windows))
			  (window-names (cdr windows)))
                  (window-names (cdr windows))))))
           (names-matching
            (lambda (re names)
              (when names
                (if (string-match re (car names) nil t)
                    (cons (car names) (names-matching re (cdr names)))
                  (names-matching re (cdr names))))))
           (prompt-completion-fun
            (lambda (text)
              (names-matching (format nil "^%s" text)
                              (sort (window-names (managed-windows)))))))
    (let ((window-title (prompt (or title (_ "Window:")))))
      (unless (zerop (length window-title))
        (cdr (assoc window-title (mapcar (lambda (w)
					   (cons (window-name w) w))
					 (managed-windows))))))))

;;;###autoload
(defun prompt-for-workspace (&optional title)
  "Prompt for a workspace title, return the workspace number."
  (letrec ((make-workspace-list
            (lambda (ws)
              (unless (zerop ws)
                (cons (or (nth (1- ws) workspace-names)
                          (format nil (_ "Workspace %d") ws))
                      (make-workspace-list (1- ws))))))
           (workspaces
            (lambda ()
              (reverse (make-workspace-list (1+ (cdr (workspace-limits)))))))
           (names-matching
            (lambda (re names)
              (when names
                (if (string-match re (car names))
                    (cons (car names) (names-matching re (cdr names)))
                  (names-matching re (cdr names))))))
           (prompt-completion-fun
            (lambda (text)
              (names-matching (format nil "^%s" text) (workspaces)))))
    (let ((ws-title (prompt (or title (_ "Workspace:"))))
          (wsl (workspaces)))
      (unless (zerop (length ws-title))
        (let ((where (member ws-title wsl)))
          (when where
            (- (length wsl) (length where))))))))

;;;###autoload
(defun select-workspace-interactively ()
  "Prompt for a workspace and switch to it."
  (interactive)
  (let ((ws (prompt-for-workspace)))
    (when ws
      (select-workspace-from-first ws))))
