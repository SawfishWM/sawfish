;;; uptimes.jl --- Record sawfish session uptimes.
;; Copyright 1999,2000,2001 by Dave Pearson <davep@davep.org>
;; $Revision: 1.7 $

;; uptimes.jl is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; The following code allows you to track the uptimes of your sawfish
;; sessions, this is essentially a port of uptimes.el to sawfish (see
;; <URL:http://www.davep.org/emacs/#uptimes.el> for the emacs version of
;; uptimes).
;;
;; To use this code simply drop it (or a compiled copy) into your load-path
;; and in your ~/.sawfishrc put:
;;
;; (require 'uptimes)
;;
;; You might want to place it as close to the top of the file as possible
;; because the "boottime" of your sawfish session is in reality measured
;; from the moment that uptimes.jl is loaded and evaluated.
;;
;; There are a number of commands that you might want to bind to keys. Two
;; are `uptimes-display-uptime' and `uptimes-display-uptimes'. The first
;; will display the uptime of your current sawfish session, the second will
;; display the last "n" and top "n" uptimes (where "n" is defined by the
;; value of `uptimes-count'). Both of these displays clear after
;; `uptimes-display-timeout' seconds.
;;
;; Two others are `uptimes-toggle-uptime' and `uptimes-toggle-uptimes'.
;; These commands display the same as the last two but instead of the
;; display clearing after a time they toggle the message on and off.

;;; Code:

;; Stuff we require.

(require 'timers)

;; Customise options.

(defgroup uptimes "Uptime Tracking")

(defcustom uptimes-display-timeout 5
  "Seconds to display the uptime before clearing the display."
  :group     uptimes
  :type      number
  :allow-nil nil)

(defcustom uptimes-database "~/.sawfish/uptimes"
  "File that holds the uptimes database."
  :group     uptimes
  :type      file-name
  :allow-nil nil)

(defcustom uptimes-count 10
  "Number of uptimes to keep on file."
  :group     uptimes
  :type      number
  :allow-nil nil)

(defcustom uptimes-autosave t
  "Should the uptime data be auto-saved?"
  :group     uptimes
  :type      boolean
  :after-set (lambda (var)
               (if (symbol-value var)
                   (uptimes-autosave-start)
                 (uptimes-autosave-stop))))

(defcustom uptimes-autosave-interval 300
  "How often, in seconds, should we autosave the data?"
  :group     uptimes
  :type      number
  :allow-nil nil
  :after-set (lambda ()
               (when uptimes-autosave
                 (uptimes-autosave-stop)
                 (uptimes-autosave-start))))

;; Non-customisable variables.

(defvar uptimes-boottime (current-time)
  "The time that uptimes.jl came into existence.")

(defvar uptimes-display-timer nil
  "Handle for the uptime display timer.")

(defvar uptimes-autosave-timer nil
  "Handle for the autosave timer.")

(defvar uptimes-last-n nil
  "Last `uptimes-count' uptimes.")

(defvar uptimes-top-n nil
  "Top `uptimes-count' uptimes.")

(defvar uptimes-uptime-visible nil
  "Is the uptime display visible?")

(defvar uptimes-uptimes-visible nil
  "Is the uptimes display visible?")

;; Main code.

(defun uptimes-key (#!optional boottime)
  "Return an `assoc' key for BOOTTIME.

If BOOTTIME is not supplied the value of `uptimes-boottime' is used."
  (let ((boot (or boottime uptimes-boottime)))
    (format nil "%d-%d" (car boot) (cdr boot))))

(defun uptimes-uptime (#!optional boottime endtime)
  "Return the difference between BOOTTIME and ENDTIME.

If BOOTTIME isn't supplied the value of `uptimes-boottime' is used.

If ENDTIME isn't supplied the return value of `current-time' is used."
  (let ((boot (or boottime uptimes-boottime))
        (end  (or endtime  (current-time))))
    (fix-time (cons (- (car end) (car boot))
                    (- (cdr end) (cdr boot))))))

(defun uptimes-uptime-list (#!optional boottime endtime)
  "Return the difference between BOOTTIME and ENDTIME as a list of values.

If BOOTTIME isn't supplied the value of `uptimes-boottime' is used.

If ENDTIME isn't supplied the return value of `current-time' is used.

The list contains (DAYS HOURS MINS SECS)."
  (let* ((uptime  (uptimes-uptime boottime endtime))
         (days    (car uptime))
         (hours   (truncate (/ (cdr uptime) 3600)))
         (mins    (truncate (/ (- (cdr uptime) (* hours 3600)) 60)))
         (secs    (- (cdr uptime) (* mins  60) (* hours 3600))))
    (list days hours mins secs)))

(defun uptimes-uptime-string (#!optional boottime endtime)
  "Return the difference between BOOTTIME and ENDTIME as a string.

If BOOTTIME isn't supplied the value of `uptimes-boottime' is used.

If ENDTIME isn't supplied the return value of `current-time' is used."
  (apply format (append '(() "%d.%02d:%02d:%02d") (uptimes-uptime-list boottime endtime))))

(defun uptimes-wordy-uptime (#!optional boottime endtime)
  "Return the difference between BOOTTIME and ENDTIME as text.

If BOOTTIME isn't supplied the value of `uptimes-boottime' is used.

If ENDTIME isn't supplied the return value of `current-time' is used."
  (let* ((uptime (uptimes-uptime-list boottime endtime))
         (days   (nth 0 uptime))
         (hours  (nth 1 uptime))
         (mins   (nth 2 uptime))
         (secs   (nth 3 uptime))
         (mul    (lambda (n word) (concat word (unless (= n 1) "s")))))
    (format nil   "%d %s, %d %s, %d %s and %d %s"
            days  (mul days  "day")
            hours (mul hours "hour")
            mins  (mul mins  "minute")
            secs  (mul secs  "second"))))

(defun uptimes-read-uptimes ()
  "Read the list of uptimes.

This function populates the variables `uptimes-last-n' and `uptimes-top-n'
with the historical uptimes. These values are read from the file pointed to
by `uptimes-database'."
  (when (file-exists-p uptimes-database)
    (let ((db (open-file uptimes-database 'read)))
      (unwind-protect
           (setq uptimes-last-n (read db)
                 uptimes-top-n  (read db))
        (close-file db)))))

(defun uptimes-save-uptimes ()
  "Save the list of uptimes.

This function writes the values of `uptimes-last-n' and `uptimes-top-n' to
the file pointed to by `uptimes-database'."
  (uptimes-update)
  (let ((db (open-file uptimes-database 'write)))
    (unwind-protect
         (progn
           (print uptimes-last-n db)
           (print uptimes-top-n db))
      (close-file db))))

(defun uptimes-truncate-list (uptimes)
  "Truncate UPTIMES to `uptimes-count' items in length."
  (let ((trunc-point (nthcdr (1- uptimes-count) uptimes)))
    (when (consp trunc-point)
      (setcdr trunc-point nil))
    uptimes))

(defun uptimes-update-list (uptimes now sort-pred)
  "Update the uptime record for NOW in UPTIMES and sort on SORT-PRED."
  (let* ((key  (uptimes-key))
         (this (cdr (assoc key uptimes))))
    (unless this
      (setq this    (cons uptimes-boottime nil)
            uptimes (append (list (cons key this)) uptimes)))
    (setcdr this now)
    (uptimes-truncate-list (sort uptimes sort-pred))))

(defun uptimes-update ()
  "Update the list of uptimes."
  (uptimes-read-uptimes)
  (let ((now (current-time)))
    (setq uptimes-last-n (uptimes-update-list
                          uptimes-last-n now
                          (lambda (x y) (> (cddr x) (cddr y))))
          uptimes-top-n  (uptimes-update-list
                          uptimes-top-n now
                          (lambda (x y)
                            (> (uptimes-uptime (cadr x) (cddr x))
                               (uptimes-uptime (cadr y) (cddr y))))))))

(defun uptimes-display-start ()
  "Start an uptime display."
  (when uptimes-display-timer
    (delete-timer uptimes-display-timer))
  (setq uptimes-display-timer
        (make-timer uptimes-display-clear uptimes-display-timeout 0)))

(defun uptimes-display-clear ()
  "Clear an uptime display."
  (interactive)
  (when uptimes-display-timer
    (display-message nil)
    (delete-timer uptimes-display-timer)
    (setq uptimes-display-timer nil)))

(defun uptimes-display-uptime (#!optional no-autoclear)
  "Display the sawfish session's uptime."
  (interactive)
  (display-message (format nil "sawfish has been up and running for\n%s"
                           (uptimes-wordy-uptime)))
  (unless no-autoclear
    (uptimes-display-start)))

(defun uptimes-toggle-uptime ()
  "Toggle the permanent display of the current uptime."
  (interactive)
  (if uptimes-uptime-visible
      (progn
        (display-message)
        (setq uptimes-uptime-visible nil))
    (uptimes-display-uptime t)
    (setq uptimes-uptime-visible  t
          uptimes-uptimes-visible nil)))

(defun uptimes-format-uptime (s uptime)
  "Format UPTIME onto stream S."
  (let ((format-time
         (lambda (time)
           (current-time-string time "%Y-%m-%d %T"))))
    (format s "%19s %19s %12s %s\n"
            (format-time (cadr uptime))
            (format-time (cddr uptime))
            (uptimes-uptime-string (cadr uptime) (cddr uptime))
            (if (string= (car uptime) (uptimes-key)) "<---" ""))))

(defun uptimes-display-uptimes (#!optional no-autoclear)
  "Display the sawfish uptime history."
  (interactive)
  (uptimes-update)
  (let* ((msg    (make-string-output-stream))
         (fmt    (lambda (uptime) (uptimes-format-uptime msg uptime)))
         (header (lambda (s)
                   (format s "Boot                Endtime             Uptime       This sawfish\n")
                   (format s "=================== =================== ============ ============\n"))))
    (format msg "Last %d sawfish uptimes\n\n" uptimes-count)    
    (header msg)
    (mapc fmt uptimes-last-n)
    (format msg "\nTop %d sawfish uptimes\n\n" uptimes-count)
    (header msg)
    (mapc fmt uptimes-top-n)
    (display-message (get-output-stream-string msg))
    (unless no-autoclear
      (uptimes-display-start))))

(defun uptimes-toggle-uptimes ()
  "Toggle the permanent display of the uptime history."
  (interactive)
  (if uptimes-uptimes-visible
      (progn
        (display-message)
        (setq uptimes-uptimes-visible nil))
    (uptimes-display-uptimes t)
    (setq uptimes-uptimes-visible t
          uptimes-uptime-visible nil)))

(defun uptimes-autosave-uptimes ()
  "Timer handler for the autosaving of uptimes."
  (uptimes-save-uptimes)
  (set-timer uptimes-autosave-timer))

(defun uptimes-autosave-start ()
  "Turn on autosaving of the uptimes."
  (unless uptimes-autosave-timer
    (setq uptimes-autosave-timer
          (make-timer uptimes-autosave-uptimes uptimes-autosave-interval 0))))

(defun uptimes-autosave-stop ()
  "Stop the autosave timer."
  (when uptimes-autosave-timer
    (delete-timer uptimes-autosave-timer)
    (setq uptimes-autosave-timer nil)))

;; Stuff to do when we're loaded.

(uptimes-save-uptimes)
(when uptimes-autosave
  (uptimes-autosave-start))
(unless (in-hook-p 'before-exit-hook uptimes-save-uptimes)
  (add-hook 'before-exit-hook uptimes-save-uptimes))

(provide 'uptimes)

;;; uptimes.jl ends here
