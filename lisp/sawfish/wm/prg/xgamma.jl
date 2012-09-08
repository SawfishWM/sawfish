;; xgamma.jl -- xgamma integration

;; Copyright (C) 2012 Christopher Roy Bratusek <nano@tuxfamily.org>

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.prg.xgamma

  (export xgamma-set
          xgamma-get)

  (open rep
        rep.system
        rep.io.processes
        rep.io.timers
        rep.util.misc
        sawfish.wm.misc
	sawfish.wm.custom)

  (define-structure-alias xgamma sawfish.wm.prg.xgamma)

  ;; SAWFISHRC
  ;; (require 'sawfish.wm.prg.xgamma)
  ;; ;; set all three channels at once
  ;; (xgamma-set t #:gamma 0.5)
  ;; ;; set single channels
  ;; (xgamma-set nil #:red 0.4 #:green 0.5 #:blue 0.6)

  (defcustom init-xgamma nil
    "Set gamma values when Sawfish starts."
    :type boolean
    :group (misc apps))

  (defcustom xgamma-red 100
    "Gamma value for red channel in per cent."
    :type (range (0 . 200))
    :group (misc apps)
    :after-set (lambda () (xgamma-set-from-cfg t nil nil)))

  (defcustom xgamma-green 100
    "Gamma value for green channel in per cent."
    :type (range (0 . 200))
    :group (misc apps)
    :after-set (lambda () (xgamma-set-from-cfg nil t nil)))

  (defcustom xgamma-blue 100
    "Gamma value for blue channel in per cent."
    :type (range (0 . 200))
    :group (misc apps)
    :after-set (lambda () (xgamma-set-from-cfg nil nil t)))

  (define (xgamma-get)
    (if (program-exists-p "xgamma")
        (system "xgamma &")
      (message (format nil "xgamma executable not found in PATH."))))

  (define (xgamma-set all #!key
                          (gamma 1.0)
                          (red 1.0)
                          (green 1.0)
                          (blue 1.0))
    (if (program-exists-p "xgamma")
        (if all
	    (system (format nil "xgamma -gamma %s &" gamma))
          (system (format nil "xgamma -rgamma %s -ggamma %s -bgamma %s &" red green blue)))
      (display-message (format nil "xgamma executable not found in PATH."))))

  (define (xgamma-set-from-cfg red green blue)
    (if (program-exists-p "xgamma")
        (progn
          (when red
            (system (format nil "xgamma -rgamma %s &" (/ xgamma-red 100.))))
          (when green
            (system (format nil "xgamma -ggamma %s &" (/ xgamma-green 100.))))
          (when blue
            (system (format nil "xgamma -bgamma %s &" (/ xgamma-blue 100.)))))
      (display-message (format nil "xgamma executable not found in PATH."))))

  (unless batch-mode
    (when init-xgamma
      (add-hook 'after-initialization-hook (lambda () (xgamma-set-from-cfg t t t)) t))))
