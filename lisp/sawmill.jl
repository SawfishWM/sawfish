;;;; sawmill.jl -- initialisation script

(provide 'sawmill)

;; load standard libraries
(require 'functions)
(require 'cursors)
(require 'focus)
(require 'keymaps)
(require 'transient)
(require 'workspace)
(require 'place-window)

(load-all "autoload.jl" t)
(load-all (concat "os-" (symbol-name operating-system)) t)

;; Load site specific initialisation. Errors here are trapped since
;; they're probably not going to leave us in an unusable state
(unless (get-command-line-option "--no-rc")
  (condition-case error-data
      (progn
	;; First the site-wide stuff
	(load-all "site-init")

	;; then the users rep configuration, or site-wide defaults
	(or (load (concat (user-home-directory) ".reprc") t t)
	    (load "rep-defaults" t))

	;; then the sawmill specific user configuration
	(or (load (concat (user-home-directory) ".sawmillrc") t t)
	    (load "sawmill-defaults" t)))
    (error
     (format (stderr-file) "error in local config--> %S\n" error-data))))

;; Use all arguments which are left.
(let
    (arg)
  (while (setq arg (car command-line-args))
    (setq command-line-args (cdr command-line-args))
    (cond
      ((equal "-f" arg)
       (setq arg (car command-line-args))
       (setq command-line-args (cdr command-line-args))
       (funcall (read-from-string arg)))
      ((equal "-l" arg)
       (setq arg (car command-line-args))
       (setq command-line-args (cdr command-line-args))
       (load arg))
      ((equal "-q" arg)
       (throw 'quit 0))
      (t
       (load arg)))))
