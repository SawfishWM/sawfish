(setq load-path (nconc load-path (list "/var/lib/sawfish")))
(let*
    ((dir "/etc/X11/sawfish/site-init.d")
     (files (and (file-directory-p dir) (directory-files dir))))
  (mapc
    (lambda (file)
      (condition-case data
          (load (expand-file-name file dir) t t t)
        (error (error-handler-function (car data) (cdr data)))))
    (sort files)))
