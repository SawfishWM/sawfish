;; functions.jl -- miscellaneous stuff
;; $Id$

(provide 'functions)

(defun get-window-by-name (name)
  (catch 'foo
    (mapc #'(lambda (w)
	      (when (string= (window-name w) name)
		(throw 'foo w))) (managed-windows))
    nil))

(defun get-window-by-id (id)
  (catch 'foo
    (mapc #'(lambda (w)
	      (when (= (window-id w) id)
		(throw 'foo w))) (managed-windows))))
