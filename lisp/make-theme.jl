;; make-theme.jl -- 
;; $Id$

(require 'gaol)
(provide 'make-theme)
			      
(defun make-theme (patterns-alist frame-alist mapping-alist)
  (let*
      ((make-pattern
	(lambda (def)
	  (mapcar (lambda (x)
		    (when (stringp x)
		      (if (string-match "^#" x)
			  (get-color x)
			(gaol-eval `(make-image ',x))))) def)))
       (loaded-patterns
	(mapcar (lambda (cell)
		  (cons (car cell) (make-pattern (cdr cell))))
		patterns-alist))

       (make-frame-part
	(lambda (def)
	  (mapcar (lambda (cell)
		    (cons (car cell)
			  (cond ((and (eq (car cell) 'text)
				      (symbolp (cdr cell)))
				 (gaol-eval (cdr cell)))
				((and (memq (car cell)
					    '(foreground background))
				      (stringp (cdr cell)))
				 (if (string-match "^#" (cdr cell))
				     ;; color
				     (get-color (cdr cell))
				   (cdr (assoc (cdr cell) loaded-patterns))))
				((and (eq (car cell) 'font)
				      (stringp (cdr cell)))
				 (get-font (cdr cell)))
				(t
				 (cdr cell))))) def)))

       (real-frames
	(mapcar (lambda (cell)
		  (cons (car cell) (mapcar make-frame-part (cdr cell))))
		frame-alist)))

    (lambda (w type)
      (let
	  ((frame-name (or (cdr (assq type mapping-alist))
			   (cdr (assq 'default mapping-alist)))))
	(if frame-name
	    (or (cdr (assoc frame-name real-frames)) nil-frame)
	  nil-frame)))))

(gaol-add-function 'make-theme)
