;; General utilities

(define-structure sawflibs.utils
    (export take group-by)
    (open rep)

  (define (take n l)
    (cond ((null l) '())
          ((<= n 0) '())
          ((= 1 n) (list (car l)))
          (t (cons (car l) (take (- n 1) (cdr l))))))

  (define (group-by ws n)
    (cond ((null ws) ws)
          ((< n 1) (list ws))
          ((<= (length ws) n) (list ws))
          (t (cons (take n ws) (group-by (nthcdr n ws) n))))))
