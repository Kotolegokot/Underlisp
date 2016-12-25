;;; closures
(defvar closure
  (let ((a 12))
    (lambda (x) (+ x a))))

(assert (= (closure 1) 13))

(define flip (f)
  (lambda (x y) (f y x)))

(assert (= ((flip -) 1 2) 1))
