(load-module "stdlib/ord.lisp")

;; factorial
(define factorial (x) (if (<= x 1) 1 (* x (factorial (- x 1)))))
