(import-module "stdlib/ord.lisp")

;; factorial
(define factorial (y) (if (<= y 1) 1 (* y (factorial (- y 1)))))
