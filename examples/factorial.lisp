(load-module "stdlib/ord.lisp")

;; factorial
(define factorial (x) (if (<= x 1) 1 (* x (factorial (- x 1)))))

(print-ln "FACTORIAL INSIDE: " (function-context factorial))
