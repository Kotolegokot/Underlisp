(load-module "stdlib/ord.lisp")

(define factorial (x)
  (if (<= x 1)
      1
    (* x (factorial (- x 1)))))

(print-ln (factorial 12))

(define flip (f)
  (lambda (x y) (f y x)))

(print-ln ((flip -) 1 2))

