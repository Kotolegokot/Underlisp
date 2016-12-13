(load-module "examples/factorial.lisp")

(define null (xs)
  (= 0 (length xs)))

(define foreach (f xs)
  (if (null xs)
    (quote ())
    (append (list (f (head xs))) (foreach f (tail xs)))))

;(define zip (xs ys)
;   (defvar 

(defvar l (quote (1 2 3 4)))
(defvar ll (foreach factorial l))

(print-ln l)
(print-ln ll)
