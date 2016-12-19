(defvar nil ())

(defmacro when (cond &rest body)
  `(if ~cond (seq @body)))

(defmacro unless (cond &rest body)
  `(when (not ~cond) @body))

(defmacro cond (&rest pairs)
  (if (null pairs)
      nil
    `(if ~(head (head pairs))
       ~(head (tail (head pairs)))
       (cond @(tail pairs)))))

;;(load-module "examples/factorial.lisp")
(load-module "stdlib/ord.lisp")

;;(define f (x) (factorial (+ x 1)))

;;(f 12)

(define fctr (x)
  (if (<= x 1)
      1
    (* x (fctr (- x 1)))))

(print-ln "fctr 12 = " (fctr 12))
