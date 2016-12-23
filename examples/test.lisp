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

(load-module "examples/factorial.lisp")

;;(print-ln (context-from-file "examples/factorial.lisp"))

(define f (x) (factorial (+ x 1)))

(f 12)
