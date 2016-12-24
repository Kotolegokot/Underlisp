(define f (x) (* x x))
(print-ln (f 10))
(defvar previous-context (current-context))

(define f (x) (* x x x))
(print-ln (f 10))

(seq
 (load-context previous-context)
 (print-ln (f 10)))
