(defvar defmacro (macro (name args &rest body)
  `(defvar ~name (macro ~args @body))))

(defmacro define (name args &rest body)
  `(defvar ~name (lambda ~args @body)))

(define outer-1 () (inner))

(define outer-2 ()
  (define inner () (print "INSIDE"))
  (outer-1)
)

;(outer-1) would fail
(outer-2)

