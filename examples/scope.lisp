(defvar defmacro (macro (name args &rest body)
  (backquote (defvar (interpolate name) (macro (interpolate args) (unfold body))))))

(defmacro define (name args &rest body)
  (backquote (defvar (interpolate name) (lambda (interpolate args) (unfold body)))))

(define outer-1 () (inner))

(define outer-2 ()
  (define inner () (print "INSIDE"))
  (outer-1)
)

;(outer-1) would fail
(outer-2)

