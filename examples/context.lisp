(program
  ; some handy macros
  (defvar defmacro (macro (name args &rest body)
    (backquote (defvar (interpolate name) (macro (interpolate args) (unfold body))))))
  (defmacro define (name args &rest body)
    (backquote (defvar (interpolate name) (lambda (interpolate args) (unfold body)))))

  ; the program itself
  (define f (x) (* x x))
  (defvar +context (context (quote f)))

  (define f (x) (+ x x))
  (print "f 12.45 = ")
  (print-ln (f 12.45))

  (load-context +context)
  (print-ln "loading previous context...")
  (print "f 12.45 = ")
  (print-ln (f 12.45))
)
