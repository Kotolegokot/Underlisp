(module
  (defvar defmacro (macro (name args &rest body)
    (backquote (defvar (interpolate name) (macro (interpolate args) (unfold body))))))

  (defmacro when (cond &rest body)
                     (backquote (if (interpolate cond) (seq (unfold body)))))

  (defmacro unless (cond &rest body)
                     (backquote (when (not (interpolate cond)) (unfold body))))

  (defmacro define (name args &rest body)
    (backquote (defvar (interpolate name) (lambda (interpolate args) (unfold body)))))

  (current-context)
)
