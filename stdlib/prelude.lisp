; (defmacro macro-name args body)
(defvar defmacro (macro (name args &rest body)
  `(defvar ~name (macro ~args @body))))

; (when true-condition body)
(defmacro when (cond &rest body)
  `(if ~cond (seq @body)))

; (unless false-condition body)
(defmacro unless (cond &rest body)
  `(when (not ~cond) @body))

; (switch (condition1 exp1) (condition2 exp2) ...)
(defmacro switch (&rest pairs)
  (if (null pairs)
    ''()
    `(if ~(head (head pairs))
       ~(head (tail (head pairs)))
       (switch @(tail pairs)))))

; (define function-name args body)
(defmacro define (name args &rest body)
  `(defvar ~name (lambda ~args @body)))

; (load-module module-name)
(defmacro load-module (filename)
  `(load-context (context-from-file ~filename)))

; (apply function list)
(defmacro apply (f xs)
  `(~f @xs))

; otherwise is used with `cond` macro
(defvar otherwise True)

; not equal
(define /= (x y)
  (not (= x y)))

