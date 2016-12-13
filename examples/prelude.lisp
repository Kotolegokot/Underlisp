(defvar defmacro (macro (name args &rest body)
  `(defvar ~name (macro ~args @body))))

(defmacro when (cond &rest body)
  `(if ~cond (seq @body)))

(defmacro unless (cond &rest body)
  `(when (not ~cond) @body))

(defmacro define (name args &rest body)
  `(defvar ~name (lambda ~args @body)))

(defmacro load-module (filename)
  `(load-context (context-from-file ~filename)))
