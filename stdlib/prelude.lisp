;; (when true-condition body)
(defmacro when (cond &rest body)
  `(if ~cond (seq @body)))

;; (unless false-condition body)
(defmacro unless (cond &rest body)
  `(when (not ~cond) @body))

;; (switch (condition1 exp1) (condition2 exp2) ...)
(defmacro switch (&rest pairs)
  (if (null pairs)
      ''()
    `(if ~(head (head pairs))
       ~(head (tail (head pairs)))
       (switch @(tail pairs)))))

;; (load-module module-name)
(defmacro load-module (filename)
  `(load-context (context-from-file ~filename)))

;; (load-module-no-prelude module-name)
(defmacro load-module-no-prelude (filename)
  `(load-context
    (context-from-file-no-prelude ~filename)))

;; (apply function list)
(defmacro apply (f xs)
  `(~f @(eval xs)))

;; otherwise is used with `switch` macro
(defvar otherwise True)

;; not equal
(define /= (x y)
  (not (= x y)))

;; type predicates
(define list? (x)
  (= 'LIST (type x)))
(define int? (x)
  (= 'INT (type x)))
(define float? (x)
  (= 'FLOAT (type x)))
(define string? (x)
  (= 'STRING (type x)))
(define char? (x)
  (= 'CHAR (type x)))
(define bool? (x)
  (= 'BOOL (type x)))
(define symbol? (x)
  (= 'SYMBOL (type x)))
(define callable? (x)
  (= 'CALLABLE (type x)))
(define context? (x)
  (= 'CONTEXT (type x)))

;; tells whether x is an atom
(define atom? (x)
  (/= 'LIST (type x)))

;; tells whether x is an empty list
(define nil? (x)
  (and (list? x) (null x)))

;; prints error if ex is false
(defmacro assert (ex)
  `(unless ~ex
     (print-ln '~ex)
     (error "assert failed")))
