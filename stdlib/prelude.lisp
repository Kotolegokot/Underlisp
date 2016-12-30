;; (defmacro name lambda-list body)
(define defmacro
  (macro (name lambda-list &rest body)
         `(define ~name (macro ~lambda-list @body))))

;; (define name lambda-list body)
(defmacro defun (name lambda-list &rest body)
  `(define ~name (lambda ~lambda-list @body)))

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

;; (import-module module-name)
(defmacro import-module (filename)
  `(import-env (env-from-file ~filename)))

;; (load-module module-name)
(defmacro load-module (filename)
  `(load-env (env-from-file ~filename)))

;; (load-module-no-prelude module-name)
(defmacro load-module-no-prelude (filename)
  `(load-env
    (env-from-file-no-prelude ~filename)))

;; (apply function list)
(defmacro apply (f xs)
  `(~f @(eval xs)))

;; otherwise is used with `switch` macro
(define otherwise True)

;; not equal
(defun /= (x y)
  (not (= x y)))

;; type predicates
(defun list? (x)
  (= 'LIST (type x)))
(defun int? (x)
  (= 'INT (type x)))
(defun float? (x)
  (= 'FLOAT (type x)))
(defun char? (x)
  (= 'CHAR (type x)))
(defun bool? (x)
  (= 'BOOL (type x)))
(defun symbol? (x)
  (= 'SYMBOL (type x)))
(defun callable? (x)
  (= 'CALLABLE (type x)))
(defun env? (x)
  (= 'ENV (type x)))

;; tells whether x is an atom
(defun atom? (x)
  (/= 'LIST (type x)))

;; tells whether x is an empty list
(defun nil? (x)
  (and (list? x) (null x)))

;; prints error if ex is false
(defmacro assert (cond)
  `(unless ~cond
     (write '~cond)
     (put-char #newline)
     (error "assert failed")))

(define nil ())
(define PI  3.14159265359)
(define E   2.71828182845)
(define INF Infinity)

;; cosecant
(defun csc (x)
  (recip  (sin x)))

;; secant
(defun sec (x)
  (recip (cos x)))

;; arccosecant
(defun acsc (x)
  (asin (recip x)))

;; arcsecant
(defun asec (x)
  (acos (recip x)))

;; hyperbolic cosecant
(defun csch (x)
  (recip (sinh x)))

;; hyperbolic secant
(defun sech (x)
  (recip (cosh x)))

;; hyperbolic arccosecant
(defun acsch (x)
  (asinh (recip x)))

;; hyperbolic arcsecant
(defun asech (x)
  (acosh (recip x)))

;; reciprocal fraction
(defun recip (x)
  (/ 1 (float x)))
