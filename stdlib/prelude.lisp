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

;; (cond (condition1 exp1) (condition2 exp2) ...)
(defmacro cond (&rest pairs)
  (if (null pairs)
      ()
    `(if ~(head (head pairs))
       ~(head (tail (head pairs)))
       (cond @(tail pairs)))))

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

;; otherwise is used with `cond` macro
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
(define pi  3.14159265359)
(define e   2.71828182845)

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
;; less or equal
(defun <= (x y)
  (or (< x y)
      (= x y)))

;; greater than
(defun > (x y)
  (not (<= x y)))

;; greater or equal
(defun >= (x y)
  (not (< x y)))

;; (compare x) returns 'LT, 'EQ, or 'GT
(defun compare (x y)
  (if (= x y)
      'EQ
    (if (< x y)
      'LT
      'GT)))

(defun neg (x)
  (* -1 x))

(defun zero? (x)
  (= x 0))

(defun neg? (x)
  (< x 0))

(defun pos? (x)
  (> x 0))

(defun atan2 (y x)
  (cond
   ((pos? x)                 (atan (/ y x)))
   ((and (zero? x) (pos? y)) (/ pi 2))
   ((and (neg? x)  (pos? y)) (+ pi (atan (/ y x))))
   ((or
     (and (<= x 0) (neg? y))
     (and (neg? x) (negative-zero? y))
     (and (negative-zero? x) (negative-zero? y)))
    (neg (atan2 (neg y) x)))
   ((and (zero? y)
         (or (neg? x)
             (negative-zero? x)))
    pi)
   ((and (zero? x) (zero? y)) y)
   (otherwise (+ x y))))
