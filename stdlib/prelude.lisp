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

(defun reverse (xs)
  (if (null xs)
      '()
    (append (reverse (tail xs))
            (list (head xs)))))

(defun length (xs)
  (foldl (lambda (acc _) (+ acc 1)) 0 xs))

(defun prepend (x xs)
  (append (list x) xs))

(defun init (xs)
  (if (null xs)
      (error "init: empty list")
    (if (= 1 (length xs))
            '()
      (prepend (head xs) (init (tail xs))))))

(defun last (xs)
  (if (null xs)
      (error "last: empty list")
    (if (= 1 (length xs))
        xs
      (last (tail xs)))))

(defun nth (n xs)
  (cond ((null xs)       (error "nth: empty list"))
	((>= n (length xs)) (error "nth: out of bounds"))
	((< n 0)            (error "nth: negative index"))
	(otherwise
	 (if (= n 0)
	     (head xs)
	   (nth (- n 1) (tail xs))))))

(defun map (f xs)
  (if (null xs)
      nil
    (prepend (f (head xs))
             (map f (tail xs)))))

(defun map-nil (f xs)
  (map f xs)
  nil)

(defun foldl (f acc xs)
  (if (null xs)
      acc
    (foldl f (f acc (head xs))
           (tail xs))))

(defun foldr (f acc xs)
  (if (null xs)
      acc
    (foldr f (f (last xs) acc)
           (init xs))))

(defun zip (xs ys)
  (if (or (null xs) (null ys))
      '()
    (prepend (list (head xs) (head ys))
             (zip (tail xs) (tail ys)))))

(defun zip-with (f xs ys)
  (if (or (null xs) (null ys))
      '()
    (prepend (f (head xs) (head ys))
             (zip-with f (tail xs) (tail ys)))))

(defun elem (y xs)
  (foldl (lambda (acc x) (if (= x y) True acc)) False xs))

(defun filter (p xs)
  (if (null xs)
      '()
    (if (p (head xs))
        (prepend (head xs) (filter p (tail xs)))
      (filter p (tail xs)))))

(defun all (p xs)
  (foldl (lambda (acc x) (if (p x) acc False)) True xs))

(defun any (p xs)
  (foldl (lambda (acc x) (if (p x) True acc)) False xs))

(defun find (p xs)
  (if (null xs)
      '()
    (if (p (head xs))
        (head xs)
      (find (p (tail xs))))))

(defun take (n xs)
  (cond ((> n (length xs)) (error "take: out of bounds"))
	((< n 0)           (error "take: negative number"))
	(otherwise
	 (if (= n 0)
	     '()
	   (prepend (head xs) (take (- n 1) (tail xs)))))))

(defun drop (n xs)
  (cond ((> n (length xs)) (error "drop: out of bounds"))
	((< n 0)           (error "drop: negative number"))
	(otherwise
	 (if (= n 0)
	     xs
	   (drop (- n 1) (tail xs))))))

(defun cons (a b)
  (if (list? b)
      (prepend a b)
    (list a b)))

(defun find (x xs)
  (cond
   ((null xs)       False)
   ((= x (head xs)) True)
   (otherwise       (find x (tail xs)))))

(defmacro case (expr &rest pairs)
  (defun handle-pairs (expr-var pairs)
    (if (null pairs)
	nil
      (let ((first (head pairs)))
	(prepend
	 `((= ~expr-var ~(head first))
	   ~(head (tail first)))
	 (handle-pairs expr-var (tail pairs))))))

  (let ((expr-var (gensym)))
    `(let ((~expr-var ~expr))
       (cond @(handle-pairs expr-var pairs)))))

(defun newline ()
  (put-char #newline))

(defun print-string (str)
  (map-nil put-char str))

(defun print-string-ln (str)
  (print-string str)
  (newline))

(defun write-ln (s-expr)
  (write s-expr)
  (newline))

;; swap a function's args
(defun flip (f)
  (lambda (x y) (f y x)))

;; compose two functions
(defun compose (f g)
  (lambda (x) (f (g x))))

;; the identity
(defun id (x) x)

;; const returns the same value regardless its argument
(defun const (x)
  (lambda (y) x))

;; same as (compose x1 (compose x2 (... xn)))
(defun <<< (&rest fs)
  (if (null fs)
      id
    (compose (head fs) (apply <<< (tail fs)))))

;; same as (compose xn (compose x{n - 1} (... x1)))
(defun >>> (&rest fs)
  (if (null fs)
      id
    (compose (apply >>> (tail fs)) (head fs))))

;; makes a function that takes a list take any
;; number of arguments
(defun curry (f)
  (lambda (&rest rest)
    (f rest)))

;; make a function that takes any number of
;; arguments take a list
(defun uncurry (f)
  [apply f])

;; make a one parameter function take a list
;; and modify its head
(defun first (f)
  (lambda (list)
    (let ((e1 (nth 0 list))
          (e2 (nth 1 list)))
      (cons (f e1) e2))))

;; make a one parameter function take a list
;; and modify its second element
(defun second (f)
  (lambda (list)
    (let ((e1 (nth 0 list))
          (e2 (nth 1 list)))
      (cons e1 (f e2)))))

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

(defun tan (x)
  (/ (sin x) (cos x)))

(defun cot (x)
  (/ (cos x) (sin x)))

(defun tanh (x)
  (/ (sinh x) (cosh x)))

(defun coth (x)
  (/ (cosh x) (sinh x)))

(defun sqrt (x)
  (^ x 0.5))

(defun log (x y)
  (/ (ln x) (ln y)))

(defun quot (x y)
  (head (quot-rem x y)))

(defun rem (x y)
  (head (tail (quot-rem x y))))

(defun div (x y)
  (head (div-mod x y)))

(defun mod (x y)
  (head (tail (div-mod x y))))

(defun sign (x)
  (cond
   ((zero? x) 0)
   ((neg?  x) -1)
   ((pos?  x) 1)))

(define succ [+ 1])
(define pred [(flip -) 1])
