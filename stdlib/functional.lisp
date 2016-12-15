(load-module "stdlib/list.lisp")

;; swap a function's args
(define flip (f)
  (lambda (x y) (f y x)))

;; compose two functions
(define compose (f g)
  (lambda (x) (f (g x))))

;; the identity
(define id (x)
  x)

;; const returns the same value regardless its argument
(define const (x)
  (lambda (y) x))

;; same as (compose x1 (compose x2 (... xn)))
(define <<< (&rest fs)
  (if (null fs)
      id
    (compose (head fs) (apply <<< (tail fs)))))

;; same as (compose xn (compose x{n - 1} (... x1)))
(define >>> (&rest fs)
  (if (null fs)
      id
    (compose (apply >>> (tail fs)) (head fs))))

;; makes a function that takes a list take any
;; number of arguments
(define curry (f)
  (lambda (&rest rest)
    (f rest)))

;; make a function that takes any number of
;; arguments take a list
(define uncurry (f)
  [apply f])

;; make a one parameter function take a list
;; and modify its head
(define first (f)
  (lambda (list)
    (prepend (f (head list))
            (tail list))))


;; make a one parameter function take a list
;; and modify its second element
(define second (f)
  (lambda (list)
    (prepend (head list)
            (prepend (f (head (tail list)))
                    (tail (tail list))))))
