(load-module "stdlib/list.lisp")

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
