(load-module "examples/factorial.lisp")

; check whether xs is empty or not
(define null (xs)
  (= 0 (length xs)))

; apply f to every element of xs
(define foreach (f xs)
  (if (null xs)
    (quote ())
    (append (list (f (head xs))) (foreach f (tail xs)))))

; (zip '(1 2 3) '(a b c)) -> ((1 a) (2 b) (3 c))
(define zip (xs ys)
  (define zip' (xs ys acc)
    (if (or (null xs) (null ys))
      acc
      (zip' (tail xs) (tail ys)
            (append acc (list (list (head xs) (head ys)))))))

  (zip' xs ys (quote ())))

(defvar l (quote (1 2 3 4)))
(defvar ll (foreach factorial l))

(print-ln l)
(print-ln ll)
(print-ln (zip l ll))
