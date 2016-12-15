;; less or equal
(define <= (x y)
  (or (< x y)
      (= x y)))

;; greater than
(define > (x y)
  (not (<= x y)))

;; greater or equal
(define >= (x y)
  (not (< x y)))

;; (compare x) returns 'LT, 'EQ, or 'GT
(define compare (x y)
  (if (= x y)
      'EQ
    (if (< x y)
      'LT
      'GT)))
