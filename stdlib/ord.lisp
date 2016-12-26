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
