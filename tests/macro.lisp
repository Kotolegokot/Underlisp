(defvar put-in-the-middle
  (macro (x xs) `(~(head xs) ~x ~(head (tail xs)))))

(assert (= (put-in-the-middle 4 (- 3)) 1))
