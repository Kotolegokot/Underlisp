;; factorial
(defun factorial (x)
  (if (<= x 1)
      1
    (* x (factorial (pred x)))))
