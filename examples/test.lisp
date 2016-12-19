(defvar nil ())

(defmacro when (cond &rest body)
  `(if ~cond (seq @body)))

(defmacro unless (cond &rest body)
  `(when (not ~cond) @body))

(defmacro cond (&rest pairs)
  (if (null pairs)
      nil
    `(if ~(head (head pairs))
       ~(head (tail (head pairs)))
       (cond @(tail pairs)))))

(if (= 1 2)
    (print-ln 1)
  (if nil
      (print-ln 2)
    nil))

;;(cond ((= 1 2) (print-ln 1))
;;      (nil (print-ln 2)))
