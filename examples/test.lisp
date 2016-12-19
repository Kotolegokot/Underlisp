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

(define f (x) (print-ln x))

(f 12)
