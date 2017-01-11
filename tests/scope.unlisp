;;; shadowing functions
(scope
 (defun f () "outer function")

 (assert (= (f) "outer function"))

 (scope
  (defun f () "inner function")
  (assert (= (f) "inner function")))

 (assert (= (f) "outer function")))

;;; a function's local context
;;; shouldn't depend on the
;;; environment
(scope
 (defun f () "f")
 (defun g () (append (f) "g"))

 (assert (= (g) "fg"))
 (scope
  ;; change f
  ;; but g must remain unchanged
  (defun f () "f2")
  (assert (= (g) "fg")))

 (assert (= (g) "fg")))

;;; indirect recursion
;;; f-1 calls f-2, f-2 calls f-1
(scope
 (defun f-1 (x)
   (if (<= x 1)
       1
       (* x (f-2 (- x 1)))))

 (defun f-2 (x)
   (if (<= x 1)
       1
       (* x (f-1 (- x 1)))))

 (assert (= (f-1 5) 120)))

;;; function definition can't be seen
;;; outside a scope, but it can with seq
(scope
 (defun f () "outer function")

 (scope
  (defun f () "inner function"))

 (assert (= (f) "outer function"))

 (seq
  (defun f () "inner function"))

 (assert (= (f) "inner function")))
