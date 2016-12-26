;;; shadowing functions
(seq
 (defun f (x) (* x x))

 (assert (= (f 3) 9))

 (seq
  (defun f (x) (* x x x))
  (assert (= (f 3) 27)))

 (assert (= (f 3) 9)))

;;; a function's local context
;;; shouldn't depend on the
;;; environment
(seq
 (defun f (x) (* x x))
 (defun g (x) (+ (f x) 3))

 (assert (= (g 4) 19))
 (seq
  ;; change f
  ;; but g must remain unchanged
  (defun f (x) (* x x x))
  (assert (= (g 4) 19)))

 (assert (= (g 4) 19)))

(import-module "stdlib/ord.lisp")

(defun f-1 (x)
  (if (<= x 1)
      1
    (* x (f-2 (- x 1)))))

(defun f-2 (x)
  (if (<= x 1)
      1
    (* x (f-1 (- x 1)))))

(assert (= (f-1 5) 120))
