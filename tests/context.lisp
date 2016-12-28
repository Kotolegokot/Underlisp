(defun f (x) (* x x))
(define outer-env (current-env))

(assert (= (f 3) 9))

(seq
 (defun f (x) (* x x x))
 (assert (= (f 3) 27))

 ;; f must remain same
 ;; because import-env can't
 ;; rewrite local functions
 (import-env outer-env)
 (assert (= (f 3) 27))

 ;; but in this case
 ;; it must change f
 ;; because now both f's
 ;; are external
 (seq
  (import-env outer-env)
  (assert (= (f 3) 9))))

(seq
 (defun f (x) (* x x x))
 (assert (= (f 3) 27))

 ;; f must change
 ;; because load-env
 ;; can rewrite local
 ;; functions
 (load-env outer-env)
 (assert (= (f 3) 9))
)
