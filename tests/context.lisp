(define f (x) (* x x))
(defvar outer-context (current-context))

(assert (= (f 3) 9))

(seq
 (define f (x) (* x x x))
 (assert (= (f 3) 27))

 ;; f must remain same
 ;; because import-context can't
 ;; rewrite local functions
 (import-context outer-context)
 (assert (= (f 3) 27))

 ;; but in this case
 ;; it must change f
 ;; because now both f's
 ;; are external
 (seq
  (import-context outer-context)
  (assert (= (f 3) 9))))

(seq
 (define f (x) (* x x x))
 (assert (= (f 3) 27))

 ;; f must change
 ;; because load-context
 ;; can rewrite local
 ;; functions
 (load-context outer-context)
 (assert (= (f 3) 9))
)
