;;; shadowing functions
(seq
 (define f (x) (* x x))

 (assert (= (f 3) 9))

 (seq
  (define f (x) (* x x x))
  (assert (= (f 3) 27)))

 (assert (= (f 3) 9)))

;;; a function's local context
;;; shouldn't depend on the
;;; environment
(seq
 (define f (x) (* x x))
 (define g (x) (+ (f x) 3))

 (assert (= (g 4) 19))
 (seq
  ;; change f
  ;; but g must remain unchanged
  (define f (x) (* x x x))
  (assert (= (g 4) 19)))

 (assert (= (g 4) 19)))
