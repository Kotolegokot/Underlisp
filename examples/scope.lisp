(define outer-1 () (inner))

(define outer-2 ()
  (define inner () (print "INSIDE"))
  (outer-1)
)

;(outer-1) would fail
(outer-2)

