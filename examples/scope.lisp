(define outer-1 () (inner))

(define outer-2 ()
  (define inner () (print "INSIDE"))
  (outer-1)
)

(define flip (f)
  (lambda (x y) (f y x)))

(print ((flip -) 1 2))
