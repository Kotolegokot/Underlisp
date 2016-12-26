(import-module "stdlib/list.lisp")

(define newline ()
  (put-char #newline))

(define print-string (str)
  (map put-char str))

(define print-string-ln (str)
  (print-string str)
  (newline))

(define write-ln (s-expr)
  (write s-expr)
  (newline))
