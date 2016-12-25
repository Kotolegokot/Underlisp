(import-module "stdlib/list.lisp")

(define print-string (str)
  (map put-char str))

(define print-string-ln (str)
  (print-string str)
  (put-char #newline))
