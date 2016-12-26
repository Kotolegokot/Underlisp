(import-module "stdlib/list.lisp")

(defun newline ()
  (put-char #newline))

(defun print-string (str)
  (map put-char str))

(defun print-string-ln (str)
  (print-string str)
  (newline))

(defun write-ln (s-expr)
  (write s-expr)
  (newline))
