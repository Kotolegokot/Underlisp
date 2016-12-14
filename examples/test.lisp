(load-module "stdlib/list.lisp")

(defvar a 2)

(switch ((= 0 a) (print-ln 0))
        ((= 1 a) (print-ln 1))
        (otherwise (print-ln "other")))
