(load-module "stdlib/list.lisp")
(print-ln (macro-expand apply or (map (bind + 1) '(1 2 3))))
(print-ln (macro-expand apply or '(1 2 3)))

