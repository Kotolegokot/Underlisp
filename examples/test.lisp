;;(print-ln "OLD_CONTEXT: " (current-context))
;;(print-ln "==================")

(load-module "examples/factorial.lisp")
;;(load-context (context-from-file "examples/factorial.lisp"))

;;(print-ln "FACTORIAL CONTEXT: " (function-context factorial))

;;(print-ln "==================")
;;(print-ln "NEW_CONTEXT: " (current-context))

;;(print-ln (context-from-file "examples/factorial.lisp"))

;; (define f (x) (factorial (+ x 1)))

(print-ln (factorial 3))
(print-ln (factorial 4))
