;;(import-module "stdlib/io.lisp")
;;(import-module "stdlib/functional.lisp")

(import-module "examples/dfa.lisp")

(set dfa-1
     (dfa 0 '(1) (lambda (state symbol)
		   (case state
			 (0 (case symbol
				  (#0 0)
				  (#1 1)))
			 (1 (case symbol
				  (#0 0)
				  (#1 1)))))))

(print-string "Enter sequence: ")
(flush)
(set str (get-line))

(if (dfa.test dfa-1 str)
    (print-string-ln "SUCCESS")
  (print-string-ln "PIZDETZ"))
