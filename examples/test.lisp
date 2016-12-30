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

(setfun show-usage ()
  (print-string-ln "Usage: prog <sequence>"))

(set args (get-args))
(if (/= (length args) 1)
    (show-usage)
  (seq
   (set sequence (head args))

   (if (dfa.test dfa-1 sequence)
       (print-string-ln "SUCCESS")
     (print-string-ln "PIZDETZ"))))
