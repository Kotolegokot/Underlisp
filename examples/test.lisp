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

(with-args (list "101")
  (set args (get-args))
  (if (/= (length args) 1)
      (show-usage)
    (seq
     (set sequence (head args))
     (if (dfa.test dfa-1 sequence)
	 (print-string-ln "SUCCESS")
       (print-string-ln "PIZDETZ")))))

(let ({a (lambda (n) (if (<= n 1)
			 "A"
		       (b (pred n))))}
      {b (lambda (n) (if (<= n 1)
			 "B"
		       (a (pred n))))})
  
  (print-format "~s~%" (b 5)))
