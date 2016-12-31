(import-module "stdlib/string.lisp")

(defun string->dec (str)
  (setfun string->dec^ (str)
    (if (null str)
	0
      (+ (digit->dec (head str))
	 (* 10 (string->dec^ (tail str))))))
  
  (if (null str)
      (error (format "bad argument: '~s'" str))
    (case (head str)
    	  (#- (neg (string->dec^ (reverse (tail str)))))
    	  (#+ (string->dec^ (reverse (tail str))))
    	  ((string->dec^ (reverse str))))))
