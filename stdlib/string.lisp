(import-module "stdlib/char.lisp")

(defun string->dec (str)
  (setfun string->dec' (str)
    (if (null str)
	0
      (+ (digit->dec (head str))
	 (* 10 (string->dec' (tail str))))))
  
  (if (null str)
      (error (format "bad argument: '~s'" str))
    (case (head str)
    	  (#- (neg (string->dec' (reverse (tail str)))))
    	  (#+ (string->dec' (reverse (tail str))))
    	  ((string->dec' (reverse str))))))

(defun string->hex (str)
  (setfun string->hex' (str)
     (if (null str)
	 0
       (+ (digit->hex (head str))
	  (* 16 (string->hex' (tail str))))))

  (if (null str)
      (error (format "bad argument: '~s'" str))
    (case (head str)
	  (#- (neg (string->hex' (reverse (tail str)))))
	  (#+ (string->hex' (reverse (tail str))))
	  ((string->hex' (reverse str))))))
