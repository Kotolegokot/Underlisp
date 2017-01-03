(import-module "stdlib/char.lisp")

(defun string->dec (str)
  (setfun string->dec' (str)
    (if (empty? str)
	0
      (+ (digit->dec (head str))
	 (* 10 (string->dec' (tail str))))))
  
  (if (empty? str)
      (error (format "bad argument: '~s'" str))
    (case (head str)
    	  (#- (neg (string->dec' (reverse (tail str)))))
    	  (#+ (string->dec' (reverse (tail str))))
    	  ((string->dec' (reverse str))))))

(defun string->hex (str)
  (setfun string->hex' (str)
     (if (empty? str)
	 0
       (+ (digit->hex (head str))
	  (* 16 (string->hex' (tail str))))))

  (if (empty? str)
      (error (format "bad argument: '~s'" str))
    (case (head str)
	  (#- (neg (string->hex' (reverse (tail str)))))
	  (#+ (string->hex' (reverse (tail str))))

	  ((string->hex' (reverse str))))))

(defun dec->string (i)
  (if (int? i)
      (->string i)
    (error "int expected")))

(defun hex->string (i)
  (defun hex->string' (i)
    (if (zero? i)
	""
      (let ((div-mod' (div-mod i 16))
	    (div' (head div-mod'))
	    (mod' (head (tail div-mod'))))
	(prepend (hex->digit mod')
		 (hex->string' div')))))

  (if (not (int? i))
      (error "int expected")
    (let ((result (reverse (hex->string' (abs i)))))
      (cond
       ((empty? result) "0")
       ((neg? i) (prepend #- result))
       result))))
