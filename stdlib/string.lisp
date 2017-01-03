(import-module "stdlib/char.lisp")

(defun string->dec (str)
  (contract str string?)
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
  (contract str string?)
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
  (contract i int?)
  (->string i))

(defun hex->string (i)
  (contract i int?)
  (defun hex->string' (i)
    (if (zero? i)
	""
      (let ((div-mod' (div-mod i 16))
	    (div' (head div-mod'))
	    (mod' (head (tail div-mod'))))
	(prepend (hex->digit mod')
		 (hex->string' div')))))

  (let ((result (reverse (hex->string' (abs i)))))
    (cond
     ((empty? result) "0")
     ((neg? i) (prepend #- result))
     (otherwise result))))
