(defun digit->hex (c)
  (let ((dec  (- (char->int c) (char->int #0)))
	(hexl (- (char->int c) (char->int #a)))
	(hexu (- (char->int c) (char->int #A))))
    (cond
     ((in-range 0 9 dec)  dec)
     ((in-range 0 5 hexl) (+ 10 hexl))
     ((in-range 0 5 hexu) (+ 10 hexu))
     (otherwise
      (error (format "not a hexadecimal digit: '~c'" c))))))

(defun hex->digit (i)
  (int->char
   (+ i (cond
	 ((in-range 0   9 i) (char->int #0))
	 ((in-range 10 15 i) (- (char->int #a) 10))
	 (otherwise
	  (error (format "bad argument: '~a'" i)))))))

(defun digit->dec (c)
  (set dec (- (char->int c) (char->int #0)))
  (if (in-range 0 9 dec)
      dec
    (error (format "not a decimal digit: '~c'" c))))

(defun dec->digit (i)
  (int->char
   (+ i (if (in-range 0 9 i)
	    (char->int #0)
	  (error (format "bad argument: '~a'" i))))))
