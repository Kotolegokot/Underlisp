(defun digit->int (c)
  (let ((dec  (abs (- (char->int c) (char->int #0))))
	(hexl (abs (- (char->int c) (char->int #a))))
	(hexu (abs (- (char->int c) (char->int #A)))))
    (cond
     ((<= dec 9)  dec)
     ((<= hexl 5) (+ hexl 10))
     ((<= hexu 5) (+ hexu 10))
     (otherwise   (error (append "not a digit: " (to-string c)))))))

(defun int->digit (i)
  (int->char
   (+ i (cond
	 ((in-range 0   9 i) (char->int #0))
	 ((in-range 10 15 i) (- (char->int #a) 10))
	 (otherwise
	  (error (format "bad argument: ~a" i)))))))
