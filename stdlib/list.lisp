(define reverse (xs)
  (if (= 0 (length xs))
      '()
    (append (reverse (tail xs))
	    (list (head xs)))))

(defvar ++ append)

(define empty (xs)
  (= (length xs) 0))

(define prepend (x xs)
  (append (list x) xs))

(define map (f xs)
  (if (empty xs)
      '()
    (prepend (f (head xs))
	     (map f (tail xs)))))

(define foldl (f acc xs)
  (if (empty xs)
      acc
    (foldl f (f acc (head xs))
	   (tail xs))))

(define foldr (f acc xs)
  (if (empty xs)
      acc
    (foldr f (f (last xs) acc)
	   (init xs))))

(define zip-with (f xs ys)
  (if (or (empty xs) (empty ys))
      '()
    (prepend (f (head xs) (head ys))
	     (zip-with f (tail xs) (tail ys)))))
