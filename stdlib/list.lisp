(define reverse (xs)
  (if (null xs)
      '()
    (append (reverse (tail xs))
            (list (head xs)))))

(define length (xs)
  (foldl (lambda (acc _) (+ acc 1)) 0 xs))

(define prepend (x xs)
  (append (list x) xs))

(define init (xs)
  (if (null xs)
      (error "init: empty list")
    (if (= 1 (length xs))
            '()
      (prepend (head xs) (init (tail xs))))))

(define last (xs)
  (if (null xs)
      (error "last: empty list")
    (if (= 1 (length xs))
        xs
      (last (tail xs)))))

(define nth (n xs)
  (switch ((null xs)       (error "nth: empty list"))
          ((>= n (length xs)) (error "nth: out of bounds"))
          ((< n 0)            (error "nth: negative index"))
          (otherwise
           (if (= n 0)
               (head xs)
             (nth (- n 1) (tail xs))))))

(define map (f xs)
  (if (null xs)
      '()
    (prepend (f (head xs))
             (map f (tail xs)))))

(define foldl (f acc xs)
  (if (null xs)
      acc
    (foldl f (f acc (head xs))
           (tail xs))))

(define foldr (f acc xs)
  (if (null xs)
      acc
    (foldr f (f (last xs) acc)
           (init xs))))

(define zip (xs ys)
  (if (or (null xs) (null ys))
      '()
    (prepend (list (head xs) (head ys))
             (zip (tail xs) (tail ys)))))

(define zip-with (f xs ys)
  (if (or (null xs) (null ys))
      '()
    (prepend (f (head xs) (head ys))
             (zip-with f (tail xs) (tail ys)))))

(define elem (y xs)
  (foldl (lambda (acc x) (if (= x y) True acc)) False xs))

(define filter (p xs)
  (if (null xs)
      '()
    (if (p (head xs))
        (prepend (head xs) (filter p (tail xs)))
      (filter p (tail xs)))))

(define all (p xs)
  (foldl (lambda (acc x) (if (p x) acc False)) True xs))

(define any (p xs)
  (foldl (lambda (acc x) (if (p x) True acc)) False xs))

(define find (p xs)
  (if (null xs)
      '()
    (if (p (head xs))
        (head xs)
      (find (p (tail xs))))))

(define take (n xs)
  (switch ((> n (length xs)) (error "take: out of bounds"))
          ((< n 0)              (error "take: negative number"))
          (otherwise
           (if (= n 0)
               '()
             (prepend (head xs) (take (- n 1) (tail xs)))))))

(define drop (n xs)
  (switch ((> n (length xs)) (error "drop: out of bounds"))
          ((< n 0)           (error "drop: negative number"))
          (otherwise
           (if (= n 0)
               xs
             (drop (- n 1) (tail xs))))))

(define cons (a b)
  (if (list? b)
      (prepend a b)
    (list a b)))
