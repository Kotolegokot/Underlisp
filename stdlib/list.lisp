(import-module "stdlib/ord.lisp")

(defun reverse (xs)
  (if (null xs)
      '()
    (append (reverse (tail xs))
            (list (head xs)))))

(defun length (xs)
  (foldl (lambda (acc _) (+ acc 1)) 0 xs))

(defun prepend (x xs)
  (append (list x) xs))

(defun init (xs)
  (if (null xs)
      (error "init: empty list")
    (if (= 1 (length xs))
            '()
      (prepend (head xs) (init (tail xs))))))

(defun last (xs)
  (if (null xs)
      (error "last: empty list")
    (if (= 1 (length xs))
        xs
      (last (tail xs)))))

(defun nth (n xs)
  (switch ((null xs)       (error "nth: empty list"))
          ((>= n (length xs)) (error "nth: out of bounds"))
          ((< n 0)            (error "nth: negative index"))
          (otherwise
           (if (= n 0)
               (head xs)
             (nth (- n 1) (tail xs))))))

(defun map (f xs)
  (if (null xs)
      nil
    (prepend (f (head xs))
             (map f (tail xs)))))

(defun map-nil (f xs)
  (map f xs)
  nil)

(defun foldl (f acc xs)
  (if (null xs)
      acc
    (foldl f (f acc (head xs))
           (tail xs))))

(defun foldr (f acc xs)
  (if (null xs)
      acc
    (foldr f (f (last xs) acc)
           (init xs))))

(defun zip (xs ys)
  (if (or (null xs) (null ys))
      '()
    (prepend (list (head xs) (head ys))
             (zip (tail xs) (tail ys)))))

(defun zip-with (f xs ys)
  (if (or (null xs) (null ys))
      '()
    (prepend (f (head xs) (head ys))
             (zip-with f (tail xs) (tail ys)))))

(defun elem (y xs)
  (foldl (lambda (acc x) (if (= x y) True acc)) False xs))

(defun filter (p xs)
  (if (null xs)
      '()
    (if (p (head xs))
        (prepend (head xs) (filter p (tail xs)))
      (filter p (tail xs)))))

(defun all (p xs)
  (foldl (lambda (acc x) (if (p x) acc False)) True xs))

(defun any (p xs)
  (foldl (lambda (acc x) (if (p x) True acc)) False xs))

(defun find (p xs)
  (if (null xs)
      '()
    (if (p (head xs))
        (head xs)
      (find (p (tail xs))))))

(defun take (n xs)
  (switch ((> n (length xs)) (error "take: out of bounds"))
          ((< n 0)           (error "take: negative number"))
          (otherwise
           (if (= n 0)
               '()
             (prepend (head xs) (take (- n 1) (tail xs)))))))

(defun drop (n xs)
  (switch ((> n (length xs)) (error "drop: out of bounds"))
          ((< n 0)           (error "drop: negative number"))
          (otherwise
           (if (= n 0)
               xs
             (drop (- n 1) (tail xs))))))

(defun cons (a b)
  (if (list? b)
      (prepend a b)
    (list a b)))

(defun find (x xs)
  (switch
   ((null xs)       False)
   ((= x (head xs)) True)
   (otherwise       (find x (tail xs)))))
