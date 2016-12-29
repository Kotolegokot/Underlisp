(import-module "stdlib/list.lisp")

(defmacro case (expr &rest pairs)
  (defun handle-pairs (expr-var pairs)
    (if (null pairs)
	nil
      (let ((first (head pairs)))
	(prepend
	 `((= ~expr-var ~(head first))
	   ~(head (tail first)))
	 (handle-pairs expr-var (tail pairs))))))

  (let ((expr-var (gensym)))
    `(let ((~expr-var ~expr))
       (switch @(handle-pairs expr-var pairs)))))
