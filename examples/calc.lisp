(program
  (defvar defmacro (macro (name args &rest body)
    (backquote (defvar (interpolate name) (macro (interpolate args) (unfold body))))))

  (defmacro define (name args &rest body)
    (backquote (defvar (interpolate name) (lambda (interpolate args) (unfold body)))))

  (print-ln "Hi! I'm a stupid calculator. I can either multiply or add two numbers or calculate a factorial.")
  (print-ln "Please, type either '+' or '*', and then two numbers, or 'factorial' and then one number")

  (define get-int () (str-to-int (get-line)))
  (define get-float () (str-to-float (get-line)))
  (define print-number (x) (print "Your number is ") (print-ln x))

  ;; factorial
  (define factorial (x) (if (<= x 1) 1 (* x (factorial (- x 1)))))

  (let ((input (get-line)))
    (if (= input "*")
      (let ((n1 (get-float)) (n2 (get-int)) (mult (* n1 n2)))
        (print-number mult)
      )
      (if (= input "+")
        (let ((n1 (get-float)) (n2 (get-int)) (sum (+ n1 n2)))
          (print-number sum)
        )
        (if (= input "factorial")
          (let ((n (get-int)) (f (factorial n)))
            (print-number f)
          )
          (print-ln "Error: you entered neither + or *. Go to hell, bitch.")
        )
      )
    )
  )
)
