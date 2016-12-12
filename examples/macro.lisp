(program
  (defvar when (macro (cond &rest body)
                      (backquote (if (interpolate cond) (seq (unfold body))))))

  (when True (print-ln "MEOW") (print-ln "MEOW2"))
  ; becomes (if Tru
)
