(program
  (defvar when (macro (cond &rest body)
                      (backquote (if (interpolate cond) (seq (unfold body))))))

  (when True (print-ln "MEOW"))
)
