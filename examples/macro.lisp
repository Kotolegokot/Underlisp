(program
  (defvar when (macro (cond &rest body)
                      (backquote (if (interpolate cond) (seq (unfold body))))))
  (defvar unless (macro (cond &rest body)
                        (backquote (if (interpolate cond) (interpolate (list)) (seq (unfold body))))))

  (when True (print-ln "MEOW") (print-ln "MEOW2"))
  ; becomes (if True (seq (print-ln "MEOW") (print-ln "MEOW2")))
  (unless False (print-ln "MEOW3") (print-ln "MEOW4"))
  ; becomes (if False (list) (seq (print-ln "MEOW3") (print-ln "MEOW4")))
)
