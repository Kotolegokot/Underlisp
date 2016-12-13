(print-ln (macro-expand defmacro when (cond &rest body) `(if ~cond (seq @body))))
;(print-ln defmacro')
;(print-ln when)


;(defmacro unless (cond &rest body)
;           (backquote (when (not (interpolate cond)) (seq (unfold body)))))

(print-ln (macro-expand when True (print-ln "MEOW") (print-ln "MEOW2")))
; becomes (if True (seq (print-ln "MEOW") (print-ln "MEOW2")))
;(unless False (print-ln "MEOW3") (print-ln "MEOW4"))
; becomes (if False (list) (seq (print-ln "MEOW3") (print-ln "MEOW4")))

