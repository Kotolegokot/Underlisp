(defmacro when (cond &rest body)
  `(if ~cond (seq @body)))

(defmacro unless (ond &rest ody)
  `(when (not ~ond) @ody))

(unless (< 7 6) (print-ln 12) (print-ln 13))

;;(defvar cond '(< 7 6))
;;(defvar body '((print-ln 12) (print-ln 13)))

;;(print-ln `(when (not ~cond) @body))
