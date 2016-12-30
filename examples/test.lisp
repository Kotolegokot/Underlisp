(import-module "stdlib/io.lisp")
(import-module "stdlib/functional.lisp")

;;(write-ln (initial-env))

;; (import-module "examples/dfa.lisp")

;; (define dfa-1 (dfa 0 '(1) (lambda (state symbol)
;;                             (cond
;;                              ((= state 0)
;;                               (cond
;;                                ((= symbol #1) 1)
;;                                ((= symbol #0) 0)))
;;                              ((= state 1)
;;                               (cond
;;                                ((= symbol #1) 1)
;;                                ((= symbol #0) 0)))))))

;; (print-string "Enter sequence: ")
;; (flush)
;; (define str (get-line))

;; (if (dfa.test dfa-1 str)
;;     (print-string-ln "SUCCESS")
;;   (print-string-ln "PIZDETZ"))

(cond
 ((= 1 2)   (print-string-ln "MEOW"))
 (otherwise (print-string-ln "BARK")))
