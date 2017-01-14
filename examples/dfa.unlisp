;;; DFA = (start state, accepting states, delta function)
;;; delta = state, symbol -> state

;; dfa : start-state, accepting-states, delta -> dfa
(defun dfa (start-state accepting-states delta)
  (list start-state accepting-states delta))

(define dfa.start-state      head)
(define dfa.accepting-states [nth 1])
(define dfa.delta            [nth 2])

(defun dfa.test (dfa sequence)
  (let ((last-state (foldl (dfa.delta dfa)
                           (dfa.start-state dfa)
                           sequence)))
    (elem last-state (dfa.accepting-states dfa))))
