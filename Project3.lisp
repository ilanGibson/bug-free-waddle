;; Return T if item is a member of set
;; Return NIL if item is not a member of set
;; The type of set is list
;; Examples:
;;  (set-member '(1 2) 1) => T
;;  (set-memebr '(1 2) 3) => NIL
(defun set-member (set item)
    (cond
        ((null set) NIL)
        ((eq (car set) item) T)
        (t (set-member (cdr set) item))))

(format t "set-member: ~a~%" (set-member '(1 2 3) 4))