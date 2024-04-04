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
        (T (set-member (cdr set) item))))

;; (format t "set-member: ~a~%" (set-member '(1 2) 1))


;; Return the union of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-union '(1 2) '(2 4)) => '(1 2 4)
(defun set-union (set-1 set-2)
;; run set-member on each car of set-2
    (cond
        ((null set-2) set-1)
        ((null set-1) set-2)
        ((set-member set-1 (car set-2)) (set-union set-1 (cdr set-2)))
        ((not(set-member set-1 (car set-2))) (set-union (cons (car set-2) set-1) (cdr set-2)))))

;; (format t "set-union: ~a~%" (set-union '(1 2) '(2 4)))
;; TODO: set-union is not working correctly. It is returning '(4 1 2) instead of '(1 2 4)


;; Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)
(defun set-intersection (set-1 set-2)
    (cond 
        ((null set-1) NIL)
        ((null set-2) NIL)
        ((set-member set-2 (car set-1)) (cons (car set-1) (set-intersection (cdr set-1) set-2)))
        ((not(set-member set-2 (car set-1))) (set-intersection (cdr set-1) set-2))))

;; (format t "set-intersection: ~a~%" (set-intersection '(1 2) '(2 4)))


;; Return the difference of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-diff '(1 2) '(2 4)) => '(1)
(defun set-diff (set-1 set-2)
    (cond
        ((null set-1) NIL)
        ((null set-2) set-1)
        ((set-member set-2 (car set-1)) (set-diff (cdr set-1) set-2))
        ((not(set-member set-2 (car set-1))) (cons (car set-1) (set-diff (cdr set-1) set-2)))))

;; (format t "set-diff: ~a~%" (set-diff '(1 2) '(2 4)))


;; Return the exclusive or of a and b
;;
;; Examples:
;;  (boolean-xor t nil) => t
;;  (boolean-xor nil nil) => nil
(defun boolean-xor (a b)
    (cond 
        ((and a b) NIL)
        ((or (and a b) (or a b)) T)))

;; (format t "boolean-xor: ~a~%" (boolean-xor t nil))

;; Return the implication of a and b
;; Examples:
;;  (boolean-implies t nil) => nil
;;  (boolean-implies nil nil) => t
;; (defun boolean-implies (a b)
;;     (cond
;;         ((and a b) T)
;;         )
;; )