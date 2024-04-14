;; Return T if item is a member of set
;; Return nil if item is not a member of set
;; The type of set is list
;; Examples:
;;  (set-member '(1 2) 1) => T
;;  (set-memebr '(1 2) 3) => nil
(defun set-member (set item)
    (cond
        ((null set) nil)
        ((equal (first set) item) T)
        (T (set-member (cdr set) item))))

;; (format t "set-member: ~a~%" (set-member '(1 2) 1))


;; Return the union of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-union '(1 2) '(2 4)) => '(1 2 4)
(defun set-union (set-1 set-2)
;; run set-member on each first of set-2
    (cond
        ((null set-2) set-1)
        ((null set-1) set-2)
        ((set-member set-1 (first set-2)) (set-union set-1 (cdr set-2)))
        ((not(set-member set-1 (first set-2))) (set-union (cons (first set-2) set-1) (cdr set-2)))))

;; (format t "set-union: ~a~%" (set-union '(1 2) '(2 4)))
;; TODO: set-union is not working correctly. It is returning '(4 1 2) instead of '(1 2 4)


;; Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)
(defun set-intersection (set-1 set-2)
    (cond 
        ((null set-1) nil)
        ((null set-2) nil)
        ((set-member set-2 (first set-1)) (cons (first set-1) (set-intersection (cdr set-1) set-2)))
        ((not(set-member set-2 (first set-1))) (set-intersection (cdr set-1) set-2))))

;; (format t "set-intersection: ~a~%" (set-intersection '(1 2) '(2 4)))


;; Return the difference of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-diff '(1 2) '(2 4)) => '(1)
(defun set-diff (set-1 set-2)
    (cond
        ((null set-1) nil)
        ((null set-2) set-1)
        ((set-member set-2 (first set-1)) (set-diff (cdr set-1) set-2))
        ((not(set-member set-2 (first set-1))) (cons (first set-1) (set-diff (cdr set-1) set-2)))))

;; (format t "set-diff: ~a~%" (set-diff '(1 2) '(2 4)))


;; Return the exclusive or of a and b
;;
;; Examples:
;;  (boolean-xor t nil) => t
;;  (boolean-xor nil nil) => nil
(defun boolean-xor (a b)
    (cond 
        ((and a b) nil)
        ((or (and a b) (or a b)) T)
        ((and (not a) (not b)) nil)))

;; (format t "boolean-xor: ~a~%" (boolean-xor T nil))


;; Return the implication of a and b
;; Examples:
;;  (boolean-implies t nil) => nil
;;  (boolean-implies nil nil) => t
(defun boolean-implies (a b)
    (cond
        ((and a b) T)
        ((and a (not b)) nil)
        ((and (not a) b) T)
        ((and (not a) (not b)) T)))

;; (format t "boolean-implies: ~a~%" (boolean-implies T nil))


;; Return the bi-implication (if and only if) of a and b
;; Examples:
;;  (boolean-iff t nil) => nil
;;  (boolean-iff nil nil) => t
(defun boolean-iff (a b)
    (cond
        ((and a b) T)
        ((and (not a) b) nil)
        ((and a (not b)) nil)
        ((and (not a) (not b)) T)))

;; (format t "boolean-iff: ~a~%" (boolean-iff nil nil))

;; Evaluate a boolean expression.
 ;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
 ;; Examples:
 ;;  (boolean-eval '(and t nil)) => nil
 ;;  (boolean-eval '(and t (or nil t)) => t
;;  (defun boolean-eval (exp)
;;      (format t "exp: ~a~%" exp)

;;      (cond
;;         ((null exp) nil)
;;         ((equal exp T) T)
;;         ((equal (first exp) 'nil) nil)
;;         ((equal (first exp) 'not) (not (boolean-eval (cdr exp))))
;;         ((equal (first exp) 'and) (and (first (cdr exp)) (boolean-eval (first (cdr (cdr exp))))))
;;         ;; ((eq (first exp) 'or) (or (first exp) (boolean-eval (cdr exp))))
;;         ((equal (first exp) 'or) (or (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))


;;         ((equal (first exp) 'xor) (boolean-xor (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))
;;         ((equal (first exp) 'implies) (boolean-implies (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))
;;         ((equal (first exp) 'iff) (boolean-iff (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))
;;         (T (first exp))))

(defun boolean-eval (exp)

    ;; (format t "exp: ~a~%" exp)
    ;; (if (listp exp)
    ;;     (progn
    ;;         (format t "first exp: ~a~%" (first exp))
    ;;         (format t "cdr exp: ~a~%" (cdr exp))
    ;;         (format t "first cdr exp: ~a~%" (first (cdr exp)))
    ;;         (format t "cdr cdr exp: ~a~%" (cdr (cdr exp)))))



    (cond
        ((equal exp T) T) ;; covers case of boolean-eval T
        ((equal exp nil) nil) ;; covers case of boolean-eval nil
        ((equal exp '(T)) T) ;; covers case of boolean-eval '(T)
        ((equal exp '(nil)) nil) ;; covers case of boolean-eval '(nil)

        ((equal (first exp) 'not) (not (boolean-eval (first (cdr exp)))))
        ((equal (first exp) 'or) (or (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))
        ((equal (first exp) 'and) (and (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))
        ((equal (first exp) 'xor) (boolean-xor (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))
        ((equal (first exp) 'implies) (boolean-implies (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))
        ((equal (first exp) 'iff ) (boolean-iff (boolean-eval (first (cdr exp))) (boolean-eval (cdr (cdr exp)))))))


;; (defun boolean-eval (exp)

;;     (format t "exp: ~a~%" (first(cdr exp)))
;;     (not (first exp)))

;; (format t "boolean-eval: ~a~%" (boolean-eval '(or (or (or nil nil) nil) t)))


;; t/nil boolean-eval tests
(assert (equal (boolean-eval T) T) nil "T should be T")
(assert (equal (boolean-eval nil) nil) nil "nil should be nil")

;; not boolean-eval tests
(assert (equal (boolean-eval '(not T)) nil) nil "not T should be nil")
(assert (equal (boolean-eval '(not nil)) T) nil "not nil should be T")
(assert (equal (boolean-eval '(not (not (not (not (not (not (not (not (not T)))))))))) nil) nil "not not not not not not not not not t should be nil")

;; or boolean-eval tests
(assert (equal (boolean-eval '(or T nil)) T) nil "or T nil should be T")
(assert (equal (boolean-eval '(or nil nil)) nil) nil "or nil nil should be nil")
(assert (equal (boolean-eval '(or nil T)) T) nil "or nil T should be T")
(assert (equal (boolean-eval '(or T T)) T) nil "or T T should be T")
(assert (equal (boolean-eval '(or T (or nil (or T (or nil T))))) T) nil "or t (or nil (or t (or nil t))) should be T")
(assert (equal (boolean-eval '(or (or T nil) (or nil (or T (or nil T))))) T) nil "or t (or nil (or t (or nil t))) should be T")


;; ;; and boolean-eval tests
(assert (equal (boolean-eval '(and T nil)) nil) nil "and T nil should be nil")
(assert (equal (boolean-eval '(and nil nil)) nil) nil "and nil nil should be nil")
(assert (equal (boolean-eval '(and nil T)) nil) nil "and nil T should be nil")
(assert (equal (boolean-eval '(and T T)) T) nil "and T T should be T")
(assert (equal (boolean-eval '(and (and nil T) nil)) nil) nil "and (and nil t) t should be nil")
(assert (equal (boolean-eval '((and nil (and T (and nil T))))) nil) nil "and nil (and t (and nil t)) should be nil")
(assert (equal (boolean-eval '(and T (and nil (and T (and nil T))))) nil) nil "and t (and nil (and t (and nil t))) should be nil")

;; ;; xor boolean-eval tests
(assert (equal (boolean-eval '(xor T nil)) T) nil "xor T nil should be T")
(assert (equal (boolean-eval '(xor nil nil)) nil) nil "xor nil nil should be nil")
(assert (equal (boolean-eval '(xor nil T)) T) nil "xor nil T should be T")
(assert (equal (boolean-eval '(xor T T)) nil) nil "xor T T should be nil")

;; ;; implies boolean-eval testss
(assert (equal (boolean-eval '(implies T nil)) nil) nil "implies T nil should be nil")
(assert (equal (boolean-eval '(implies nil nil)) T) nil "implies nil nil should be T")
(assert (equal (boolean-eval '(implies nil T)) T) nil "implies nil T should be T")
(assert (equal (boolean-eval '(implies T T)) T) nil "implies T T should be T")

;; ;; iff boolean-eval tests
(assert (equal (boolean-eval '(iff T nil)) nil) nil "iff T nil should be nil")
(assert (equal (boolean-eval '(iff nil nil)) T) nil "iff nil nil should be T")
(assert (equal (boolean-eval '(iff nil T)) nil) nil "iff nil T should be nil")
(assert (equal (boolean-eval '(iff T T)) T) nil "iff T T should be T")

