;; Return T if item is a member of set
;; Return NIL if item is not a member of set
;; The type of set is list
;; Examples:
;;  (set-member '(1 2) 1) => T
;;  (set-memebr '(1 2) 3) => NIL
(defun set-member (set item)
    (loop for i in set
        when (= i item)
        ;; do (format t "T")
        do (progn (format t "T") (return))
        end))

    ;; (format t "NIL"))

(set-member '(1 2 3) 2)