(defpackage examenes)

;; '((a) (b c))
;; ((a b)(c d))

;; (mueve 'a 'b '((a)(b c))
;; ((a b c))
;; (mueve 'a 'a '((a)(c b)))
;; NIL
;; mueve ('a 'b '((b c)(a d)))
;; '((a b c)(d))

(defun mueve-colums (b1 b2 col1 col2j))

(defun mueve (b1 b2 bs)
  (let ((col1 (car bs)) (col2 (cadr bs)))
    (if (and (eq b1 (car col1))
             (eq))
        )))
