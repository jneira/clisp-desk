(defpackage examenes)

;; Examen-2011-Febrero-1
;; Ejercicio 1
;; '((a) (b c))
;; ((a b)(c d))

;; (mueve 'a 'b '((a)(b c))
;; ((a b c))
;; (mueve 'a 'a '((a)(c b)))
;; NIL
;; mueve ('a 'b '((b c)(a d)))
;; '((a b c)(d))

(defun encimap (bloque bloques)
  (member bloque (mapcar #'car bloques)))

;;(defun encimap (bloque bloques)
;;  (assoc bloque bloques))

(defun desapilar (bloque bloques)
  (reduce (lambda (acc next)
            (append acc (remove bloque next)))
         bloques :initial-value ()))

(defun desapilar.filter (bloque bloques)
  (remove-if #'null
   (mapcar (lambda (next) (remove bloque next))
           bloques)))

(defun desapilar.dolist (bloque bloques)
  (setq result ())
  (dolist (next bloques result)
    (setq result
          (append result
              (remove bloque next)))))

(defun desapilar.do (bloque bloques)
  (do ((nxt bloques (cdr nxt))
       (acc () (append acc (remove bloque (car nxt)))))
      ((null nxt) acc)))

(defun desapilar.for (bloque bloques)
  (loop for next in bloques
        when (remove bloque next)
        collect it))

(defun apilar (superior base bloques)
  (reduce
   (lambda (acc next)
     (append acc
             (list (if (eq base (car next))
                       (cons superior next) next))))
   bloques :initial-value ()))

(defun apilar.for (up base blocks)
  (loop for next in blocks
        collect
        (if (eq base (car next))
            (cons up next)) next))

(defun mueve (b1 b2 bs)
  (when (and (encimap b1 bs)
             (encimap b2 bs))
    (apilar b1 b2 (desapilar b1 bs))))

;; Ejercicio 2

(defun crear-lista-propiedad (nombre &rest pairs)
  (setf (symbol-plist nombre) (apply #'append pairs)))

(defun crear-lista-propiedad.dolist (nombre &rest pairs)
  (dolist (p pairs (symbol-plist nombre))
    (setf (get nombre (car p)) (cadr p))))

(defun crear-lista-propiedad.for (nombre &rest pairs)
  (loop for (key value) in pairs
        do (setf (get nombre key) value)))

(defun leer-lista-propiedad (n p) (get n p))

;; Examen Septiembre 2010-1

;; Problema 1
;; (intercalar '(1 3 5) '(2 4 6))
;; (1 2 3 4 5 6)
;; (intercalar '(1 1) '(2 3 3 3 3))
;; (1 2 1 2 3 3 3)

(defun intercalar.rec (x y)
  (append
   (when x (list (car x)))
   (when y (list (car y)))
   (when (or x y)
     (intercalar.rec (cdr x) (cdr y)))))

(defun intercalar.map (x y)
  (let* ((sorted  (sort (list x y) #'> :key #'length))
         (rest (subseq (car sorted) (length (cadr sorted)))))
    (append (apply #'append
                   (mapcar #'list x y)) rest)))

(defun intercalar.loop (xs ys)
  (loop for xt on xs
        and yt on ys
        when (car xt) collect it into l
        when (car yt) collect it into l
        finally (return (append l xt yt))))

(defmacro intercalar2 (x y &key (orden 1))
  (case orden
    (1 `(intercalar.rec ,x ,y))
    (2 `(intercalar.rec ,y ,x))))

;; Problema 2

;; Determinar num posiciones coincidentes dos arrays

(defun celdas-coincidentes (a1 a2)
  (length
   (loop for i below (min (array-dimension a1 0)
                           (array-dimension a2 0))
         append
         (loop  for j below (min (array-dimension a1 1)
                                 (array-dimension a2 1))
                when (eq (aref a1 i j) (aref a2 i j))
                collect (list i j)))))

(defun celdas-coincidentes.do (a1 a2)
  (setq cont 0)
  (do ((i 0 (1+ i)))
      ((= i (min (array-dimension a1 0)
                 (array-dimension a2 0))) cont)
    (do* ((j 0 (1+ j)))
        ((= j (min (array-dimension a1 1)
                   (array-dimension a2 1))))
      (print (list i j (aref a1 i j)))
      (when (eq (aref a1 i j)
                (aref a2 i j))
        (incf cont)))))

(setq a1 (make-array '(2 2) :initial-contents '((1 2) (3 a))))
(setq a2 (make-array '(2 3) :initial-contents '((1 2 c) (3 b d))))

(celdas-coincidentes a1 a2)

;; Examen Septiembre 2010-1

;; > (grabar '(juan 15 181) '(pedro 25 185) '(antonio 75 165))
;; ((nombre . juan) (edad . 15) (altura . 181))
;; ((nombre . pedro) (edad . 25) (altura . 185))
;; ((nombre . antonio) (edad . 75) (altura . 165))

;; > (leer)
;; ((juan pedro antonio) (15 25 75) (181 185 165))

(defun grabar (&rest records)
  (with-open-file 
   (stream "personas.dat"
           :direction :output
           :if-exists :supersede)
   (dolist (r records)
     (destructuring-bind (name edad altura) r
         (prin1 (list (cons 'name name)
                      (cons 'edad edad)
                      (cons 'altura altura)) stream)
         (terpri stream)))))

(defun leer ()
  (let ((res (with-open-file
              (fich "personas.dat"
                    :direction :input)
              (do* ((nxt () (read-from-string
                             (read-line fich nil ":EOF")))
                    (res () (cons nxt res)))
                  ((eq :EOF nxt) (remove :EOF res))))))
    (apply #'mapcar #'list (mapcar (lambda (lst) (mapcar #'cdr lst)) res))))

;; Problema 2

;;a)
(defun ciclop (movs) (eq (car movs) (car (last movs))))
(defun vuelta (movs) (reverse (butlast movs)))
(defun comunp (movs1 movs2)
  (some (lambda (mv) (member mv (cdr movs2))) movs1))

;; Febrero 2010-1

;; (completar '(1 4))
;; (1 2 3 4)
;; (completar '(3 2 0))
;; (3 2 1 0)

(defun range (x y)
  (if (= x y)
      (list y)
    (cons x (range (+ x (if (> x y) -1 1 )) y))))

(defun completar (num)
  (reduce (lambda (acc nxt)
            (append acc (cdr (range (car (last acc)) nxt)))) 
          (cdr num) :initial-value (list (car num))))

;; Problema 2 . Trasponer

(defmacro transpose (m)
  `(let* ((dims (array-dimensions ,m))
         (new (make-array dims)))
    (loop for i below (car dims)
          do (loop for j below (cadr dims)
                   do (setf (aref new j i) (aref ,m i j)))
          finally (return new))))

(defun transpose.do (m)
  (let* ((dims (array-dimensions m))
        (new (make-array dims)))
   (dotimes (i (array-dimension m 0) new)
     (dotimes (j (array-dimension m 1))
       (setf (aref new j i) (aref m i j))))))
