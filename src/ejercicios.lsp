(defpackage "EJERCICIOS")

;;1.1 Definir una función que calcule el valor de:
;; F = 1/ sqrt (ax2 + bx2 + c)

(defun f.1.1 (a b c)
  (lambda (x) (l-et ((x2 (expt x 2)))
           (/ 1 (sqrt
                 (+ (* a x2)
                    (* b x2)
                    c))))))

;;1.2 Definir una función que devuelva la longitud de una
;;circunferencia, dando como parámetro el radio R de la misma siendo

(defun length-circ (r) (* 2 pi r))

;;1.3 Definir una función que pase de grados centígrados a grados
;;Fahrenheit, sabiendo que: F = (C + 40) x 1.8 - 40

(defun celsius-to-farenheit (c) (- (* (+ c 40) 1.8) 40))

;;1.4 Definir una función que, dados tres argumentos numéricos,
;;devuelva cuál es el mediano, utilizando MAX y MIN.

(defun mediano (x y z)
  (let* ((min (min x y z))
         (max (max x y z))
         (medp (lambda (x)
                    (and(not (= x min))
                        (not (= x max)) x))))
    (or (funcall medp x) (funcall medp y) z)))

(defun mediano-2 (x y z)
  (let* ((min&max (list (min x y z)
                        (max x y z)))
         (medp (lambda (x) (and (not (member x min&max)) x))))
    (or (funcall medp x) (funcall medp y) z)))

(defun medianos (&rest all)
  (let* ((min (apply #'min all))
         (max (apply #'max all))
         (minmaxp (lambda (n) (or (= n min) (= n max)))))
    (remove-if minmaxp all)))

;;1.5 Definir un predicado que dados A, B y C como argumentos devuelva
;;    T si B2 - 4AC es menor que cero.

(defun ejercicio.1.5 (a b c) (> 0 (- (expt b 2) (* 4 a c))))

;;1.6 Definir un predicado que devuelva T si alguno de sus dos
;;primeros argumentos es menor que el tercero y mayor que el cuarto.

(defun ejercicio.1.6 (a b c d)
  (and (or (< a c) (< b c ))
       (or (> a d) (> b d))))

;;1.7 Definir una función que calcule la entropía de un suceso
;;aleatorio que representa k modos de realización de probabilidades
;;P1, P2, P3, …, Pk, cuyo valor viene dado por la expresión:

(defun sum (lst) (apply #'+ lst))

(defun take (lst i)
  (when (plusp i) 
    (cons (car lst) (take (cdr lst) (1- pos)))))

(defun entropia (i)
  (let* ((f (lambda (p) (* p (log p)))))
    (lambda (lst) (sum (mapcar f (take lst i))))))

;;1.8 Asignar a la variable X1 la lista (COCHE MOTO TREN) y a la
;;variable X2 la lista (EDUARDO PEDRO ANTONIO).

(defvar x1 '(COCHE MOTO TREN))
(defvar x2 '(EDUARDO PEDRO ANTONIO))

;;a) Concatenar las dos listas y calcular la longitud de la lista
;;resultante.
(length (append x1 x2))
;;b) Construir una lista cuyos elementos sean los elementos finales de
;;X1 y X2.
(append (last x1) (last x2))
;;c) A partir de X1 y X2, construir las listas:
;;(TREN ANTONIO)
(append (last x1) (last x2))
;;((TREN) (ANTONIO))
(list (last x1) (last x2))
;;((TREN) ANTONIO)
(list (last x1) (caddr x2))
;;d) Concatenar X1 con el inverso de X2 y asignar el resultado a X3.
(setq x3 (append x1 (reverse x2)))

;;1.9 Definir una función que tenga por argumento una lista y devuelva
;;el tercer elemento de dicha lista.

(defun tercero (lst) (caddr lst))

;;1.10 Definir una función que tenga por argumento una lista y
;;devuelva otra lista con el primer y último elemento de la lista.

(defun first&last (lst) (list (car lst) (car (last lst))))

;;1.11 Definir un predicado con tres argumentos: un átomo y dos
;;listas. El predicado debe devolver T si el átomo pertenece a las dos
;;listas.
(defun member-of-2 (a lst1 lst2)
  ((and (member a lst1) (member a lst2))))
  
(defun member-of-all (a &rest lsts)
  (funcall (macro-function 'and)
           (mapcar (lambda (lst) (member a lst)) lsts) nil))

;;1.12 Definir ROTAIZQ, un procedimiento que recibe una lista como
;;argumento y devuelve otra en la que el primer elemento pasa a ser el
;;último y todos los demás ocupan una posición más a la izquierda.

(defun rotaizq (lst) (append (cdr lst) (list (car lst))))

;;Definir también ROTADCHA que realiza la operación inversa.

(defun rotadcha (lst) (append (last lst) (butlast lst)))

;;1.13 Un palíndromo es una lista que tiene la misma secuencia de
;;elementos cuando se lee de izquierda a derecha que cuando se
;;hace de derecha a izquierda. Definir un función PALINDROMO
;;que tome una lista como argumento y devuelva su palíndromo.
;;Definir también PALINDROMOP, un predicado que comprueba si
;;la lista que se pasa como argumento es un palíndromo.

(defun revertir (lst)
  (let ((fst (list (car lst)))
        (rst (cdr lst)))
    (if rst (append (revertir rst) fst)
      fst)))

(defun palindromo (lst)
  (append lst (revertir (butlast lst)))) 

(defun palindromop (lst)
  (if lst
      (and (equal (car lst) (car (last lst)))
           (palindromop (cdr (butlast lst))))
    t))

;;1.14 Definir una función que dados tres números X, Y y Z,
;;devuelva una lista con los números ordenados por orden creciente.

(defun ordernar-nums (&rest nums) (sort nums '<))

;;1.15 Definir una función que tomando como argumentos una lista
;;y un elemento, devuelva T si el elemento aparece más de una vez
;;en la lista.

(defun ocurrencias (lst a)
  (length (remove-if-not (lambda (x) (equal x a)) lst)))

(defun ocurrencias>1 (lst a)
  (> (ocurrencias lst a) 1))

;;1.16 Definir una función que devuelva el número de átomos que
;;hay en una lista situados a la izquierda de un átomo determinado
;;de dicha lista.

(defun take-while (pred lst)
  (let ((fst (car lst)))
       (when (funcall pred fst) 
         (cons fst (take-while pred (cdr lst))))))

(defun ocurrencias-a-mi-izq (lst center elem)
  (ocurrencias (take-while
                (lambda (x) (not (equal x center))) lst) elem))

;;1.17 Definir una función que añada un elemento a una lista en
;;caso de que aquel no se encuentre en ésta.

(defun add-if-not-member (elem lst)
  (when (not (member elem lst)) (cons elem lst)))

;;1.18 Definir la función CLIMA que, recibiendo como parámetro
;;la temperatura en un recinto, devuelva:
;;a) HELADO si la temperatura es menor de 0 grados.
;;b) FRIO si está entre 0 y 10.
;;c) CALIDO si está entre 10 y 20.
;;d) SOFOCANTE si está entre 20 y 30.
;;e) ABRASIVO si es mayor de 30 grados.

(defun clima (c)
  (cond
   ((< c 0) 'helado)
   ((< c 10) 'frio)
   ((< c 20) 'calido)
   ((< c 30) 'sofocante)
   (:else 'abrasivo)))

;;1.19 Definir un predicado que tome tres argumentos: día, mes y año,
;;y devuelva T si es una fecha válida.

(defun fecha-validap (d m a)
  (and (> m 0) (<= m 12)
       (let ((max))
         )))
