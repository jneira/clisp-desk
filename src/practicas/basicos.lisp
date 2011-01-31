(defpackage "PRACTICAS")

;;1.1 Definir una función que calcule la entropía de un suceso
;;aleatorio que representa k modos de realización de probabilidades
;;P1, P2, P3, …, Pk, cuyo valor viene dado por la expresión:

(defun sum (lst) (apply #'+ lst))

(defun take (lst i)
  (when (plusp i) 
    (cons (car lst) (take (cdr lst) (1- pos)))))

(defun entropia (i)
  (let* ((f (lambda (p) (* p (log p)))))
    (lambda (lst) (sum (mapcar f (take lst i))))))

;;1.2 Definir una función que dados tres números X, Y y Z,
;;devuelva una lista con los números ordenados por orden creciente.

(defun ordernar-nums (&rest nums) (sort nums '<))

;;1.3 Definir una función que devuelva verdadero (cualquier valor
;;distinto de NIL) o falso (NIL) dependiendo de si el menor de sus
;;argumentos sea par o impar. Resolverlo para dos argumentos y
;;para un número variable de éstos.

(defun menor-es-par.2 (x y)
  (evenp (min x y)))

(defun menor-es-par (&rest lst)
  (evenp (apply #'min lst)))

;;1.4 Definir una función que concatene un número variable de listas
;;(algunas pueden ser listas vacías) y átomos, comprobando qué
;;argumentos son listas y cuáles son átomos. En el resultado de dicha
;;concatenación se deben excluir las listas vacías.

(defun concat (&rest xs)
  (let ((f (car xs))
        (r (cdr xs)))
    (if  f
        (cons (car f)
              (apply #'concat (cons (cdr f) r)))
      (when r (apply #'concat r)))))

;;1.5 Definir las funciones SUST1 y SUST2 que reciban como
;;argumentos una lista de asociación y una expresión simbólica L.
;;SUST1 debe sustituir en L los primeros elementos de las parejas
;;de la lista de asociación por sus correspondientes segundos
;;elementos; esta sustitución la hará de forma secuencial (una
;;sustitución puede influir en el resultado de otra sustitución
;;anterior). SUST2 opera de forma análoga pero todas las
;;sustituciones en la lista L deben realizarse simultáneamente (no
;;teniendo ninguna influencia el resultado de una sustitución en las
;;sustituciones que se realicen posteriormente).
;;Ejemplo:
;;> (SUST1 ‘( (A B) (C D) (E F) (B K) (D L) ) ‘(A C E B D M))
;;(K L F K L M)
;;> (SUST2 ‘( (A B) (C D) (E F) (B K) (D L) ) ‘(A C E B D M))
;;(B D F K L M)

(defun get-val (a-lst key)
  (let ((res (cadr (assoc key a-lst))))
    (or res key)))

(defun get-val-seq (a-lst key) 
  (if a-lst
    (get-val-seq (cdr a-lst) (get-val a-lst key))
    key))

(defun sust1 (a-lst lst)
  (mapcar (lambda (i) (get-val-seq a-lst i)) lst))

(defun sust2 (a-lst lst)
  (mapcar (lambda (i) (get-val a-lst i)) lst))

;;1.6 Definir una función recursiva AGRUPAR que reciba dos
;;argumentos, compruebe cuál de ellos es un átomo y cuál una lista,
;;y a continuación introduzca el átomo junto a los átomos iguales
;;que hubiera en la lista o al final de la misma, en el caso de no
;;encontrar semejantes.
;;86 APÉNDICE: EJERCICIOS DE PROGRAMACIÓN
;;Por ejemplo:
;;> (AGRUPAR ‘(A A A B B B C C C) ‘B)
;;(A A A B B B B C C C )

(defun agrupar (x y)
  (typecase x
    (null (list y))
    (atom (and (listp y) (agrupar y x)))
    (cons (let ((h (car x))
                (tl  (cdr x)))
            (if (equal h y) (cons y x)
              (cons h (agrupar tl y)))))
    (otherwise nil)))

;;1.33 Definir una función APLANAR que reciba como argumento
;;una expresión simbólica y elimine todos los paréntesis que
;;aparezcan en esa expresión, devolviendo como resultado una lista
;;con todos los átomos que aparezcan en el argumento.

;;Ejemplo:
;;> (APLANAR ‘( (1 2 3) (9 (2 3 4) ) ( ( ( ( 3 4 ( 7 ) ) ) ) ) ) )
;;( 1 2 3 9 2 3 4 3 4 7 )

(defun aplanar (x)
  (if (consp x)
      (apply #'append (mapcar #'aplanar x))
    (list x)))

;;1.8Definir una macro MI-IF que reciba tres argumentos, siendo el
;;tercero opcional; si el primero es cierto devuelve el segundo, si no
;;devuelve el tercero o NIL si éste no existiera.

(defmacro mi-if (pred body &optional else)
  `(cond (,pred ,body)
         (:else ,else)))

;;1.9Definir una macro MI-DO que tenga exactamente la misma
;;funcionalidad que la macro DO, pero que además de devolver el valor
;;correspondiente cuando se cumpla la condición de finalización,
;;devuelva un segundo valor que indique el número de iteraciones que
;;se han realizado. No se deben utilizar las primitivas DO, DO*,
;;DOLIST, DOTIMES.


(defmacro iterar (steps (finp res) &body body)
  `(labels ((main ,(mapcar #'car steps)
                  (if ,finp ,res
                    (progn ,body
                           (main ,@(apply #'append
                                          (mapcar #'last steps)))))))
     (main ,@(mapcar #'cadr steps))))

(defmacro mi-do (steps (finp res) &body body)
  (let ((c (gensym "counter")))
    `(iterar ,(cons `(,c 0 (1+ ,c)) steps)
             (,finp (values ,res ,c))
             ,body)))

;;1.10Definir una matriz de 10 filas y 20 columnas, rellenarla,
;;asignando a cada elemento el valor de la suma de su fila más su
;;columna y obtener una lista de salida con los elementos de la
;;diagonal.

(defvar m (make-array '(10 20)))

(prog (res)
      (dotimes (i 10)
        (dotimes (j 20)
          (setf (aref m i j) (+ i j))
           (when (= i j)
             (setf res (cons (+ i j) res)))))
      (return (reverse res)))

;;2.1Un móvil es un tipo de escultura abstracta construida por
;;elementos que pueden tener un movimiento relativo unos respecto a
;;otros. Puede definirse un tipo particularmente simple de móvil de
;;forma recursiva como, o bien un objeto suspendido en el aire, o bien
;;una barra con un submóvil colgando de cada extremo. Si se asume que
;;cada barra está suspendida de su punto medio, puede representarse un
;;móvil como un árbol binario. Los objetos suspendidos ser
;;representarán como números que corresponderán al peso de cada
;;objeto. Los móviles más complejos se representarán como listas de
;;tres elementos el primero de ellos será un número igual al peso de
;;la barra, y los otros dos representan submóviles unidos a los
;;extremos de la barra.

;;Un móvil debe ser balanceado, es decir, los dos submóviles de los
;;extremos de la barra deben tener el mismo peso. definir MOVILP, una
;;función que determina si un móvil está o no balanceado. Devolverá
;;NIL si no lo está y el peso total del móvil en caso contrario.

;;> (MOVILP '(6 (4 (2 1 1) 4) (2 5 (1 2 2))))
;;30

(defun movilp (nodo)
  (if (consp nodo)
      (let ((izq (movilp (cadr nodo)))
            (der (movilp (caddr nodo))))
        (when (and izq der (= izq der))
          (+ (car nodo) izq der)))
    nodo))

