(defpackage "EJERCICIOS")

;;1.1 Definir una función que calcule el valor de:
;; F = 1/ sqrt (ax2 + bx2 + c)

(defun f.1.1 (a b c)
  (lambda (x) (let ((x2 (expt x 2)))
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

;;(FECHAP 12 12 1986) => T
;;(FECHAP 12 30 1987) => NIL
;;(FECHAP 31 2 1986) => NIL
;;(FECHAP 31 11 1876) => T

(defun fecha-validap (d m a)
  (and (> m 0) (<= m 12)
       (> d 0)
       (let ((max (cond
                   ((= m 2) 28)
                   ((evenp m) 30)
                   (t 31))))
        (<= d max))))

;;1.20 Definir una función que devuelva cierto (T) si alguno de sus
;;tres argumentos no es divisible por 2.

(defun div2 (&rest nums)
  (some (lambda (x) (/= (rem x 2) 0)) nums))

;;1.21 Definir la función ABSOLUTO que calcula el valor
;;absoluto de su argumento si éste es un número utilizando una
;;estructura condicional.

(defun absoluto (n)
  (when (numberp n)
    (if (>= 0) n (- n))))

;;1.22 Definir la función MINIMO, que devuelve el mínimo de sus
;;tres argumentos numéricos, utilizando una estructura condicional.

(defun minimo (x y z)
  (or (and (< x y) (< x z) x)
      (and (< y x) (< y z) y)
      z))

(defun minimo (x y z)
  (cond
   ((and (< x y) (< x z)) x)
   ((and (< y x) (< y z)) y)
   (t z)))

;;1.23 Definir la función MAXIMO, que devuelve el máximo de
;;sus tres argumentos numéricos, utilizando una estructura
;;condicional.

(defun maximo (x y z)
  (or (and (> x y) (> x z) x)
      (and (> y x) (> y z) y)
      z))

(defun maximo (x y z)
  (cond
   ((and (> x y) (> x z)) x)
   ((and (> y x) (> y z)) y)
   (t z)))

;;1.24 Definir, utilizando una estructura condicional, las funciones
;;lógicas OR y AND para tres argumentos.

(defun and3 (x y z)
  (when x (when y z)))

(defun or3 (x y z)
  (if x t (if y t z)))

;;1.25 Definir de forma iterativa la función SUMA de dos números.

(defun suma (x y)
  (do ((total x (1+ total))
       (cont y (1- cont)))
      ((zerop cont) total)))

;;1.26 Definir de forma iterativa la EXPONENCIACION de dos números

(defun exponent (x y)
  (do ((total x (* total x))
       (exp (1- y) (1- exp)))
      ((zerop exp) total)))

;;1.27 Definir la función FACTORIAL de forma recursiva e
;;iterativa sabiendo que el factorial de N es 1 si N es 0 y en otro
;;caso será N veces el factorial de N-1

(defun fact.rec (n)
  (if (= n 1) 1
    (* n (fact.rec (1- n)))))

(defun fact.iter (n)
  (do* ((num n (1- num))
        (acc n (* acc num)))
      ((= 1 num) acc)))

;;1.28 Redefinir de forma recursiva e iterativa las funciones
;;primitivas MEMBER, REVERSE y LENGTH.

(defun member.rec (a lst)
  (when lst
    (or (and (equal a (car lst)) a)
        (member.rec a (cdr lst)))))

(defun member.iter (a lst)
  (dolist (i lst) (when (= a i) (return i))))

(defun reverse.rec (lst)
  (let ((fst (list (car lst)))
        (rst (cdr lst)))
    (if rst (append (revertir rst) fst)
      fst)))

(defun reverse.iter (lst)
  (dolist (i lst acc)
    (cons i acc)))

(defun length.rec (lst)
  (if lst (1+ (length.rec (cdr lst))) 0))

(defun length.iter (lst)
  (do ((l lst (cdr lst))
       (acc 0 (+1 acc))
       ((null l) acc))))

;;1.29 Definir una función que determine la profundidad de una
;;lista (número máximo de paréntesis que hay que extraer para
;;obtener un determinado elemento de la misma).

(defun depth (lst)
  (typecase lst
    (cons  (1+ (apply 'max (mapcar 'depth lst))))
    (null 1)
    (otherwise 0)))

;;1.30 Definir las funciones SUST1 y SUST2 que reciban como
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

;;1.31 Definir una función recursiva AGRUPAR que reciba dos
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

;;1.32 Construir de forma recursiva e iterativa la función de
;;Fibonacci, sabiendo que:
;;F(0) = 1
;;F(1) = 1
;;F(n) = F(n-1) + F(n-2) si n>1

(defun fibo.rec (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((> n 1) (+ (fibo.rec (- n 1))
                    (fibo.rec (- n 2))))))

(defun fibo.iter (n)
  (do* ((a 1 (+ a b))
        (b 0 a)
       (cont n (1- cont)))
      ((zerop cont) a)))

;;1.33 Definir una función APLANAR que reciba como argumento
;;una expresión simbólica y elimine todos los paréntesis que
;;aparezcan en esa expresión, devolviendo como resultado una lista
;;con todos los átomos que aparezcan en el argumento.

;;Ejemplo:
;;> (APLANAR ‘( (1 2 3) (9 (2 3 4) ) ( ( ( ( 3 4 ( 7 ) ) ) ) ) ) )
;;( 1 2 3 9 2 3 4 3 4 7 )

(defun aplanar (lst)
  (when (consp lst)
    (let ((h (car lst)) (tl (cdr lst)))
      (append  (if (consp h) (aplanar h)
                 (list h))
               (aplanar tl)))))

(defun aplanar2 (x)
  (if (consp x)
      (apply #'append (mapcar #'aplanar2 x))
    (list x)))

;;1.34 Usando la función MAPCAR, definir PRESENTP, un
;;procedimiento que determine si un átomo particular existe en una
;;expresión, teniendo en cuenta los elementos de las listas anidadas.

(defun presentp (a lst)
  (mapcar (lambda (i)
            (if (atom i) (equal a i)
              (presentp a i))) lst))

;;1.35 Definir una función que reciba como argumento una lista de
;;números y devuelva otra lista cuyos elementos sean los cuadrados
;;de los anteriores.

(defun cuadrados (lst) (mapcar (lambda (x) (expt x 2)) lst))

;;1.36 Definir una función que devuelva verdadero (cualquier valor
;;distinto de NIL) o falso (NIL) dependiendo de si el menor de sus
;;argumentos sea par o impar. Resolverlo para dos argumentos y
;;para un número variable de éstos.

(defun menor-es-par.2 (x y)
  (evenp (min x y)))

(defun menor-es-par (&rest lst)
  (evenp (apply #'min lst)))

;;1.37 Definir una función que compruebe si un número variable de
;;listas tienen el mismo número de elementos.

(defun misma-longitud (&rest lsts)
  (apply #'= (mapcar  #'length lsts)))

;;1.38 Definir una función que al aplicarla a un número variable de
;;números cree una lista con todos aquellos números que sean
;;múltiplos de 3.

(defun filtrar-multiplos-3 (&rest nums)
  (remove-if (lambda (n) (/= (mod n 3) 0)) nums))

;;1.39 Definir una función que concatene un número variable de
;;listas, comprobando que los argumentos que recibe son listas y
;;excluyendo todos las listas que estén vacías.

(defun concat.builtin (&rest lsts)
  (if (some #'atom lsts) (print "Existe algun atomo")
    (apply #'append lsts)))

(defun concat (&rest xs)
  (let ((f (car xs))
        (r (cdr xs)))
    (if  f
        (cons (car f)
              (apply #'concat (cons (cdr f) r)))
      (when r (apply #'concat r)))))

;;1.40 Definir la función SUMA que, tomando dos argumentos
;;numéricos devuelva su suma.
;;Definir la función SUMA-VARIOS con un número variable de
;;argumentos, utilizando la función SUMA definida anteriormente.
;;Deberá devolverse NIL si alguno de los argumentos no es
;;numérico.

(defun suma (x y) (+ x y))

(defun suma-varios (&rest nums)
  (reduce #'suma nums))

;;1.41 Definir una función que añada un átomo a una lista que se le
;;pasan como argumentos a la función.
;;
;;Modificar la función anterior de forma que el átomo que se le
;;pasa como argumento sea opcional. En caso de que no se
;;especifique en la llamada el átomo, se añadirá a la lista un átomo
;;cualquiera.
;;
;;Modificar la función anterior de forma que pueda añadirse un
;;número variable de átomos a la lista que se le pasa como
;;argumento.

(defun mycons (lst a) (cons a lst))

(defun mycons.1 (lst &optional (a 'default))
  (cons a lst))

(defun mycons.2 (lst &rest as)
  (append as lst))

;;1.42 Definir una función que tome como argumentos una lista y
;;un número variable de átomos y devuelva una lista con los
;;resultados de aplicar la función MEMBER con cada uno de los
;;átomos y la lista.
;;Modificar la función anterior de forma que devuelva NIL si
;;alguno de los átomos no es miembro de la lista. En caso contrario,
;;el resultado de aplicar MEMBER al último átomo.

(defun members (lst &rest ats)
  (mapcar (lambda (a) (member a lst)) ats))

(defun members.1 (lst &rest ats)
  (reduce (lambda (p a) (when p (member a lst))) ats))

(defun members.2 (lst &rest ats)
  (dolist (a ats res)
    (setf res (member a lst))
    (unless res (return res))))

;;1.43 Definir una macro MI-IF que reciba tres argumentos, siendo
;;el tercero opcional; si el primero es cierto devuelve el segundo, si
;;no devuelve el tercero o NIL si éste no existiera.

(defmacro si (pred body &optional else)
  `(cond (,pred ,body)
         (:else ,else)))

;;1.44 Además de IF, Common LISP incorpora las primitivas
;;WHEN y UNLESS definidas como sigue:
;;(WHEN <cond> <cuerpo>) = (COND ( <cond> <cuerpo>)
;;(UNLESS <cond> <cuerpo>) =
;;(COND ((NOT <cond>) <cuerpo>))
;;Definir las macros MI-WHEN y MI-UNLESS, de forma que
;;realicen las misma funciones que las versiones que incorpora
;;COMMON LISP.

(defmacro cuando (pred body)
  `(cond (,pred ,body)))

(defmacro si-no (pred body)
  `(cond ((not ,pred) ,body)))

;;1.45 Definir una macro MI-DO que tenga exactamente la misma
;;funcionalidad que la macro DO, pero que además de devolver el
;;valor correspondiente cuando se cumpla la condición de
;;finalización, devuelva un segundo valor que indique el número de
;;iteraciones que se han realizado. No se deben utilizar las
;;primitivas DO, DO*, DOLIST, DOTIMES.

(defmacro iterar (steps (finp res) &body body)
  `(labels ((main ,(mapcar #'car steps)
                  (if ,finp ,res
                    (progn ,body
                           (main ,@(apply #'append
                                          (mapcar #'last steps)))))))
     (main ,@(mapcar #'cadr steps))))

(defmacro iterar.con.contador (steps (finp res) &body body)
  (let ((c (gensym "counter")))
    `(iterar ,(cons `(,c 0 (1+ ,c)) steps)
             (,finp (values ,res ,c))
             ,body)))

(defun suma.2 (x y)
  (iterar.con.contador ((total x (1+ total))
       (cont y (1- cont)))
      ((zerop cont) total)
    (print total) (print cont)))

;;1.46 Definir mediante una macro una función iterativa que realice
;;algo (el cuerpo) un número de veces. Suponer que la llamada a la
;;función se hace de la siguiente forma:
;;(DOTIMES ( <var> <cont> <result> ) <cuerpo>)
;;donde var es una variable que va a contar el número de
;;iteraciones que se han realizado, su valor inicial será cero y se irá
;;incrementando hasta que valga el valor dado en cont. Al final,
;;después de haber realizado cont veces el cuerpo se devolverá el
;;valor almacenado en result.

(defmacro hacer-n-veces ((i max &optional res) &body body)
  `(do ((,i 0 (1+ ,i)))
       ((>= ,i ,max) ,res)
     ,@body))

;;1.47 Definir DOWHILE, una macro de dos argumentos que
;;evalúa ambos hasta que el primero es NIL.
;;   (DOWHILE <cond> <cuerpo>)

(defmacro hacer-mientras (pred &body body)
  `(do () (,pred ()) ,@body))

;;1.48 No todos los sistemas de LISP implementan el mecanismo
;;Backquote. Definir BACKQUOTE de forma que tenga el mismo
;;efecto que Backquote y permita manejar de forma apropiada
;;expresiones como COMA y COMA-AT de la forma que se
;;muestra en el siguiente ejemplo:
;;> (BACKQUOTE (A B (LIST ‘C ‘D) (COMA (LIST ‘E ‘F)
;;                               (COMA-AT (LIST ‘G ‘H)))
;;(A B (LIST ‘C ‘D) (E F) G H)

(defmacro backquote (&rest forms)
  (labels ((step (case (car l)
                    ('comma (cadr l))
                    ('comma-at (cadr l))
                    (t (list 'quote l)))))
    (step (car forms))))
