(defpackage proyecto
  (:use common-lisp))
(in-package proyecto)

;; El conocimiento sobre un campo determinado puede expresarse, por
;; una parte, mediante una serie de hechos o evidencias que se
;; observan; y por otro lado, en forma de reglas que deducen qué
;; sucede cuando se dan una serie de condiciones. Un hecho puede
;; representarse por medio de una lista de átomos. Todos ellos se
;; reunirán dentro de otra lista.

;; Por ejemplo,

;; (defvar *hechos*
;;   '((Pablo es hijo de Juan)
;;     (Pablo es marido de Adela)
;;     (Jaime es hijo de Pablo)
;;     (Pablo es hermano de Javier)))

;; También pueden verse las reglas en forma de listas. Un formato posible sería:
;; (REGLA <nombre>
;;        (SI
;;         <condición1>
;;         <condición2>
;;         .........)
;;        (ENTONCES
;;         <conclusión1>
;;         <conclusión2>
;;         ..........)
;;Por ejemplo,
;; (defvar r1
;;   '(regla abuelos
;;           (si ((+ hijo) es hijo de (+ padre))
;;               ((< padre) es hijo de (+ abuelo)))
;;           (entonces
;;            ((< hijo) es nieto de (+ abuelo))
;;            ((< abuelo) es abuelo de (+ hijo)))))

;; Los sistemas que utilizan encadenamiento hacia delante tratan de
;; comprobar los antecedentes de las reglas en base a los hechos
;; existentes para así deducir o aplicar las conclusiones. El objetivo
;; de este ejercicio es construir una estructura de control que maneje
;; un sistema de este tipo.

;; Las variables de patrón permiten que cada antecedente de una regla
;; se equipare con varios hechos diferentes. En el caso de la regla
;; R1, al aplicar el procedimiento COMPARAR al primer antecedente de
;; la regla como patrón con cada uno de los hechos:

;; (PABLO ES HIJO DE JUAN) y
;; (JAIME ES HIJO DE PABLO)

;; con el patrón ((+ HIJO) ES HIJO DE (+ PADRE)), obteniéndose las
;; siguientes asignaciones de las variables de patrón:

;; ((HIJO (PABLO)) (PADRE (JUAN))) y
;; ((HIJO (JAIME)) (PADRE (PABLO)))

(defun es-variable? (token syms)
  (and (listp token)
       (member t (mapcar (lambda (s) (eq s (car token))) syms))
       (cadr token)))

(defun comparar (antecedente)
  (loop for hecho in *hechos*
        for coincidencia =
        (loop for hToken in hecho
              and aToken in antecedente 
              for var = (es-variable? aToken '(+))
              if var collect (list var (list hToken))
              unless (or var (eq hToken aToken))
              return nil)
        when coincidencia collect it))

;;(comparar '((+ hijo) es hijo de (+ padre)))
;;(((HIJO (PABLO)) (PADRE (JUAN))) ((HIJO (JAIME)) (PADRE (PABLO))))

;; El siguiente antecedente ha de comprobarse teniendo en cuenta cada
;; una de las equiparaciones obtenidas para el antecedente anterior. En
;; este caso, sólose podrá equiparar el hecho

;; (PABLO ES HIJO DE JUAN)
;; cuando se considera
;; ((HIJO (JAIME)) (PADRE (PABLO)))
;; obteniéndose
;; ((HIJO (JAIME)) (PADRE (PABLO))(ABUELO (JUAN)))

;; En caso de que hubiera más antecedentes, se realizaría este proceso
;; con todos ellos. Definir un procedimiento COMPROBAR-HECHOS que tome
;; dos argumentos: un patrón que corresponde a un antecedente de la
;; regla y una lista que será una de las equiparaciones de las
;; variables del patrón obtenida a partir de los antecedentes
;; anteriores. Devolverá las nuevas equiparaciones junto a las
;; antiguas. Ejemplo,

;; > (COMPROBAR-HECHOS '((+ HIJO) ES HIJO DE (+ PADRE))NIL)

;; (((HIJO(PABLO))(PADRE(JUAN)))(HIJO(JAIME))(PADRE(PABLO))))

;; > (COMPROBAR-HECHOS '((< PADRE) ES HIJO DE (+ ABUELO))
;;                     '((HIJO (JAIME))(PADRE (PABLO))))

;; (((HIJO(JAIME))(PADRE(PABLO))(ABUELO(JUAN))))

;; Se aconseja que este procedimiento sea iterativo, ya que la lista
;; de hechos podría ser demasiado grande y no soportase la
;; recursividad.

(defun sustituir-var (var dato syms) 
  (let ((name (es-variable? var syms)))
    (and  (eq name (car dato)) (caadr dato))))

(defun sustituir-vars (patron datos syms)
  (loop for token in patron
        for var = (loop for d in datos 
                        when (sustituir-var token d syms)
                        return it)
        collect (or var token)))

(defun comprobar-hechos (patron
                         &optional datos-prev)
  (let* ((p (if datos-prev
                (sustituir-vars patron datos-prev '(<))
              patron))
         (res (comparar p)))
    (when res (if datos-prev
                  (mapcar (lambda (r) (append datos-prev r))
                          res)
                res))))

;; Construir un procedimiento FILTRAR-EQUIPARACIONES que reciba como
;; argumentos un patrón y las equiparaciones y que devuelva la nueva
;; lista de equiparaciones. Este procedimiento llamará al
;; anteriormente descrito COMPROBARHECHOS.

;; Por ejemplo:
;; > (FILTRAR-EQUIPARACIONES '((+ HIJO) ES HIJO DE (+ PADRE))¢
;; (NIL))
;; (((HIJO(PABLO))(PADRE(JUAN)))(HIJO(JAIME))(PADRE(PABLO))))
;; > (FILTRAR-EQUIPARACIONES '((< PADRE) ES HIJO DE (+
;; ABUELO))
;; '(((HIJO(PABLO))(PADRE(JUAN)))(HIJO(JAIME))(PADRE(PABLO)))))
;; ((HIJO(JAIME))(PADRE(PABLO))(ABUELO(JUAN)))

(defun equiparacion-correcta (equip)
  (reduce (lambda (x y)
            (and x (not (member-if (lambda (z)
                                     (or
                                      (and (equal (car z) (car y))
                                           (not (equal (cdr z) (cdr y))))
                                      (and (not (equal (car z) (car y)))
                                           (equal (cdr z) (cdr y))))) equip))))
         equip :initial-value t))


(defun filtrar-equs (equips)
  (remove-if-not #'equiparacion-correcta equips))

(defun filtrar-equiparaciones (patron prevs)
  (if prevs
      (loop for d in prevs
            for n = (filtrar-equs
                     (comprobar-hechos patron d))
            when n
            append it)
    (comprobar-hechos patron)))

;; Para terminar con la comprobación de los antecedentes debe
;; definirse el procedimiento COMPROBAR-ANTECEDENTE, de forma que tome
;; como argumento una lista con los antecedentes de una
;; regla. Recursivamente, debe construir una lista con todas las
;; equiparaciones de los hechos con los antecedentes de la regla.Por
;; ejemplo:

;; > (COMPROBAR-ANTECEDENTE
;; '(((+ HIJO) ES HIJO DE (+ PADRE))
;; '((< PADRE) ES HIJO DE (+ ABUELO))))
;; (((HIJO(JAIME))(PADRE(PABLO))(ABUELO(JUAN))))

(defun comprobar-antecedentes (ants &optional prevs)
  (if ants
      (let ((eqs (filtrar-equiparaciones
                  (car ants) prevs)))
        (comprobar-antecedentes (cdr ants) eqs))
    prevs))

;; Una vez se han comprobado las condiciones de la regla (parte
;; izquierda), han de aplicarse las acciones correspondientes (parte
;; derecha) si se han cumplido todas las condiciones, o lo que es lo
;; mismo, si la lista de equiparaciones no está vacía.

;; Definir un procedimiento EJECUTAR-ACCIONES que para cada una de las
;; equiparaciones obtenidas para los antecedentes ejecute las
;; acciones. Para ello, recorrerá iterativamente la lista de acciones
;; reemplazando las variables de patrón de la acción por los valores
;; que toma en la equiparación. Los argumentos de este procedimiento
;; serán: el nombre de la regla, los antecedentes, las acciones y las
;; equiparaciones encontradas. Devolverá una lista con las acciones
;; realizadas. Este procedimiento llamará a los siguientes:


;; -REEMPLAZAR-VARIABLES que, tomando como argumento una lista de
;; patrones y otra lista con equiparaciones, reemplaza las variables
;; de los patrones por los valores especificados en la lista de
;; equiparaciones.

(defun reemplazar-variables (patrones equs)
  (loop for p in patrones
        append (mapcar
                 (lambda (e) (sustituir-vars p e  '(+ <)))
                 equs)))

;;(reemplazar-variables '(((< HIJO) ES NIETO DE (+ ABUELO)))
;;                      '(((HIJO(JAIME)) (PADRE(PABLO)) (ABUELO(JUAN)))))

;; -RECORDAR introduce en la lista general de hechos (una variable que
;; contendrá todos los hechos, tanto los iniciales como los que vayan
;; adquiriéndose según vayan ejecutándose las reglas aplicables) las
;; nuevas acciones obtenidas a partir de las reglas.

(defun recordar (hechos)
  (loop for h in hechos
        unless (member h *hechos* :test #'equal)
        do (setf *hechos* (cons h *hechos*))
        and collect h))

;; Sólo introducirá en la lista de hechos aquellos que no estuvieran
;; ya. Devolverá NIL si no ha introducido ninguno y algo diferente de
;; NIL si lo hiciera.

(defun ejecutar-acciones (acciones equips)
  (recordar (reemplazar-variables acciones equips)))


;; A continuación, deberá definirse el procedimiento USAR-REGLA
;; que,recibiendo como argumento una regla, realiza todas las acciones
;; correspondientes,habiendo comprobado el antecedente sobre la base
;; de los hechos existentes.

(defun usar-regla (regla)
  (let* ((antecedentes (cdaddr regla))
         (acciones (cdr (cadddr regla)))
         (equs (comprobar-antecedentes
                antecedentes))
         ( nuevos (ejecutar-acciones
                  acciones equs)))
    (when nuevos
      (pprint (format nil "Regla ~s: Hechos ~s añadidos."
                      (cadr regla) nuevos)))
    nuevos)) 

;; Para culminar el proceso se tendrá que definir el procedimiento
;; ENCADENAADELANTE que se encargará de probar todas las reglas
;; disponibles. Por claridad conviene que este procedimiento informe
;; cada vez que consigue aplicar una determinada regla. Finalmente
;; devolverá, cuando no haya más reglas aplicables, la nueva lista de
;; hechos. Para realizar este ejercicio se aconseja definir
;; previamente dos variables globales que contengan respectivamente la
;; lista de hechos inicial (BHi)y la lista de reglas.


(defvar *reglas* nil)
(defvar *hechos-iniciales* nil)

(defun encadenar-adelante (reglas)
  (loop for r in reglas
        do (print (format nil "Regla ~s: Inicio"
                          (cadr r)))
        (usar-regla r)))

;; Se pide igualmente aumentar el conjunto de hechos y de reglas con
;; el fin de comprobar el funcionamiento correcto del
;; encadenamiento. Como ejemplo de aplicación de este sistema (también
;; denominado motor de inferencia hacia adelante) puede utilizarse el
;; siguiente caso sobre una jerarquía de relaciones familiares.

;; En primer lugar, se requiere definir una estructura LISP PERSONA
;; que contenga la siguiente información: nombre, apellido, cónyuge,
;; padres, hijos y si está viva o no. Definir instancias de la
;; estructura para representar la familia de la siguiente
;; figura. También se pide definir funciones que obtengan: padres,
;; abuelos y tíos de una determinada persona.

(defstruct persona nombre apellido conyuge padres hijos viva)

(setq juan-rodriguez (make-persona :nombre 'juan
                                   :apellido 'rodriguez
                                   :conyuge 'ana-lopez
                                   :hijos '(elena-rodriguez
                                            eva-rodriguez)
                                   :viva  t))

(setq ana-lopez (make-persona :nombre 'ana
                              :apellido 'lopez
                              :conyuge 'juan-rodriguez
                              :hijos (persona-hijos juan-rodriguez)
                              :viva  t))

(setq jose-perez (make-persona :nombre 'jose
                               :apellido 'perez
                               :conyuge 'elena-rodriguez
                               :hijos '(jaime-perez belen-perez
                                                    alba-perez)
                               :viva  t))

(setq elena-rodriguez (make-persona :nombre 'elena
                                    :apellido 'rodriguez
                                    :conyuge 'jose-perez
                                    :hijos (persona-hijos jose-perez)
                                    :padres '(juan-rodriguez ana-lopez)))

(setq eva-rodriguez (make-persona :nombre 'eva
                              :apellido 'rodriguez
                              :padres '(juan-rodriguez ana-lopez)
                              :viva  t))

(setq jaime-perez (make-persona :nombre 'jaime
                              :apellido 'perez
                              :padres '(jose-perez elena-rodriguez)
                              :viva  t))

(setq belen-perez (make-persona :nombre 'belen
                              :apellido 'perez
                              :padres '(jose-perez elena-rodriguez)
                              :viva  t))

(setq alba-perez (make-persona :nombre 'alba
                              :apellido 'perez
                              :padres '(jose-perez elena-rodriguez)
                              :viva  t))

(defun padres (p)
  (mapcar #'symbol-value (persona-padres p)))

(defun abuelos (p)
  (mapcar #'padres (padres p)))

(defun tios (p)
  (remove-if
   (lambda (p) (member p (padres (symbol-value p))))
   (mapcan (lambda (a)
             (when a (persona-hijos(car a))))
           (abuelos p))))

(setq familia '(juan-rodriguez
                ana-lopez
                jose-perez
                elena-rodriguez
                eva-rodriguez
                jaime-perez
                belen-perez
                alba-perez))

(defun hecho-hijo (hijo padre)
  (list hijo 'es 'hijo 'de padre))

(defun hecho-casado (conyuge1 conyuge2)
  (list conyuge1 'esta 'casado 'con conyuge2))

(defun hecho-vivo (p)
  (list p 'esta 'vivo))

(defun hecho-muerto (p)
  (list p 'esta 'muerto))

(defun init-hechos-familia (familia)
  (loop for m in familia
        for ms = (symbol-value m)
        for hijos = (persona-hijos ms)
        for conyuge = (persona-conyuge ms )
        when (consp hijos)
        append (mapcar (lambda (h) (hecho-hijo h m)) hijos)
        when conyuge
        collect (hecho-casado m conyuge)
        if (persona-viva ms)
        collect (hecho-vivo m)
        else collect (hecho-muerto m)))

(defun init-hechos ()
  (setf *hechos-iniciales*
        (init-hechos-familia familia))
  (setf *hechos* *hechos-iniciales*))

(defvar regla-padres
  '(regla padres
          (si
           ((+ hijo) es hijo de (+ padre)))
          (entonces
           ((< padre) es padre de (+ hijo)))))

(defvar regla-abuelos
  '(regla abuelos
          (si
           ((+ hijo) es hijo de (+ padre))
           ((< padre) es hijo de (+ abuelo)))
          (entonces
           ((< hijo) es nieto de (+ abuelo))
           ((< abuelo) es abuelo de (+ hijo)))))

(defvar regla-tios
  '(regla tios
          (si ((+ hijo) es hijo de (+ padre))
              ((< padre) es hijo de (+ abuelo))
              ((+ tio) es hijo de (< abuelo)))
          (entonces
           ((< hijo) es sobrino de (+ tio))
           ((< tio) es tio de (+ hijo)))))

(defvar reglas-familia (list regla-padres regla-abuelos regla-tios))

(progn (init-hechos) (encadenar-adelante reglas-familia))

(defvar regla-herencia1
  '(regla regla-herencia1
          (si
           ((+ conyuge) esta vivo)
           ((< conyuge) esta casado con (+ persona)))
          (entonces
           ((< conyuge) es heredero de  (+ persona)))))

(defvar regla-herencia2
  '(regla regla-herencia2
          (si
           ((+ hijo) esta vivo)
           ((< hijo) es hijo de (+ persona)))
          (entonces
           ((< hijo) es heredero de  (+ persona)))))

;; Para que esta regla ofrezca resultados hay que matar a ana-lopez
;; y a eva rodriguez ya que asi juan-rodriguez tiene al conyuge y
;; todos sus hijos muertos y sus nietos son sus herederos
;;(setf (persona-viva ana-lopez) nil)
;;(setf (persona-viva eva-rodriguez) nil)
;;(init-hechos)

(defvar regla-herencia3
  '(regla regla-herencia3
          (si
           ((+ conyuge) esta muerto)
           ((< conyuge) esta casado con (+ persona))
           ((+ hijo) esta muerto)
           ((< hijo) es hijo de (+ persona))
           ((+ nieto) es hijo de (< hijo) ))
          (entonces
           ((< nieto) es heredero de (+ persona)))))


(defvar regla-herencia4
  '(regla regla-herencia4
          (si
           ((+ conyuge) esta muerto)
           ((< conyuge) esta casado con (+ persona))
           ((+ hijo) esta muerto)
           ((< hijo) es hijo de (+ persona))
           ((+ padre) esta muerto)
           ((< persona) es hijo de (+ padre))
           ((+ heredero) es heredero de (< padre)))
          (entonces
           ((< heredero) es heredero de (+ persona)))))

(defvar reglas-herencia (list regla-herencia1 regla-herencia2
                              regla-herencia3 regla-herencia4))

(progn (init-hechos) (encadenar-adelante reglas-herencia))
