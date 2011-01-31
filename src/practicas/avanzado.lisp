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

;;2.2El juego de la vida es un juego de simulación que se desarrolla
;;en una cuadrícula, de modo que en cada casilla pueda haber un
;;organismo. Cada casilla se puede encontrar ocupada o vacía.
;;Dos organismos se consideran vecinos si sus casillas son contiguas
;;en sentido horizontal, vertical o diagonal. Por tanto, cada casilla
;;tiene ocho casillas vecinas.

;; Las reglas del juego son:
;; - En cada casilla vacía nace un nuevo organismo si dicha casilla
;;   tiene exactamente tres vecinos.3
;; - Una casilla ocupada que tenga cero o un vecino muere por aislamiento.
;; - Una casilla ocupada con cuatro o más vecinos muere por superpoblación.
;; - Una casilla ocupada con dos o tres vecinos sobrevive.
;; Todos los nacimientos y muertes ocurren simultáneamente y la
;; aplicación de las leyes anteriores produce una nueva generación.

;; El juego continúa hasta que suceda uno de los siguientes hechos:
;; - La generación actual es igual que la generación inicial.
;; - La generación actual es igual que dos generaciones anteriores.
;; - Se ha alcanzado el número máximo de generaciones prefijado.

(defun get-neighbours (grid x y)
  (let ((result 0) (xo (- x 1))
        (yo (- y 1)))
    (dotimes (i 3)
      (dotimes (j 3)
        (let ((dx (+ xo i))
              (dy (+ yo j)))
          (when (and (array-in-bounds-p grid dx dy)
                     (not (and (= dx x) (= dy y))))
            (setf result (+ result (aref grid dx dy)))))))
    result))

(defun apply-rules (grid x y)
  (let* ((n (aref grid x y))
         (ns (get-neighbours grid x y)))
   (or (born n ns)
       (survive n ns) 0)))

(defun alivep (n) (> n 0))
(defun deadp (n) (not (alivep n)))

(defun born (n ns)
  (and (deadp n) (= ns 3) 1))

(defun survive (n ns)
  (and (alivep n)
       (or (= ns 3) (= ns 2)) 1))

(defun make-empty-grid (x y)
  (make-array (list x y) :initial-element 0))

(defun init-generation (grid coords)
  (let ((g (copy-array grid)))
    (dolist (c coords)
      (setf (aref g (car c) (cadr c)) 1))
    g))

(defun next-generation (grid)
  (let* ((g (copy-array grid))
         (dims (array-dimensions g))
         (endx (car dims))
         (endy (cadr dims)))
    (dotimes (x endx )
      (dotimes (y endy)
        (setf (aref g x y)
              (apply-rules grid x y))))
    g))

(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :element-type
                 (array-element-type array)
                 :displaced-to array)
     dims)))

(defun game-of-life (x y init  maxgen)
  (let* ((init-grid (init-generation
                     (make-empty-grid x y) init))
         (first-gen (next-generation init-grid)))
    (do ((g first-gen (next-generation g))
         (n 1 (1+ n))
         (hist (list init-grid)
               (cons g hist)))
        ((or (eq g init-grid)
             (eq g (cadr hist))
             (= n maxgen))
         hist))))

(defvar block 
  '((1 1) (1 2) (2 1) (2 2)))

(defvar beehive
  '((1 2) (1 3)
    (2 1) (2 4)
    (3 2) (3 3)))

(defvar blinker '((1 1) (1 2) (1 3)))

(defvar glitter
  '((1 2)
    (2 3)
    (3 1) (3 2) (3 3)))

(game-of-life 10 10 block 5)
(game-of-life 10 10 beehive 5)
(game-of-life 10 10 blinker 5)
(game-of-life 10 10 glitter 5)

;;2.3Escribir REINAS, una función que devuelva la solución al problema
;;de las ocho reinas. Se trata de situar a ocho reinas en un tablero
;;de ajedrez, de manera que ninguna de ellas sea atacada por otra, es
;;decir, no puede haber dos reinas en la misma fila, columna o
;;diagonal.

(defun find-queen (arr p d)
  (destructuring-bind ((px py) (dx dy)) (list p d)
    (do ((x px (+ x dx))
         (y py (+ y dy)))
        ((not (array-in-bounds-p arr x y)))
      (when (= (aref arr x y) 1) (return t)))))

(defun queen-in-row (board row)
  (find-queen board (list row 0) '(0 1)))

(defun queen-in-col (board col)
  (find-queen board (list 0 col) '(1 0) ))

(defun queen-in-diags (board x y)
  (member t (mapcar (lambda (p)
                    (find-queen board (list x y) p))
                  '((1 1) (-1 -1) (1 -1) (-1 1)))))
  
(defun queen-in-range (board x y)
  (or (queen-in-row board x)
      (queen-in-col board y)
      (queen-in-diags board x y)))

(defun backtracking (pred explore node)
  (if (funcall pred node) (list node)
    (mapcan (lambda (n) (backtracking pred explore n))
            (funcall explore node))))

(defun count-queens (board)
  (loop for i below (array-total-size board)
        for box = (row-major-aref board i)
        count (> box 0)))

(defun solutionp (board)
  (and board (= 8 (count-queens board))))

(defun put-queens (board)
  (loop for i below 8
        when (not (queen-in-row board i))  
        return
        (loop for j below 8
              for b = (copy-array board)
              when (not (queen-in-range board i j))
               do (setf (aref b i j) 1)
               and collect b)))

(defun 8-queens ()
  (backtracking #'solutionp #'put-queens board))

(defvar board (make-array '(8 8) :initial-element 0))


