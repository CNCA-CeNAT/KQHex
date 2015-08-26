;; *****************************************************
;; hex.lisp:
;; Representación del tablero y movimientos en Hex.
;;
;; Juan Carlos Saborio Morales,
;; Programa de Posgrado en Ciencias Cognoscitivas, UCR y
;; Colaboratorio Nacional de Computación Avanzada, CeNAT.
;; 2015.
;; *****************************************************

;; ********** Funciones exclusivas de HEX **************
(load "funcs.lisp")
(load "seleccion.lisp")

;; Se define la lista *letras* para utilizar símbolos y no caracteres.
;; Se agrega "relleno" a los lados para compensar el offset de las sumas.
(defparameter	*letras*	'(A A B C D E F G H I J K L M N O P Q R S T U V W X Y Z Z))
;; Dimensiones cuadradas del tablero.  Eg. 8x8, 9x9, 11x11
(defparameter	*tablero*	8)

;; Lista de nodos "muertos", para identificar estados finales.  Utilizado por función interna.
(defparameter *nodosMuertos* nil)

;;******** Funciones dependientes del Dominio *********
;; Leer una lista de estrategias iniciales de HEX y guardarlas en la red especificada.
;; NOTA: Las estrategias pueden ser un conjunto básico o cargar una red antes creada.
(defun estrategiasInit()
	(load "s_hex.txt") ;; Asumir que carga una lista con nombre 's_hex'
	S_Hex ;; determinar forma de liberar símbolo S_Hex de la memoria
)

;; Funcion para determinar si s es un estado final.
;; Llama a la implementación para Hex.
(defun terminalD(s)
	(terminal_hex s)
)

;; Función para determinar el ganador
;; Llama a la implementación para Hex.
(defun ganadorD(P)
	(ganador_hex P)
)
;;******** Fin Funciones dependientes del Dominio *********

;; Seleccionar una posición aleatoria en todo el tablero POSIBLE según estado (evitando posiciones ocupadas).
(defun seleccionarAleatorio(estado)
	(if	(= (list-length (aplanar estado)) (* *tablero* *tablero*))
		(format t "Tablero lleno!")
		(let	( (pos (append (list (nth (+ (random *tablero*) 1) *letras*)) (list (+ (random *tablero*) 1))))
			)
			(if	(contains (aplanar estado) pos)
				(seleccionarAleatorio estado)
				pos
			)
		)
	)
)

;; Generar conjunto de movimientos posibles a partir de una posición en el tablero
;; En este caso, 9 celdas vecinas a 'posicion'.
;; TODO: incrementar el rango de movimientos posibles incluyendo "puentes" de Hex (vecindario + 1)
(defun movimientosPosibles(estado posicion)
	(if	(null posicion)
		(list (seleccionarAleatorio estado))
		(let	(	(valLetra	(val (car posicion)))
				(valNum	 	(cadr posicion))
			)		
			(eliminarRepetidos (eliminarNoValidos (aplanar estado) (genmovs posicion (- valLetra 1) (- valLetra 1) (+ valLetra 1) (- valNum 1) (+ valNum 1) nil)))
		)
	)
)

;; Eliminar movimientos fuera del tablero, e.g. (., -1).
(defun eliminarNoValidos(listaEstado movimientos)
	(cond	((null movimientos)	nil)
		((contains listaEstado (car movimientos)) (eliminarNoValidos listaEstado (cdr movimientos))) ;; Si el movimiento ya fue jugado, ignorarlo.
		((or (< (cadar movimientos) 1) (> (cadar movimientos) *tablero*) (> (val (caar movimientos)) *tablero*))	(eliminarNoValidos listaEstado (cdr movimientos))) ;; Si el movimiento se sale de los límites, eliminarlo.
		(t	(append (list (car movimientos)) (eliminarNoValidos listaEstado (cdr movimientos)))) ;; Si el movimiento es válido, agregarlo a la lista y recurrir.
	)
)

;; eliminarRepetidos: eliminar movimientos ya existentes en una lista de candidatos o aquellos con valor 0.
(defun eliminarRepetidos(movimientos)
	(eliminar_R movimientos nil)
)

(defun eliminar_R(movimientos resultado)
	(cond 	((null movimientos)	resultado)
		((contains resultado (car movimientos))	(eliminar_R (cdr movimientos) resultado))
		(t					(eliminar_R (cdr movimientos) (append resultado (list (car movimientos)))))	
	)
)

;; Función recursiva para generar los vecinos de posicion.
(defun genmovs(posicion letraI letraA letraF numI numF lista)
	(let	( (pos (append (list (nth letraA *letras*)) (list numI))) )
		(cond	((and (= letraA letraF) (= numI numF))	(append lista (list pos)))
			((= letraA letraF)	(genmovs posicion letraI letraI letraF (+ numI 1) numF (append lista (list pos))))
			((iguales pos posicion)	(genmovs posicion letraI (+ letraA 1) letraF numI numF lista)) ;; Saltarse el centro (posicion original).
			(t			(genmovs posicion letraI (+ letraA 1) letraF numI numF (append lista (list pos))))
		)	
	)
)

;; ************ Evaluar estados finales **************
;; Terminal_hex: determinar si el estado actual es un estado final de Hex.
;; Se evalua si la lista I o bien la lista II son listas finales.
(defun terminal_hex(estado)
	(or (evaluarTerm (car estado)) (evaluarTerm (transponerE (cadr estado))))
)

;; Función term. Determinar si una lista de movimientos es un estado final.
;; Los estados son siempre en formato de I, para II se debe transponer su lista.
(defun term(lista)
	;;Idea: recorrer la lista ordenada y comprobar hipótesis: dado movimiento inicial, existe un sucesor
	;;Determinar criterios según el turno, jugador I ó II
	(cond	(*turnoI*	(evaluarTerm lista)) ;; Jugador I, evaluar O-E
		(t		(evaluarTerm (transponerE lista))) ;; Jugador II, evaluar N-S
	)
)


;; Evaluar la lista de movimientos para determinar si es un estado final, siguiendo la dirección indicada.
;; Si 'dir' = 'O', se debe satisfacer una línea de la forma (A,.), (B,.), ...(Z,.)
;; Sino, (., 1), (., 2), ... (.,N).
;; Los límites en ambos casos están dados por la variable *tablero*

;; Condición 1: Lista de largo mayor o igual que *tablero*
;; Condición 2: Deben estar presentes todas las letras

(defun evaluarTerm(lista)
	(setq *nodosMuertos* nil)
	(let	((m (movsIniciales lista)))
		(evaluarTermA (eliminarL lista m) m) ;;extraer los pares que inicien con A
	)
)

;; Generar la sublista que contiene solamente los elementos iniciales en una secuencia de movimientos.
;; Para Hex, corresponde a los pares (A,.)
(defun movsIniciales(lista)
	(cond	((null lista) nil)
		((eq (caar lista) (nth 1 *letras*))	(append (list (car lista)) (movsIniciales (cdr lista))))
		(t   (movsIniciales (cdr lista)))
	)
)

;; Realizar la iteración para comprobar la existencia de rutas ganadoras en 'lista', utilizando como partida sólo los
;; movimientos iniciales correctos.  Esto evita que existan condiciones ganadoras por subrutas.
(defun evaluarTermA(lista iniciales)
	(cond	((null iniciales) nil)
		(t (if (not (evaluarTerm_ lista (car iniciales) (nth *tablero* *letras*))) (evaluarTermA lista (cdr iniciales)) t)) ;; probar para cada posicion en lista
	)
)

;; por definición mov pertenece a lista.
(defun evaluarTerm_(lista mov posFinal)
	(cond	((eq (car mov) posFinal)	T) ;; si s es la posicion final, es estado ganador
		((contains *nodosMuertos* mov) nil) ;; Si el nodo actual está muerto, no continuar
		(t	(iterarVecinos (eliminarE lista mov) mov (interseccion lista (vecinos mov)) posFinal)) ;; iterar con cada vecinos
	)
)

;; Iterar sobre lista 'v' de vecinos de mov
(defun iterarVecinos(lista mov v final)
	(cond	((null v) (agregarNodoMuerto mov))  ;; si se agotan los vecinos, no hay ruta posible y el estado no es final
		((contains *nodosMuertos* (car v)) (iterarVecinos lista mov (cdr v) final)) ;; si el vecino actual está muerto, saltarlo
		;;((contains lista (car v))	(if (not (evaluarTerm_ lista (car v) final)) (iterarVecinos lista (cdr v) final))) ;;si la ruta contiene un vecino, probar con ese vecino
		(t	(if (not (evaluarTerm_ lista (car v) final)) (iterarVecinos lista mov (cdr v) final) t)) ;; sino, iterar con el siguiente vecino.
	)
)

;; Los nodos muertos son aquellos cuyos sucesores no conducen a una posición final.
(defun agregarNodoMuerto(n)
	(if (contains *nodosMuertos* n) nil (setq *nodosMuertos* (append *nodosMuertos* (list n))))
	nil
)
;; *************** FIN estados finales **************

;; Listar los 6 vecinos inmediatos de una celda dada.
(defun vecinos(celda)
	(eliminarRepetidos (eliminarNoValidos nil (eliminarE (listarVecinos celda) celda)))
)

(defun listarVecinos(celda)
	(let	((a	(val (car celda)))
		 (v	(cadr celda))
		)
		(list	(list (nth (- a 1) *letras*) v)
			(list (nth a *letras*) (- v 1))
			(list (nth (+ a 1) *letras*) (- v 1))
			(list (nth (+ a 1) *letras*) v)
			(list (nth a *letras*) (+ v 1))
			(list (nth (- a 1) *letras*) (+ v 1))
		)
	)
)

; Ordenar estrategia por letra.
(defun ordenar(lista)
	(mergesort lista)
)

;; Ganador_hex: determinar el ganador actual si el estado es terminal.
(defun ganador_hex(estado)
	(cond	((evaluarTerm (car estado))		 1)
		((evaluarTerm (transponerE (cadr estado)))2)
		(t nil)
	)
)

;; ************** Transponer ****************
;; Si la estrategia/estado corresponde al jugador II, entonces transponerla antes de
;; comparar o buscar.
;; NOTA: El problema de transponer radica en la búsqueda, ya que se debe conocer las posiciones reales del tablero.
;; NOTA: Se puede guardar las estrategias completas de ambos jugadores si se modifica las funciones de evaluación para determinar,
;; como estrategia válida, aquella que permita llegar a ganar.
;; NOTA: si se transponen I y II Y además se intercambian, el problema se vuelve simétrico.
(defun transP_turno(s)
	(if *turnoI*	s	(transp s))
)

;; Transponer un ESTADO o PROBLEMA completo, i.e. ((I) (II)) --> ((transp II) (transp I))
(defun transP(s)
;;	(if *turnoI* (car s) (transponerE (cadr s)))
	(append (list (transponerE (cadr s))) (list (transponerE (car s))))
)

(defun transponerE_turno(estrategia)
	(if *turnoI*	estrategia (append (transponerE (butlast estrategia)) (list (etiqueta estrategia))))
)

;; Transponer una estrategia o lista de movimientos.
(defun transponerE(estrategia)
	(transponerE_r estrategia nil)
)

(defun transponerE_r(estrategia trans)
	(cond	((null estrategia) trans)
		(t	(let	( (a	(val (caar estrategia)))
				  (v	(cadar estrategia))
				)
				(transponerE_r (cdr estrategia) (append trans (list (append (list (nth v *letras*)) (list a)))))
			)
		)
	
	)
)

;; ************ Fin Transponer **************