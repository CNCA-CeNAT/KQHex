;; *****************************************************
;; funcs.lisp:
;; Funciones de relevancia, similitud, distancia y pertenencia para método de selección.
;; Asume la representación de listas de pares (a,v).
;;
;; Juan Carlos Saborio Morales,
;; Programa de Posgrado en Ciencias Cognoscitivas, UCR y
;; Colaboratorio Nacional de Computación Avanzada, CeNAT.
;; 2015.
;; *****************************************************


;; Función de similitud
;; Con promedio lineal
;; sim(P,S) = (1/n) * suma_i { delta(p_i, s_i) }
;; ** Problema: los elementos relevantes (p_i, s_i) no necesariamente están en las mismas posiciones.
;; Para todo p_i in P.  Si p_i in S, +1.

;; P = { I, II }

;; Aclaraciones importantes:
;; Para la selección de estrategias se utiliza el delta entre el enfoque de P y el nodo actual.
;; Para la aproximación de valores q se utiliza la relevancia, suma ponderada de deltas (utilizando enfoque).
;; Para la comparación entre listas se utiliza la similitud, suma ponderada de distancias (no deltas).
;; La implementación actual se deriva de formalizaciones previas, por lo que en este archivo la función de relevancia es "similitud" y la de similitud, "simListas".

;; Relevancia de un nodo respecto a P
(defun similitud(P nodo)
	(let 	(	(I (car P))
			(II (cadr P))
		)
		(delta (enfoque I) nodo I II)
	)
)

;; Relevancia con promedio lineal
(defun similitud_lista(P lista)
	(let 	((I	(car P))
			 (II	(cadr P))
			 (L	(butlast lista 1))
		)
		(float (/ (sim_lista_R I II (enfoque I) L) (longitud L))) ;;Promediar
	)
)

(defun sim_lista_R(I II p lista)
	(cond 	((null lista)	0)
			(t (+ (delta p (car lista) I II) (sim_lista_R I II p (cdr lista))) )
	)
)

;; ******** Similitud lista-lista **********
(defun simListas(listaI listaII)
	(float (if (> (list-length listaI) (list-length listaII)) (simListas_r listaI listaII 0 0) (simListas_r listaII listaI 0 0)))
)

(defun simListas_r(listaI listaII N suma)
	(cond		((null listaII)	(/ suma (+ N (list-length listaI)))) ;;promediar similitud con la cantidad de muestras (incluyendo las faltantes, que son sim 0)
				(t	(simListas_r (cdr listaI) (cdr listaII) (+ N 1) (+ suma (dist (car listaI) (car listaII)))))
	)
)

;; ******* similitud estados ****************
;; Similitud entre estados
;; Calcula la similitud con promedio lineal, comparando la lista1 de estado1 con lista1 de estado2, y lista2 estado1 con lista2 estado2.
(defun similitud_estados(estado1 estado2)
	;; Convertir el estado actual en formato de problema
	(let 	((aI	(car estado1))
			 (aII	(cadr estado1))
			 (bI	(car estado2))
			 (bII	(cadr estado2))
		)
		(/ (+ (simE aI bI) (simE aII bII)) 2)
	)
)

;; simE: similitud entre listas de estados.
(defun simE(listaI listaII)
	(simE_r listaI listaII 0 0)
)

;; Similitud de estados-recurviso:
;; 	Para cada elemento en lista I
;;		si e en lista II, similitud es 1
;;		sino, similitud es la similitud máxima de e vs. cada elemento de lista II
;;		hacer e = siguiente elemento
;;		repetir hasta que e = nulo

(defun simE_r(listaI listaII N suma)
	(let	((mov1	(car listaI))
		)
		(cond	((null mov1)	(/ suma N))
				((contains listaII mov1)	(simE_r (cdr listaI) listaII (+ N 1) (+ suma 1)))
				(t	(simE_r (cdr listaI) listaII (+ N 1) (+ suma (maxSim mov1 listaII))))
		)
	)
)
;;

;; Similitud entre elemento y lista.  Retorna el valor máximo encontrado por elemento.
(defun maxSim(e lista)
	(maxSim_r e lista 0)
)

(defun maxSim_r(e lista maxS)
	(cond	((null lista)	maxS)
			(t 	(let	((simActual	(dist e (car lista))))
					(cond 	((> simActual maxS)	(maxSim_r e (cdr lista) simActual))
							(t				(maxSim_r e (cdr lista) maxS))
					)
				)
			)
	)
)
;; *** fin similitud estados ***

;;Definir similitud P -- nodo y similitud P -- experiencia?
	;; Comparar elemento (cada elemento?) en S con lo que exista en P
	
;; Similitud recursiva Problema--Nodo
(defun simR(I II p e_i)
	(cond 	((null I) 	0)
			(t )
	)
)

;; Determinar el área dónde jugar según una lista de movimientos
;; Default: último movimiento.
(defun enfoque(I)
	(car (last I))
)

;; F de distancia entre eventos (pares) (eq. 5.5)
;; I y II son listas de pares (movimientos actuales jugador I y II)
;; p es la posicion de enfoque
;; e_i el nodo actual en la red
(defun delta(p e_i I II)
	(cond	((contains I e_i)		1)
			((contains II e_i)	0)
			(t		 (dist p e_i))
	)
)

;; Funcion de distancia numerica para cuando e2 es un movimiento nuevo
(defun dist(e1 e2)
	(let*	(	(x1	(val (car e1)))  ;;Convertir a numero
			(y1	(cadr e1))
			(x2	(val (car e2))) ;;Convertir a numero
			(y2	(cadr e2))
			(valor (euclid x1 y1 x2 y2))
		)
		(cond	((= valor 0)	      1)
			((= valor 1)	 0.99)
			(t		(float (/ 1 valor)))
		)
	)
)

;; Distancia euclideana propuesta para eventos no presentes en el juego
(defun euclid(x1 y1 x2 y2)
	(sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))
)

;; Calcular el valor numérico correspondiente a una letra, (A = 0, B = 1, ... Z = 26)
(defun val(letra)
	(let	((ref (- (char-int #\A) 1)))		;;Calcular valor de referencia: 'A'-1 (donde 'A' = 1)
		(- (char-int (character letra)) ref)	;;Determinar valor posicional de letra con respecto a ref
	)
)

;; Determinar si el par ordenado (x,y) pertenece a la subred L
(defun contains(L par)
	(let	( 	(Li (cond ((atom (car L)) L) (t (car L))))		;;Extraer par actual
		)
		(cond	((null L) 			nil)	;;Lista nula, no pertenece
			((iguales par Li)	t) 	;;Si conciden, pertenece
			(t	(contains (cdr L)  par)) 	;;Sino buscar con el resto
		)
	)
	;;)
)

;; Conteo de elementos
(defun longitud(R)
	(cond	((null R)	1)
		(t (list-length (nodos R)))
	)
)

;; Aplanar red -- Generar lista de nodos
(defun nodos(R)
	(cond	((null R)	nil)
		((atom (car R))	(list R))
		(t (append (nodos (car R)) (nodos (cdr R))))
	)
)

;; Iguales: funcion para saber si un par es igual a otro (si son el mismo)
;; Se asume que un par tiene la forma (x y) donde 'x' es alfanumerico y 'y' numerico.
(defun iguales(p1 p2)
	(cond	((or (null p1) (null p2))	nil)
		(t (and (eq (car p1) (car p2)) (= (cadr p1) (cadr p2))))
	)
)


;; Función auxiliar para ordenar listas de pares.
;; Utilizar como llave la letra y después el número.
(defun mergesort(lista)
	(cond	((eq (list-length lista) 1)	lista)
		(t	(merge_ (mergesort (left lista)) (mergesort (right lista)) nil))		
	)
)

(defun merge_(lista1 lista2 listaR)
	(cond	((null lista1) (append listaR lista2))
		((null lista2) (append listaR lista1))
		((menor (car lista1) (car lista2))	(merge_ (cdr lista1) lista2 (append listaR (list (car lista1)))))
		(t					(merge_ lista1 (cdr lista2) (append listaR (list (car lista2)))))
	)
)

(defun left(lista)
	(subseq lista 0 (floor (/ (list-length lista) 2)))
)

(defun right(lista)
	(subseq lista (floor (/ (list-length lista) 2)) (list-length lista))
)

;; Determinar si (a,b) < (c,d) <=> a <= c && b < d
(defun menor(par1 par2)
	(let	( (a	(val (car par1)))
		  (b	(cadr par1))
		  (c	(val (car par2)))
		  (d	(cadr par2))
		)
		(or (< a c) (and (= a c) (< b d)))
	)
)

;; Eliminar listaE elementos de lista
(defun eliminarL(lista listaE)
	(cond	((null listaE) lista)
		(t (eliminarL (eliminarE lista (car listaE)) (cdr listaE)))
	)
)

;; Eliminar un elemento de una lista
(defun eliminarE(lista e)
	(eliminarE_ lista e nil)
)

;; eliminar el elemento indicado de la lista
(defun eliminarE_(lista e resultado)
	(cond	((null lista) resultado)
		((iguales (car lista) e)	(append resultado (cdr lista)))
		(t				(eliminarE_ (cdr lista) e (append resultado (list (car lista)))))
	)
)

;; interseccion entre listas de pares
(defun interseccion(lista1 lista2)
	(cond 	((null lista1)	nil)
		((contains lista2 (car lista1))	(append (list (car lista1)) (interseccion (cdr lista1) lista2)))
		(t				(interseccion (cdr lista1) lista2))
	)
)

;; diferencia entre listas
(defun diferenciaL(lista1 lista2)
	(eliminarL (union lista1 lista2) (interseccion lista1 lista2))
)