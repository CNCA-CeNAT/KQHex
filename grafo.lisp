;; *****************************************************
;; grafo.lisp:
;; Implementación de la red de experiencias/estrategias.
;; Incluye funciones para crear y manipular la red.
;;
;; Juan Carlos Saborio Morales,
;; Programa de Posgrado en Ciencias Cognoscitivas, UCR y
;; Colaboratorio Nacional de Computación Avanzada, CeNAT.
;; 2015.
;; *****************************************************

(load "funcs.lisp") ;;Funciones de similitud, pertenencia, etc.
(load "lista_atributos.lisp") ;;Implementa listas de propiedades de Lisp, para guardar valores esperados promedio (V).

;; ************************
;; Formato de grafo:
;;	(R ((suc1 ((suc1_1 (suc1_n)))) ... (sucN ((sucN_1 (sucN_n))))))
;; 
;; donde
;;	R, suc_i son nodos y nodo = (x y)
;; ************************

;; ************************
;; Listado de funciones:
;; ************************
;; crearRedVacia 				--> crea lista con nodo raiz (0 0)
;; crearRedRaiz(raiz)				--> crea lista con nodo raiz (raiz)
;; crearRed(EE) 				--> crea red con nodo raiz (0 0) y agrega elementos en EE
;; agregarExp(EE R) 				--> agrega el conjunto EE a la red previamente creada R
;; agregarER(EE R)				--> utilizado por agregarExp para recorrido recursivo de EE
;; agregar(E R N L ruta)			--> agregar experiencia E a red R a partir del nodo N con lista de dimensiones L.  Ruta es el identificador único del nodo N
;; agregarSuc(R N S)				--> Agrega el sucesor S al nodo con ruta N en la red R
;; sucesores(R N)				--> lista de sucesores del nodo N en la red R (problema con duplicados)
;; sucesores_ruta(R N)			--> lista de sucesores del nodo con ruta N en la red R
;; 1) subRed(R N)				--> puntero a la subred en R cuya raiz es el nodo N 
;; 2) subRed_recorrido(R N)		--> puntero a la subred en R cuya raiz es el nodo con ruta N
;; seleccionar_sucesor(Red nodo)	--> encontrar puntero al sucesor de la raiz de R que sea igual a nodo
;; root(R)						--> retorna el puntero a la raiz de la red R
;; dimensiones(E)				--> genera el conjunto de descriptores de la experiencia E
;; ************************


;; Crear una red con raiz (0 0)
(defun crearRedVacia()
	(list '(0 0))
)

;; Crear red con raiz especificada
(defun crearRedRaiz(raiz)
	(list raiz)
)

;; Crear una red a partir de conjunto de experiencias EE
;; Parte de una red vacia ((0 0))
(defun crearRed(EE)
	(let*	(	(red (crearRedVacia))		;;Crear simbolo para red nueva
			(tmp (agregarExp EE red))	;;Agregar EE
		)
		red	;;Retornar red creada
	)
)

;; Agregar el conjunto de experiencias EE a la red R.
(defun agregarExp(EE R)
	(cond	((null EE) R)
		(t (agregarER EE R))
	)
)

;; Agregar Experiencia --Recursivo.
;; Recorre el conjunto de experiencias, EE, y agrega cada una mediante la funcion "agregar".
;; Primero se agrega la experiencia (car EE) y luego se repite agregarER.
;; El metodo agregar NO retorna la red, hay que modificarla primero (let redTMP) y continuar recursivamente.
;; Al partir de la raiz de una red, la ruta es siempre nil.
(defun agregarER(EE R)
	(cond	((null EE)	t)
			(t	(let	((redTMP	(agregar (car EE) R (root R) (dimensiones (car EE)) nil)))
					(agregarER (cdr EE) R)
				)
			)
	)
)

;; Funcion agregar (Experiencia, Red, Nodo, Lista de dimensiones, ruta)
;; Nodo es el elemento a partir del cual agregar (ultimo en ruta)
;; Ruta es la secuencia recorrida hasta el momento.  Utilizado como identificador unico para agregar
;;siempre en la posicion correcta, independientemente de la representacion.
(defun agregar(E R N L ruta)
	(let	(	(S (sucesores_ruta  R ruta)) 	;; Sucesores del nodo actual en R
			(l_0 (car L))				;; Primer dimension
		)
		(cond	((null L) 		R) 	;; Al finalizar la lista de dimensiones, retornar la Red
				((= (list-length L) 1)  ;; Ultima dimension, i.e. llave para valor esperado
					(let	((redTMP (agregar_suc R ruta l_0))) ;;agregar nodo y finalizar
						redTMP
					)
				)
				((contains S l_0)	(agregar E R l_0 (cdr L) (append ruta (list l_0)))) 	;; Nodo ya existe, continuar con la siguiente dimension a partir de ese nodo
				(t 	(let			((redTMP (agregar_suc R ruta l_0)))				;; Sino, agregar sucesor l_0 a nodo N en R
								(agregar E R l_0 (cdr L) (append ruta (list l_0))) 	;; Recurrir a partir de l_0 en la nueva R
					)
				)
		)
	)
)

;; Genera conjunto de sucesores del nodo N en R
(defun sucesores(R N)
	(sucs_rec (cadr (subRed R N)))
)

;; Genera conjunto de sucesores subsiguiente a la secuencia de nodos N, en R
(defun sucesores_ruta(R N)
	(sucs_rec (cadr (subRed_recorrido R N)))
)

;; Utilizado sucesores y sucesores_ruta para la construccion de la lista de sucesores.
(defun sucs_rec(R)
	(cond 	( (null R) R)
			((atom (caar R))	R)	;;Identifica las raices de las subredes, i.e. sucesores.
			(t (append (list (caar R)) (sucs_rec (cdr R))))  ;;Construccion recursiva.  Doble 'car' para no perder el resto de la red (demas sucesores)
	)
)

;; Agrega nodo S a nodo N en red R
;;** Problema: no se puede utilizar "subred" porque puede haber nodos repetidos.  Se podría recibir puntero directamente (car) del metodo agregar
;;** Solución: N ES UNA RUTA, NO UN NODO
(defun agregar_suc(R N S)
;; 1) Generar la subred con raiz secuencia N.
	;;(let	((red	(subRed R N)))
	(let	((red	(subRed_recorrido R N)))
		(cond 	((null red)	nil)		;; 2) Si no existe, retornar nil.
				((null (cadr red))	(nconc red (list (list (list S)))))	;; 3) Si no tiene sucesores, concatenar el nodo con lista que contiene (S)
				(t 	(nconc (cadr red) (list (list S)))) ;; 4) Si tiene sucesores, concatenar la lista de sucesores con (S).
		)
	)
)


;; Metodo para generar la subred a partir un nodo dado, para nodos LISTA (no atomicos)
(defun subRed(R N)
	(cond	((null R)		nil)
			((atom R)	nil)		;; Si R llega a ser un valor atomico, se separo demasiado
			((atom (car R))		;; Si el car de R es atomico, quiere decir que no tiene sucesores
				(cond	((iguales R N)	R)	;;Por lo tanto se compara R y no su car
						(t	nil)
				)
			)
			((iguales (root R) N)	R)	;;Si la raiz de R es el nodo buscado, retornar R	
;; Probar (es ineficiente porque recorre la lista varias veces pero garantiza retornar un CAR
			((contains (nodos (car R)) N)	(subRed (car R) N))
			((contains (nodos (cdr R)) N)	(subRed (cdr R) N))
			(t	nil)
;;			(t	(append (subRed (car R) N) (subRed (cdr R) N)))
	)
)

;; Generar la subred en R subsiguiente a la secuencia de nodos N (raiz mas predecesores)
;; N *no* debe incluir la raiz de la red, solo sucesores.
;; Si (null N) --> se retorna R.
(defun subRed_recorrido(R N)
	(let	((nodo	(car N)))	;;Extraer primer nodo en la ruta N
		(cond 	((null R)		nil)
				((null N)		R)		;; Si se agota a secuencia, se ha llegado al lugar buscado
				((atom R)	nil)
				;;Continuar recorrido con sucesor de R que sea igual a nodo, y el resto de la secuencia
				(t	(subRed_recorrido (seleccionar_sucesor R nodo) (cdr N)))
		)
	)
)

;; Seleccionar el sucesor de la raiz de Red que es igual a nodo
;; Retornar puntero
(defun seleccionar_sucesor(Red nodo)
	(seleccionar_sucesor_R (cadr (subred Red (root Red))) nodo)
)

;; Utilizado por seleccionar_sucesor
;; Busca en los sucesores de un nodo aquel que sea igual al nodo buscado
;; Retorna un *puntero* y no una copia
(defun seleccionar_sucesor_R(Sucs nodo)
;	(subRed Red nodo)	
	(cond 	((null Sucs) nil)
			((atom (caar Sucs))	;;Nodo sin sucesores
				(cond	((iguales (car Sucs) nodo)	Sucs)
						(t nil)	;; Si no es, finalizar llamado
				)
			)
			(t	(cond 	((iguales (caar Sucs) nodo)	(car  Sucs))		;;Nodo con sucesores
						(t	(seleccionar_sucesor_R (cdr Sucs) nodo))	;;Si no es, continuar en amplitud
				)
			)
	)

)

;; root(Red) = determinar raiz
(defun root(R)
	(car R) ;;El primer nodo deberia ser la raiz.
)

;; dimensiones(Experiencia) = lista de descriptores
;; En el caso de estrategias de Hex, se puede utilizar la estrategia tal cual.
;; Para otros problemas puede ser necesario eliminar dimensiones no predictivas.
;; En el caso de Hex, *agregar* una dimension para controlar el valor esperado de la estrategia en una lista de atributos
(defun dimensiones(E)
	;;E	;;Por ahora las dimensiones son sus propios pares.
	;;Utilizar los pares de la secuencia más una etiqueta con un valor, inicialmente 0
	(let*	(	(etiqueta (gensym))
			(val (agregarpar etiqueta 'v '0))
		)
		(append E (list (list etiqueta)))
	)
)

;; *********** Mantenimiento de la Red ***************

;; Obtener la etiqueta de una estrategia en lista o subarbol
(defun etiqueta(L)
	(car (last (nodos L)))
)

;; Dada una estrategia fuera de la red, determinar su etiqueta
(defun buscarEtiqueta(E R)
	(cond	((null E)	nil)
			((eq (list-length (car (last E))) 1)	(list (etiqueta E)))
			(t (buscarEtiqueta_R E R))
	)
)

(defun buscarEtiqueta_R(E R)
	(cond	((null E)	(aplanar (cdr R)))
		(t (buscarEtiqueta_R (cdr E) (subred_recorrido R (list (car E)))))
	)
)

;; Obtener el valor esperado de una estrategia, identificada con la lista 'sym'
(defun valorEsperado(sym)
	(cond	((listp sym)	(vervalor (car sym) 'v))
		(t	(vervalor sym 'v))
	)
)

;; Modificar el valor esperado de una estrategia, identificada con la lista 'sym'
(defun modificarV(sym val)
	(cond	((listp sym)	(agregarpar (car sym) 'v val))
		(t		(agregarpar sym 'v val))
	)
	
)

;; Listar el valor de todas las estrategias
(defun listarValores(red)
	(listarValoresR	(listarLlaves red))
)

(defun listarValoresR(listaLlaves)
	(cond 	((null listaLlaves)	nil)
			(t	(append (list (append (list (caar listaLlaves)) (list (vervalor (caar listaLlaves) 'v)))) (listarValoresR (cdr listallaves))))
	)
)

;; Generar lista de llaves de la red
(defun listarLlaves(red)
	(llaves (nodos red) nil)
)

;; Filtrar lista de nodos para extraer las llaves
(defun llaves(listaNodos listaLlaves)
	(cond	((null listaNodos)	listaLlaves)
			((= (list-length (car listaNodos)) 1)	(llaves (cdr listaNodos) (append listallaves (list (car listaNodos)))))
			(t	(llaves (cdr listaNodos) listaLlaves))
	)
)

;; función que retorna la estrategia con mayor VEP.
(defun mejorEstrategia(red)
	(buscarMejorE (listar_rutas red) nil 0)
)

(defun buscarMejorE(rutas mejorE mejorVEP)
	(cond	((null rutas)	(append mejorE mejorVEP))
		((> (valorEsperado (etiqueta (car rutas))) mejorVEP) (buscarMejorE (cdr rutas) (car rutas) (valorEsperado (etiqueta (car rutas)))))
		(t	(buscarMejorE (cdr rutas) mejorE mejorVEP))
	)
)

;;****************************************************************
;; Funciones para cargar red previamente construida a partir
;; de una lista.

;; La lista contiene estrategias de la forma: (e_1, e_2, ..., V).

;; Se debe eliminar el V y reemplazar por una etiqueta 
;; autogenerada cuyo valor en la lista de propiedades es V.
;;****************************************************************

;; Cargar contenido de EE en red R (vacía)
(defun cargarER(EE R)
	(cond	((null EE)	t)
			(t	(let	((redTMP	(agregar (butlast (car EE)) R (root R) (dimensionesCargar (car EE)) nil)))
					(cargarER (cdr EE) R)
				)
			)
	)
)

(defun dimensionesCargar(E)
	;; Las dimensiones de una estrategia cargada deben incluir una nueva llave aleatoria, cuyo valor se inicializa en el valor de ultimo elemento de E
	(let*	(	(etiqueta (gensym))
			(val (agregarpar etiqueta 'v (caar (last E))))
		)
		(append (butlast E) (list (list etiqueta))) ;;Las dimensiones son la lista de pares, menos su valor, más su etiqueta nueva.
	)
)
;; *************** Fin CARGAR *********************************

;; *******************************
;; Aux
;; *******************************

;; Retorna el valor de la estrategia en forma de lista
(defun valor(lista)
	(cadr (final lista))
)

;; Retornar último elemento de lista de pares, i.e. valor de la estrategia.
(defun final(lista)
	(nth (- (contar lista) 1) lista)
)

(defun contar(R)
	(list-length (aplanar R))
)

(defun aplanar(lista)
	(cond	( (null lista) nil )
			( (atom (car lista)) (list lista) )
			( T (append (aplanar (car lista)) (aplanar (cdr lista)) ) )
	)
)