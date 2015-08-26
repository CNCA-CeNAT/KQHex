;; *****************************************************
;; seleccion.lisp:
;; Selección de estrategias.  Implementa los algoritmos heurísticos de selección
;; y funciones adicionales para listar estrategias, relevancias, etc.
;;
;; Juan Carlos Saborio Morales,
;; Programa de Posgrado en Ciencias Cognoscitivas, UCR y
;; Colaboratorio Nacional de Computación Avanzada, CeNAT.
;; 2015.
;; *****************************************************

;; Selección:
;; Construir conjunto de rutas que satisfacen la funcion de relevancia definida para el modelo.
;; P = {I, II}, p = ultima posicion en I (área de enfoque)
(load "grafo.lisp")
(load "funcs.lisp")

(defparameter *umbral_default* 0.75)
(defparameter *umbral* 0.75)

;; En el caso de jugador II, para preservar los métodos, invertir las listas I y II en P
(defun invertir(P)
	(append (cdr P) (list (car P)))
)

(defun seleccion(red P modo)
	(cond	((eq modo 'D)	(seleccion_dinamica red P))
		((eq modo 'E)	(seleccion_estatica red P))
		(t (print "sintaxis: seleccion red P [D|E], umbral estático o dinámico"))
	)
)

;;   Selección dinámica: si la selección estática no encuentra resultados,
;;  repite la búsqueda con menor umbral.
(defun seleccion_dinamica(red P)
	(sel_rec red P *umbral*)
)

;; Buscar con umbral determinado.
;; Por ejemplo, umbral 1 para recuperar una estrategia específica.
(defun seleccion_exclusiva(red P umb)
	(sel_rec red P umb)
)

(defun sel_rec(red P umb)
	(let	((*umbral*	umb)
		 (S	(seleccion_estatica red P))
		)
		(cond	((= umb 0)	S)
			((null S)	(sel_rec red P (- umb 0.05)))
			(t 	S)
		)
	)
)

;; Funcion para generar estados plausibles para un problema P, en una red dada
;; Selección estática: utiliza el umbral tal cual.
(defun seleccion_estatica(red P)
	(evaluarSucesores red P (sucesores_ruta red nil) nil (enfoque (car P)))
)

;;Funcion auxiliar: Recibe una lista de sucesores y llama a recorridoR con cada uno de ellos
(defun evaluarSucesores(red P sucs ruta meta)
	(cond		((null sucs)	nil)
			((= (list-length (car sucs)) 1)	nil) ;; Si el sucesor es largo 1, entonces es la llave.  Igual que agotar los sucesores
			((>= (similitud P (car sucs)) *umbral*) (append (seleccionR red P (car sucs) (append ruta (list (car sucs))) meta) (evaluarSucesores red P (cdr sucs) ruta meta)))
			(t	(evaluarSucesores red P (cdr sucs) ruta meta))
	)
)

(defun seleccionR_anterior(red P nodo ruta meta)
	(cond		((null red)			nil)
				;; Si se alcanza la meta, responder con el subarbol
				((iguales nodo meta) (list (subred_recorrido red ruta)))
				;;Sino, evaluar los sucesores del nodo actual
				(t (evaluarSucesores red P (sucesores_ruta red ruta) ruta meta))
	)
)

;; Alternativa: Responder no con la subred, sino con la estrategia completa.
;; De esta forma se conoce la ruta para poder modificar su Valor esperado.
(defun seleccionR(red P nodo ruta meta)
	(cond	((null red)			nil)
		;; Si se alcanza la meta, responder con las rutas independientes.
		((>= (dist nodo meta) *umbral*) (construir_rutas ruta (listar_rutas (subred_recorrido red ruta))));;(list (subred_recorrido red ruta)))
		;;Sino, evaluar los sucesores del nodo actual
		(t (evaluarSucesores red P (sucesores_ruta red ruta) ruta meta))
	)
)

;; Dados una ruta base y un conjunto de subrutas generadas a partir de un árbol, crear las soluciones completas como aparecen en la red original.
(defun construir_rutas(base rutas)
	(cond		((null rutas) nil)
				(t	(append (list (append (butlast base) (car rutas))) (construir_rutas base (cdr rutas))))
	)
)

;; Búsqueda: utilizar para recuperar una estrategia completa.

;; 1. Selección estática para generar conjunto relevante
;; 2. Eliminar aquellas con sims menores que umbral
;; 3. De las restantes, comparar su similitud estructural (una estrategia contenida en otra no debe ser sim 1).
;; 4. Si alguna es mayor o igual a umbral, retornar su etiqueta.  Sino retornar retornar nil.
(defun buscar(lista red umbral)
	(let	((sols 	(seleccion red (list lista) 'e)))
			(cond		((null sols) nil)
					(t (buscar_r lista sols umbral))
			)
	)
)

;; Recorre la lista de soluciones y retorna la etiqueta de la primera cuya similitud con 'lista' sea mayor o igual a umbral-
(defun buscar_r(lista sols umbral)
	(cond	((null sols) nil)
			((>= (simListas lista (butlast (car sols))) umbral)	(etiqueta (car sols)))
			(t	(buscar_r lista (cdr sols) umbral))
	)
)

;; listar_rutas: Listar las rutas individuales en árbol de resultados.
;; Lee el subarbol R con resultados y genera todas las rutas independientes en una lista
(defun listar_rutas(R)
	(listar_rutas_r R nil)
)

(defun listar_rutas_r(R lista)
	(cond	((null R)	lista)
		((atom (car R))		(list (append lista (list R))))
		((eq (list-length R) 1)	(listar_rutas_r (car R) lista))
		((eq (list-length R) 2) 	(cond	((atom (caar R))	(listar_rutas_r (cdr R) (append lista (list (car R)))))
							(t				(append (listar_rutas_r (car R) lista) (listar_rutas_r (cdr R) lista)))
						)
		)
		(t (append (listar_rutas_r (car R) lista) (listar_rutas_r (cdr R) lista)))
	)
)

;; Listar para todas las estrategias contenidas en R, su similitud con P
(defun listar_relevancias(R P)
	(listar_rels_r R P)
)

(defun listar_rels_r(_rutas P)
	(cond	((null _rutas)	nil)
			(t	(append (list (append  (etiqueta (car _rutas)) (list (similitud_lista P (car _rutas))))) (listar_rels_r (cdr _rutas) P)))
	)
)

;; Contar la cantidad de estrategias en la red R.
(defun cantidadEstrategias(R)
	(list-length (listar_rutas R))
)