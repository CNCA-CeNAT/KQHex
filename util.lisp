;; *****************************
;; Funciones para contar ganadores y filtrar algunas estrategias de la red.
;; *****************************
;; R1, R2 listas de recompensas.
;; 1 ganar, -1 perder.
; I II
; 0, 1
; 1, 0,
; 0, 1

(defun contarGanadores(v r)
	(cond	((null r)	0)
		((> (car r) v)	(+ 1 (contarGanadores (car r) (cdr r))))
		(t		(contarGanadores (car r) (cdr r)))
	)
)

;;   Dadas listas de recompensas recibidas por el jugador I, jugador II y el largo (en movimientos) de todos los juegos,
;; calcular la cantidad de victorias de I y las de II.
;; Asume listas *rewards1*, *rewards2*, *longjuegos*
(defun contarG()
	(let 	((g1	(contarGanadores 0 *rewards1*)))
		(cons g1 (- (list-length *longjuegos*) g1))
	)
)

;; función que retorna las estrategias cuyo VEP supere un umbral.
(defun mejoresEstrategias(red umbral)
	(bmE (listar_rutas red) umbral nil)
)

(defun bmE(rutas umbral lista)
	(cond	((null rutas)	lista)
		((>= (valorEsperado (etiqueta (car rutas))) umbral) (bmE (cdr rutas) umbral (append lista (pegar (car rutas)))))
		(t	(bmE (cdr rutas) umbral lista))
	)
)

;; ****
(defun listarEstrategiasconV(red)
	(listarEstrategiasConV_ (listar_rutas red) nil)
)

(defun listarEstrategiasConV_(rutas lista)
	(cond	((null rutas)	lista)
			(t	(listarEstrategiasConV_ (cdr rutas) (append lista (pegar (car rutas)))))
	)
)
;;****

;;retornar la ruta con su VEP en una lista
(defun pegar(ruta)
	(list (append (butlast (cdr ruta)) (list (list (valorEsperado (etiqueta ruta))))))
)

;; listar todos los V's
(defun veps(R)
	(veps_r (listarvalores R))
)

(defun veps_r(valores)
	(cond	((null valores) nil)
		(t	(append (cdar valores) (veps_r (cdr valores))))	
	)
)

