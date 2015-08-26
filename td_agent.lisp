;; *****************************************************
;; td_agent.lisp:
;; Agente Q-Learning con funciones heurísticas.
;; Basado en "Heuristic Approach to Autonomous Agent Learning".
;;
;; Juan Carlos Saborio Morales,
;; Programa de Posgrado en Ciencias Cognoscitivas, UCR y
;; Colaboratorio Nacional de Computación Avanzada, CeNAT.
;; 2015.
;; *****************************************************

;; - Exploratorio (requiere función especial)
;; - Libre de modelo (valores Q: accion-utilidad)
;; - On/Off policy (SARSA / Q-learning)

(load "seleccion.lisp")
(load "hex.lisp")

;; Tabla de valores de acciones (Red de experiencias)
(defparameter *Q* (crearredvacia))

;; Tabla de frecuencias para pares estado-accion
(setq N 0)

;; Listas para movimientos de jugadores y para el problema
(defparameter I  nil)
(defparameter II nil)
(defparameter P nil)

;; Contenido de la política actual
(defparameter *pi_I* nil)
(defparameter *pi_II* nil)
(defparameter *turnoI* nil)	;; Turno del jugador I (nil -> II)
(defparameter *epsilon* 0.1)	;; Valor epsilon para política
(defparameter *rewardW*	1)	;; Recompensa al GANAR el juego
(defparameter *rewardL*	-1)	;; Recompensa al PERDER el juego
;; entre estados no finales, la recompensa es 0.

;; Variables de estado para SARSA y QL
(defparameter *s* nil) ;;previous
(defparameter *a* nil) ;;previous
(defparameter *r* nil) ;;previous
(defparameter *s_prima* nil) ;;current
(defparameter *a_prima* nil) ;;current
(defparameter *r_prima* nil) ;;current

;; Parámetros
;; NOTA: determinar valores apropiados para parámetros.
(defparameter *alpha*	0.1)  ;;Learning rate (0 = no learning; 1 = override, only most recent)
(defparameter *gamma*	0.1)  ;;Discount factor (0 = only current rewards; 1 = long term rewards).

;; Umbral para determinar si un estado se agrega o no a la red.
(defparameter *umbral_estado_nuevo* 0.75)

;; *********** Funciones principales de operación *************
;; Leer una lista de estrategias básicas y guardarlas en la red *Q*
(defun estrategiasIniciales()
	(agregarExp (estrategiasInit) *Q*)
)

;; Iniciar entrenamiento self-play.
;; Ejemplo: (iniciar 'q 1000) para 1000 episodios de Q-Learning.
;; SARSA no está implementado.
(defun iniciar(modo trials)
	(cond	((eq modo 'S)	(sarsa trials))
		((eq modo 'Q)	(qLearning trials))
	)
)

;; Importante: reinicializar las variables de estado antes de iniciar cada episodio
(defun inicializarVars()
	(setq I nil)
	(setq II nil)
	(setq P nil)
	(setq *pi_i* (initPi))
	(setq *pi_ii* (transponerE (initPi)))
	(setq *turnoI* nil)
)



;; ******** Funciones generales para TD-Learning ********

;; Inicializar el estado inicial
;; Nil? P?
(defun initS()
	(setq *s* nil)
)

;; Seleccionar una estrategia existente en forma aleatoria.
(defun initPi()
;; Inicializar con las políticas de mayor valor esperado?
	(let	((rutas (listar_rutas *Q*)))
		(butlast (cdr (nth (random (list-length rutas)) rutas))) ;; cdr para eliminar la raíz de la red, butlast para eliminar la etiqueta
	)
)

;; Seleccionar una accion siguiendo una politica (epsilon-greedy)
(defun accion(estado)
	(if	(< (random 100) (* *epsilon* 100))
		(exploracion2 estado) 	;; P < epsilon, explorar (2 -- aleatorio)
		(greedy estado) 	;; P >= epsilon, maximizar con política greedy.
	)
)

;; Política greedy: genera las acciones posibles y selecciona aquella con max_q(s,a).
;; Implementa 1-ply lookahead.  Sólo un movimiento.
;; Para utilizar dos, habría que llamar a greedy de nuevo.

;; ALG:
;; 1. Para cada movimiento i
;; 2.   Para cada movimiento j
;; 3.	 Q_ij := q(tau(s,a_i),a)
;; 4. Seleccionar max Q_ij.

;; Pendiente: Diseñar función n-ply lookahead.
(defun greedy(estado)
	(let	((ac	(greedy_r estado))
		)
		(if	(null ac)
			(seleccionarAleatorio estado)
			ac
		)
	)
	
)

(defun greedy_r(estado)
	(cond	(*turnoI* 	(greedy_maxQ estado (movimientosPosibles estado (enfoque (car estado))) nil 0)) ;;cambiar 0 por valor más bajo
		(t 		(greedy_maxQ estado (movimientosPosibles estado (enfoque (cadr estado))) nil 0))
	)
)

;; seleccionar max_q(s,a) movsPosibles(s)
(defun greedy_maxQ(estado movimientos mejorE mejorQ)
	(cond	((null movimientos)	mejorE)		
		((>= (q estado (car movimientos)) mejorQ) 	(greedy_maxQ estado (cdr movimientos) (car movimientos) (q estado (car movimientos))))
		(t 						(greedy_maxQ estado (cdr movimientos) mejorE mejorQ))
	)
)

;; Exploración2: seleccionar una acción aleatoria de un conjunto de movimientos posibles.
;; TODO: qué hacer cuando estado es nil? (al inicio).
(defun exploracion2(estado)
	(cond	(*turnoI* 	(exploracion2_r estado (movimientosPosibles estado (enfoque (car estado)))))
		(t 		(exploracion2_r estado (movimientosPosibles estado (enfoque (cadr estado)))))
	)
)

;; Selección aleatoria de una lista de movimientos, siempre que el elemento sea factible.
;; Si no se encuentran posiciones válidas, explorar aleatoriamente.
(defun exploracion2_r(estado movimientos)
	(cond	((null movimientos)	(seleccionarAleatorio estado))
		(t	(nth (random (list-length movimientos)) movimientos))
	
	)
)

;; Determinar si 's' es un estado final
;; Separar la implementación para usar criterios de Hex
(defun terminal(s)
	(terminalD s)
)

;; Función "ganador".  Determinar si el jugador actual (*turnoI*) gana según el estado final actual.
;; Separar la implementación para usar criterios de Hex.
(defun ganador()
	(ganadorD P)
)

;; Determinar 'reward' R <-- (s,a)
;; Si el estado es final, recompensa es positiva al ganar. sino negativa.
;; Sino, recompensa es 0.
;; NOTA: validar recompensa.
(defun recompensa(s a)
	(let	((e_t	(transicionSim s a)))
		(if	(terminal e_t)	*rewardW* 0)
	)
	
	;	(cond	((and (terminal (transicionSim s a)) (eq (ganador) '1) *turnoI*) *rewardW*)
	;		((and (terminal (transicionSim s a)) (eq (ganador) '2) *turnoI*) *rewardL*)
	;		((and (terminal (transicionSim s a)) (eq (ganador) '1) (not *turnoI*)) *rewardL*)
	;		((and (terminal (transicionSim s a)) (eq (ganador) '2) (not *turnoI*)) *rewardW*)
	;		(t	0)
	;	)
	;)
)

;; Ejecutar transición al nuevo estado s' <-- (s,a)
;; Modifica las variables globales I, II y P.
(defun transicion(s a)
;	(cond	(*turnoI*	(append (list (append (car s) (list a))) (list (cadr s))))
;		(t		(append (list (car s)) (list (append (cadr s) (list a)))))
;	)
	(cond	(*turnoI*	(agregarm1 a))
		(t		(agregarm2 a))
	)
)

;; Transición simulada s'.
(defun transicionSim(s a)
	(cond	(*turnoI*	(append (list (append (car s) (list a))) (list (cadr s))))
		(t		(append (list (car s)) (list (append (cadr s) (list a)))))
	)
)

;; Cambiar entre jugador I y jugador II.  Ambos usan la misma red (self-play), 
;;pero utilizan diferentes estrategias
(defun alternarTurno()
	(setq	*turnoI* (not *turnoI*))
)

;; ********************************

;; *********** SARSA **************
;; SARSA: Loop de episodios
;; Diseñar para "self-play" (dos jugadores -- una red)
;; NOTA: No está adaptado para dos jugadores.
(defun sarsa(trials)
	(if	(> trials 0)
		(let*	(	
				(*s*	(initS))  ;;inicializar estado
				(*a*	(accion *s*)) ;;seleccionar acción para estado actual
				(tmp	(sarsaF2 *s* *a*))
			)
			;; agregar estados nuevos I y II al finalizar episodio
			(sarsa (- trials 1))
		)
	)
)

;; SARSA: función de aprendizaje
;; NOTA: Duda: si la acción se decide antes del siguente turno, puede esa acción no ser válida? Por ejemplo, si el jugador II la toma.
(defun sarsaF2(s a)
	(alternarTurno)	;;alternar jugadores cada turno
	(let*	(	(*s* s)
			(*a* a)
			(*r* (recompensa *s* *a*))  ;; Recompensa de tomar accion 'a' en 's'
			(*s_prima* (transicion *s* *a*)) ;; Determinar estado s' <-- (s,a).  Modifica variables I, II, y P.
			(*a_prima* (accion *s_prima*)) ;; Seleccionar acción utilizando política e-greedy
			(tmp (updateRuleSARSA s a *r* *s_prima* *a_prima*)) ;;actualizar valores Q con (s a r s' a')
		)
		(cond	((terminal s) (agregarEstados))		;;Al finalizar episodio, agregar estados I y II
			(t (sarsaF2 *s_prima* *a_prima*))	;;Sino continuar con s' a'
		)
	)
)

;; Todos los estados son P (<I> <II>).
;; Función updateQ debe recibir sólo (I) ó (II).
;; ¿Cómo aplicar updateQ? R/ Se incorpora ya sea I ó II, o ambos.
(defun updateRuleSARSA(s a r s_p a_p)
	(let*	(	(qVal		(q s a))
			(qVal_prima	(q s_p a_p))
			(nuevoVal 	(temporal_difference r qVal_prima qVal))
			(val		(+ qVal nuevoVal))
		)
		(updateQ s a val)
	)
)

;; ************* FIN SARSA ************

;; ************ Q-Learning ************
(defparameter *rewards1* nil)
(defparameter *rewards2* nil)
(defparameter *longJuegos* nil)
(defun actualizarValoresEpisodio()
	(setq *rewards1* (append *rewards1* (list (+ (if (eq (ganador) '1) *rewardW* *rewardL*) (if *rewards1* (car (last *rewards1*)) 0)))))
	(setq *rewards2* (append *rewards2* (list (+ (if (eq (ganador) '2) *rewardW* *rewardL*) (if *rewards2* (car (last *rewards2*)) 0)))))
	(setq *longJuegos* (append *longJuegos* (list (list-length (aplanar P)))))
)


(defun qLearning(trials)
	(if	(> trials 0)
		(let*	(	(tmp	(inicializarVars))
				(*s*	(initS))  ;;inicializar estado
				(tmp	(qLearningF2 *s*))
			)
			;; agregar estados nuevos I y II al finalizar episodio
			(format t "Episodio actual: ~D~%" trials)
			;; Registrar rewards cumulativos y longitud de los juegos
			(actualizarValoresEpisodio)
			(qLearning (- trials 1))
		)
		T
	)
)

(defun qLearningF2(s)
	(alternarTurno)	;;alternar jugadores cada turno
	(let*	(	(*s* s)
			(*a* (accion *s*))	;; seleccionar acción usando política e-greedy
			(*r* (recompensa *s* *a*))  ;; Recompensa de tomar accion 'a' en 's'
			(*s_prima* (transicion *s* *a*)) ;; Determinar estado s' <-- (s,a).  Modifica variables I, II, y P.
			(tmp (updateRuleQL *s* *a* *r* *s_prima*)) ;;actualizar valores Q con (s a r s')
		)
		(if	(terminal *s_prima*)
			(agregarEstados *s_prima*)	;;Al finalizar episodio, agregar estados I y II
			(qLearningF2 *s_prima*)		;;Sino continuar con s <-- s'
		)
	)
)

;; Regla de actualización de valores Q aproximada.
(defun updateRuleQL(s a r s_p)
	(let* 	(	(qVal		(q s a))
			(qVal_prima	(max_AQ s_p)) ;; elegir q(s',a) máximo
			(nuevoVal 	(temporal_difference r qVal_prima qVal))
			(val		(+ qVal nuevoVal))
		)
		(updateQ s a val)
	)
)

;; max_a q(s',a) --> Q(s'a) con accion cuyo valor es máximo.
;; ** Se puede hacer en paralelo. **
(defun max_AQ(estado)
	(cond	(*turnoI* 	(max_AQ_r estado (movimientosPosibles estado (enfoque (car estado))) 0))
		(t 		(max_AQ_r estado (movimientosPosibles estado (enfoque (cadr estado))) 0))
	)
)

;; Retorna el valor 'q' para un S' dado, donde q(s',a) es máximo.
(defun max_AQ_r(estado movimientos mejorQ)
	(cond	((null movimientos)	mejorQ)
		((contains (aplanar estado) (car movimientos))	(max_AQ_r estado (cdr movimientos) mejorQ)) ;;ignorar posiciones ya jugadas
		((>= (q estado (car movimientos)) mejorQ) 	(max_AQ_r estado (cdr movimientos) (q estado (car movimientos))))
		(t 						(max_AQ_r estado (cdr movimientos) mejorQ))
	)
)

;; ************* FIN Q-Learning *******

;; ******* Administración de red Q ********
; Funcion q (minuscula): conocer el valor de Q(s,a) en la red
;; similitud_estados debe retornar la similitud entre el estado mas semejante en la red y el actual (metodo seleccion)
;; valor esperado debe retornar el Q(s,a) del estado mas semejante en la red (red de experiencias)
;; estado_mas_semejante debe retornar EL estado con mayor similitud a 'estado'
(defun q(s accion)
	;; s* = max_s' sim(s', s)
	;; Q(s,a) = V(s*) * sim(s*,A(s*))
	;; NOTA: transponer estado cuando es el turno de II
	(if	(or (null (car s)) (null (cadr s))) 0			;; Por definición, ignorar los estados iniciales i.e. NIL NIL
	(let*	((estado	(transP_turno s))
		 (estrategia_semejante	(transponerE_turno (estado_mas_semejante estado)))  ;; potencialmente, igual.
		 (estado_accion		(transicionSim estado accion))	;; estado actual + acción simulada, sin realizar los cambios.  (sólo consulta)
		)
		(* (valorEsperado (etiqueta estrategia_semejante)) (similitud_lista estado_accion estrategia_semejante))
	)
	)
)

;; Estado mas semejante:
;;   Dado un estado, se genera el subconjunto de estados semejantes y se selecciona aquel
;;  cuya similitud sea mayor.
;; ** Se puede hacer en paralelo.**
(defun estado_mas_semejante(estado)
	(let* 	((s (if (null (car estado)) (append (list (list (root *Q*))) (cadr estado)) estado))
		 (rutas (seleccion *Q* s 'D))
		)
		(estado_mas_semejante_R s rutas nil 0)
	)
)

;; Recorre la lista 'rutas' y busca el estado con mayor RELEVANCIA
(defun estado_mas_semejante_R(estado rutas mejorRuta mejorValor)
	(cond	((null rutas) mejorRuta)
		(t	(let	( (valorActual (similitud_lista estado (car rutas))))
				(cond	((>= valorActual mejorValor) (estado_mas_semejante_R estado (cdr rutas) (car rutas) valorActual))
					(t (estado_mas_semejante_R estado (cdr rutas) mejorRuta mejorValor))
				)
			)
		)
	)
)

;;   Generar conjunto de rutas a partir de 'estado' y seleccionar aquel con
;; mejor valor esperado.
(defun estado_mejor_valor(estado)
	(let 	( (rutas (seleccion *Q* estado 'D))
			)
			(mejorEstado rutas nil 0)
	)
)

;; Recorre la lista 'rutas' y busca el estado con el mayor valor esperado.
(defun mejorEstado(rutas mejorRuta mejorValor)
	(cond	((null rutas) mejorRuta)
		(t	(let	( (valorActual (valorEsperado (etiqueta (car rutas)))) )
				(cond	((>= valorActual mejorValor) (mejorEstado (cdr rutas) (car rutas) valorActual))
					(t (mejorEstado (cdr rutas) mejorRuta mejorValor))
				)
			)
		)
	)
)

;; Cambiar el valor de Q(s,a) por el indicado en 'val'
;; NOTA: transponer al turno de II
(defun updateQ(s a val)
	(if (and (not (null (car s))) (not (null (cadr s))))
	(let*	( (estado	(transP_turno s)) ;;Determinar si transponer estado o no según turno.
		  (EST		(estado_mas_semejante estado))
		)
		(modificarV (etiqueta EST) val)
	)
	)
	
)

;; En esta versión de update rule no se utiliza 'a' ya que los valores Q son generalizados y promediados por estado.
;; El updateQ se aplica a una estrategia (I ó II).
;; Si se usa una política, 's' es un estado existente con VEP.
;; Sino, decidir qué actualizar.  Si el estado más semejante ó crear uno nuevo.
(defun updateQ_(s a val)
	(let	((sym	(car (buscarEtiqueta s *Q*))))
		(cond	((null sym)	(agregarEstadoValor s a val))
			(t		(modificarV sym val))
		)
	)
)

;; Regla 'temporal-difference': a(R + [g*Q(s',a') * Q(s,a)])
(defun temporal_difference(r qVal_prima qVal)
	(* *alpha* (+ r (- (* *gamma* qVal_prima) qVal)))
)

;; ** Agregar estados **
;; Utilizar umbral variable, permisivo al inicio, estricto al final.  Segun existan elementos semejantes.
;; 1) Determinar si existe estrategia s (+a).  Si existe, modificarV.
;; 2) Sino agregar s(+a) y ponerle valor
;; Además los estados en juego no tienen la 'etiqueta', hay que buscarla en la red

;; Función agregar listas de movimientos I y II
;; NOTA: transponer agregar II
;; NOTA: agregar sólo estrategias ganadoras?
(defun agregarEstados(estado)
	(if	(eq (ganador) '1)
		(agregarEstrategia (car estado))
		(agregarEstrategia (transponere (cadr estado)))
	)
)

;; Agregar Estado-Valor: busca elemento en la red cuya similitud sea igual o mayor que el umbral.
;; Si existe, se actualiza su valor.
;; Sino, se agrega nueva estrategia con valor dado.
(defun agregarEstrategia(estrategia)
	(let* 	((etiq (existeSemejante estrategia))
		 (valor	(valorEsperado etiq))
		)
		(if (null etiq) (agregar_ estrategia (valorEsperado (etiqueta (estado_mas_semejante (list estrategia)))))) ;;En teoría no hace falta modificar el valor de una estrategia existente ya que fue modificado mediante updateQ
	)
)

;; Determinar si existe estrategia en *Q* con similitud mayor o igual a umbral.
;; Si existe, retornar su etiqueta
;; Sino, retornar nil
(defun existeSemejante(estrategia)
	(buscar estrategia *Q* *umbral_estado_nuevo*)
)

;; Agregar estrategia en *Q* y poner VEP.
(defun agregar_(estrategia valor)
	(agregarExp (list estrategia) *Q*)
	(modificarV (car (buscarEtiqueta estrategia *Q*)) valor)
)

;; ****** Fin Adm. Q *****************

;; ***** Funciones Específicas *********
;; Asumen que la representación es por pares (a,v).
;; Pueden modificarse para otros dominios.

;; Actualizar P
(defun actualizarP()
	(setq P (append (list I) (list II)))
)

;; Agregar movimiento a jugador I
(defun agregarM1(mov)
	(setq I (append I (list mov)))
	(actualizarP)
)

;; Agregar movimiento a jugador II
(defun agregarM2(mov)
	(setq II (append II (list mov)))
	(actualizarP)
)

;; Las demás funciones están en hex.lisp.

;;**** FIN DE ARCHIVO ******