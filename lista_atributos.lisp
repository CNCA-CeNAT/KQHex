;; ***********************************************************
;; lista_atributos.lisp:
;; Funciones para manejar listas de propiedades de LISP como
;; listas de pares atributo-valor.

;; Juan Carlos Saborio Morales.
;; Colaboratorio Nacional de Computación Avanzada, CeNAT.
;; 2010
;; ***********************************************************

;; Agregar un par atributo-valor a la lista especificada
(defun agregarPar(lista atributo valor)
	(setf (getf (symbol-plist lista) atributo) valor)
)

;; Listar todos los pares de la lista
(defun verListaAV(lista)
	(symbol-plist lista)
)

;; Obtener el valor para el atributo especificado, en la lista especificada
(defun verValor(lista atributo)
	(get lista atributo)
)

;; Eliminar una lista de propiedades
(defun eliminarListaAV(lista)
	(setf (symbol-plist lista) nil)
)

;; Determina si el par atributo-valor existe en la lista especificada
(defun contieneAV(lista atributo valor)
	(cond 	( (eq (verValor lista atributo) valor) T)
		(T nil)
	)
)