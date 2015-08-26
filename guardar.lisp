;; Funciones para guardar el espacio de trabajo y archivos

;; Guarda el ambiente de trabajo de CLisp en el archivo 'lispinit.mem'
;(defun guardarMemClisp()
;	(EXT:SAVEINITMEM)
;)
(load "util.lisp")

;; Guardar el ambiente de trabajo de SBCL en el archivo especificado
(defun guardarMemSBCL(archivo)
	(SB-EXT:SAVE-LISP-AND-DIE archivo)
)

;; Guardar objeto Lisp en archivo especificado en String.
(defun guardarObj(ob arch_str)
	(setq ARCH (open arch_str :direction :output :if-exists :supersede))
	(print ob ARCH)
	(close	ARCH)
)

;; Convertir red R a lista de estrategias y guardarla en texto plano.
(defun guardarRed(R archivo)
	(guardarObj (listarEstrategiasConV R) archivo)
)

;; Leer archivo con red y retornar lista
;; Para construir nueva red utilizar función (cargarER lista red) de grafo.lisp.
(defun cargarRed(archivo)
	(car (with-open-file (infile archivo)
				(do 	((result nil (cons next result))
						(next (read infile nil 'eof) (read infile nil 'eof)))
						((equal next 'eof) (reverse result))
				)
		)
	)
)