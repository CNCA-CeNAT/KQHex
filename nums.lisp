;; *****************************
;; Funciones numéricas
;; *****************************

;; Media de elementos en L
(defun mean(L)
	(float (/ (suma L) (list-length L)))
)

;; Suma de elementos en L
(defun suma(L)
	(cond	((null L)	0)
		(t	(+ (car L) (suma (cdr L))))
	)
)

;; Mayor valor en L
(defun maxL(L)
	(max_r (cdr L) (car L))
)

(defun max_r(L mL)
	(cond 	((null L) mL)
		((> (car L) mL)	(max_r (cdr L) (car L)))
		(t		(max_r (cdr L) mL))
	)
)