;; Experimento para medir tiempos de respuesta
;; Requiere lista de estrategias S.

(setq P '( ((A 4) (B 4) (C 4) (D 4) ) ((E 1)(E 2)(E 3)(E 4))))

(setq R (crearredvacia))

(format t "Tiempo agregar:")

(time (agregarER s r))

(format t "Tiempo seleccionar")

(time (list-length (seleccion r p 'd)))
