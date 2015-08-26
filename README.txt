*********************************************************************
KQHEX -- Agente jugador de Hex: exploratorio, libre de modelo, heurístico.
			Basado en enfoque heurístico para aprendizaje y toma de decisiones.

Por: Juan Carlos Saborío.
Programa de Posgrado en Ciencias Cognoscitivas, UCR.
y Colaboratorio Nacional de Computación Avanzada (CNCA),
Centro Nacional de Alta Tecnología (CeNAT).
Correo-e: jcsaborio@cenat.ac.cr
2015
*********************************************************************

1. LISTADO DE ARCHIVOS

Se proveen los siguientes archivos:

Archivo						Descripción
------------------------------------
50k.dat:						Representación en texto de red de estrategias tras 50000 episodios de entrenamiento.
funcs.lisp:					Funciones de relevancia, similitud y distancia.
*grafo.lisp					Red de estrategias y funciones asociadas.
guardar.lisp				Funciones para guardar y cargar redes en y desde archivos de texto.
*hex.lisp					Modelo de tablero de Hex y funciones asociadas.
*lista_atributos.lisp	Implementa una lista de atributos mediante listas de propiedades Lisp.
nums.lisp					Funciones numéricas para análisis de juegos.
pruebas.lisp				Ejemplo de inicialización.
*seleccion.lisp			Modelo de selección de estrategias.
*td_agent.lisp				Modelo de aprendizaje con Q-Learning aproximado.
tiempos.lisp				Ejemplo de medición de tiempo de respuesta.
util.lisp					Funciones para analizar juegos.

Los archivos marcados con (*) constituyen el modelo.  Los demás son auxiliares.

2. MODO DE OPERACIÓN

2.1 Al iniciar el ambiente Lisp (CLisp o SBCL por ejemplo) se debe cargar el programa base, "td_agent.lisp":

(load "td_agent.lisp")

el cual inicializa diferentes variables y estructuras.  Se trabaja con una red de estrategias global llamada *Q*, inicialmente vacía.

2.2 Para agregar un conjunto (modificable) de estrategias iniciales (para que la red no esté vacía y se puedan generar estimaciones a priori de similitud) se ejecuta la instrucción:

(estrategiasiniciales)

2.3 Para iniciar el entrenamiento self-play se escribe la instrucción:

(iniciar 'q N)

donde N es un número natural.  Se ejecutan N episodios de Q-learning, los cuales realizan cambios persistentes en *Q*.  Por ejemplo para 100 episodios: (iniciar 'q 100).

3. RESPALDO EN TEXTO DE UNA RED DE ESTRATEGIAS

Al finalizar un entrenamiento puede ser deseable guardar la red en un archivo de texto para luego reconstruirla.  Para esto se escribe la instrucción:

(guardarRed R archivo)

donde R es el nombre del símbolo con la red de estrategias y archivo es un nombre entre comillas.  Por ejemplo, para guardar *Q* en "red.dat":

(guardarRed *Q* "red.dat").

El archivo generado es legible en un editor de texto pero puede ser muy largo.  Para reconstruir la red en una sesión posterior se ejecuta la instrucción:

(setq X (cargarRed archivo))

donde X es el símbolo deseado y archivo es un nombre entre comillas.  Por ejemplo:

(setq L (cargarRed "red.dat"))

carga la red guardada en una lista llamada L.

3.1 Formato de escritura

La red de estrategias se guarda en formato de lista, donde cada estrategia completa es un elemento.  Al final de cada estrategia se escribe también su VALOR ESPERADO PROMEDIO, requerido para reconstruir la red dado que este es el objetivo del entrenamiento.  La lista retornada por la función cargarRed se puede utilizar para construir una red nueva con el contenido anterior mediante la función:

(cargarER L *Q*)

el cual carga la lista de estrategias L en la red *Q* previamente creada.

4. RESPALDO DEL AMBIENTE DE TRABAJO

Si se desea guardar en un archivo el estado en memoria completo del ambiente Common Lisp (incluyendo variables de estado y listas de propiedades) se puede correr la función (en SBCL):

(guardarMemSBCL archivo)

el cual genera un archivo *binario* con la sesión Lisp completa.  Por ejemplo:

(guardarMemSBCL "sesion.mem")

Para cargar la sesión guardada en un archivo binario se debe iniciar el ambiente de ejecución con la opción respectiva.  En el caso de SBCL:

sbcl --core "sesion.mem"

o bien el archivo correspondiente.
