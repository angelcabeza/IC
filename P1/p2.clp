;;;;;;; JUGADOR DE 4 en RAYA ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;; Version de 4 en raya clásico: Tablero de 6x7, donde se introducen fichas por arriba
;;;;;;;;;;;;;;;;;;;;;;; y caen hasta la posicion libre mas abajo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Hechos para representar un estado del juego

;;;;;;; (Turno M|J)   representa a quien corresponde el turno (M maquina, J jugador)
;;;;;;; (Tablero Juego ?i ?j _|M|J) representa que la posicion i,j del tablero esta vacia (_), o tiene una ficha propia (M) o tiene una ficha del jugador humano (J)

;;;;;;;;;;;;;;;; Hechos para representar estado del analisis
;;;;;;; (Tablero Analisis Posicion ?i ?j _|M|J) representa que en el analisis actual la posicion i,j del tablero esta vacia (_), o tiene una ficha propia (M) o tiene una ficha del jugador humano (J)
;;;;;;; (Sondeando ?n ?i ?c M|J)  ; representa que estamos analizando suponiendo que la ?n jugada h sido ?i ?c M|J
;;;

;;;;;;;;;;;;; Hechos para representar una jugadas

;;;;;;; (Juega M|J ?columna) representa que la jugada consiste en introducir la ficha en la columna ?columna 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIALIZAR ESTADO


(deffacts Estado_inicial
(Tablero Juego 1 1 _) (Tablero Juego 1 2 _) (Tablero Juego 1 3 _) (Tablero Juego  1 4 _) (Tablero Juego  1 5 _) (Tablero Juego  1 6 _) (Tablero Juego  1 7 _)
(Tablero Juego 2 1 _) (Tablero Juego 2 2 _) (Tablero Juego 2 3 _) (Tablero Juego 2 4 _) (Tablero Juego 2 5 _) (Tablero Juego 2 6 _) (Tablero Juego 2 7 _)
(Tablero Juego 3 1 _) (Tablero Juego 3 2 _) (Tablero Juego 3 3 _) (Tablero Juego 3 4 _) (Tablero Juego 3 5 _) (Tablero Juego 3 6 _) (Tablero Juego 3 7 _)
(Tablero Juego 4 1 _) (Tablero Juego 4 2 _) (Tablero Juego 4 3 _) (Tablero Juego 4 4 _) (Tablero Juego 4 5 _) (Tablero Juego 4 6 _) (Tablero Juego 4 7 _)
(Tablero Juego 5 1 _) (Tablero Juego 5 2 _) (Tablero Juego 5 3 _) (Tablero Juego 5 4 _) (Tablero Juego 5 5 _) (Tablero Juego 5 6 _) (Tablero Juego 5 7 _)
(Tablero Juego 6 1 _) (Tablero Juego 6 2 _) (Tablero Juego 6 3 _) (Tablero Juego 6 4 _) (Tablero Juego 6 5 _) (Tablero Juego 6 6 _) (Tablero Juego 6 7 _)
(Jugada 0)
)

(defrule Elige_quien_comienza
=>
(printout t "Quien quieres que empieze: (escribre M para la maquina o J para empezar tu) ")
(assert (Turno (read)))
)

;;;;;;;;;;;;;;;;;;;;;;; MUESTRA POSICION ;;;;;;;;;;;;;;;;;;;;;;;
(defrule muestra_posicion
(declare (salience 10))
(muestra_posicion)
(Tablero Juego 1 1 ?p11) (Tablero Juego 1 2 ?p12) (Tablero Juego 1 3 ?p13) (Tablero Juego 1 4 ?p14) (Tablero Juego 1 5 ?p15) (Tablero Juego 1 6 ?p16) (Tablero Juego 1 7 ?p17)
(Tablero Juego 2 1 ?p21) (Tablero Juego 2 2 ?p22) (Tablero Juego 2 3 ?p23) (Tablero Juego 2 4 ?p24) (Tablero Juego 2 5 ?p25) (Tablero Juego 2 6 ?p26) (Tablero Juego 2 7 ?p27)
(Tablero Juego 3 1 ?p31) (Tablero Juego 3 2 ?p32) (Tablero Juego 3 3 ?p33) (Tablero Juego 3 4 ?p34) (Tablero Juego 3 5 ?p35) (Tablero Juego 3 6 ?p36) (Tablero Juego 3 7 ?p37)
(Tablero Juego 4 1 ?p41) (Tablero Juego 4 2 ?p42) (Tablero Juego 4 3 ?p43) (Tablero Juego 4 4 ?p44) (Tablero Juego 4 5 ?p45) (Tablero Juego 4 6 ?p46) (Tablero Juego 4 7 ?p47)
(Tablero Juego 5 1 ?p51) (Tablero Juego 5 2 ?p52) (Tablero Juego 5 3 ?p53) (Tablero Juego 5 4 ?p54) (Tablero Juego 5 5 ?p55) (Tablero Juego 5 6 ?p56) (Tablero Juego 5 7 ?p57)
(Tablero Juego 6 1 ?p61) (Tablero Juego 6 2 ?p62) (Tablero Juego 6 3 ?p63) (Tablero Juego 6 4 ?p64) (Tablero Juego 6 5 ?p65) (Tablero Juego 6 6 ?p66) (Tablero Juego 6 7 ?p67)
=>
(printout t crlf)
(printout t ?p11 " " ?p12 " " ?p13 " " ?p14 " " ?p15 " " ?p16 " " ?p17 crlf)
(printout t ?p21 " " ?p22 " " ?p23 " " ?p24 " " ?p25 " " ?p26 " " ?p27 crlf)
(printout t ?p31 " " ?p32 " " ?p33 " " ?p34 " " ?p35 " " ?p36 " " ?p37 crlf)
(printout t ?p41 " " ?p42 " " ?p43 " " ?p44 " " ?p45 " " ?p46 " " ?p47 crlf)
(printout t ?p51 " " ?p52 " " ?p53 " " ?p54 " " ?p55 " " ?p56 " " ?p57 crlf)
(printout t ?p61 " " ?p62 " " ?p63 " " ?p64 " " ?p65 " " ?p66 " " ?p67 crlf)
(printout t  crlf)
)


;;;;;;;;;;;;;;;;;;;;;;; RECOGER JUGADA DEL CONTRARIO ;;;;;;;;;;;;;;;;;;;;;;;
(defrule mostrar_posicion
(declare (salience 9999))
(Turno J)
=>
(assert (muestra_posicion))
)

(defrule jugada_contrario
?f <- (Turno J)
=>
(printout t "en que columna introduces la siguiente ficha? ")
(assert (Juega J (read)))
(retract ?f)
)

(defrule juega_contrario_check_entrada_correcta
(declare (salience 1))
?f <- (Juega J ?c)
(test (and (neq ?c 1) (and (neq ?c 2) (and (neq ?c 3) (and (neq ?c 4) (and (neq ?c 5) (and (neq ?c 6) (neq ?c 7))))))))
=>
(printout t "Tienes que indicar un numero de columna: 1,2,3,4,5,6 o 7" crlf)
(retract ?f)
(assert (Turno J))
)

(defrule juega_contrario_check_columna_libre
(declare (salience 1))
?f <- (Juega J ?c)
(Tablero Juego 1 ?c ?X) 
(test (neq ?X _))
=>
(printout t "Esa columna ya esta completa, tienes que jugar en otra" crlf)
(retract ?f)
(assert (Turno J))
)

(defrule juega_contrario_actualiza_estado
?f <- (Juega J ?c)
?g <- (Tablero Juego ?i ?c _)
(Tablero Juego ?j ?c ?X) 
(test (= (+ ?i 1) ?j))
(test (neq ?X _))
=>
(retract ?f ?g)
(assert (Turno M) (Tablero Juego ?i ?c J))
)

(defrule juega_contrario_actualiza_estado_columna_vacia
?f <- (Juega J ?c)
?g <- (Tablero Juego 6 ?c _)
=>
(retract ?f ?g)
(assert (Turno M) (Tablero Juego 6 ?c J))
)


;;;;;;;;;;; ACTUALIZAR  ESTADO TRAS JUGADA DE CLISP ;;;;;;;;;;;;;;;;;;

(defrule juega_clisp_actualiza_estado
?f <- (Juega M ?c)
?g <- (Tablero Juego ?i ?c _)
(Tablero Juego ?j ?c ?X) 
(test (= (+ ?i 1) ?j))
(test (neq ?X _))
=>
(retract ?f ?g)
(assert (Turno J) (Tablero Juego ?i ?c M))
)

(defrule juega_clisp_actualiza_estado_columna_vacia
?f <- (Juega M ?c)
?g <- (Tablero Juego 6 ?c _)
=>
(retract ?f ?g)
(assert (Turno J) (Tablero Juego 6 ?c M))
)


;;;;;;;;;;; CLISP JUEGA SIN CRITERIO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule elegir_jugada_aleatoria
(declare (salience -9998))
?f <- (Turno M)
=>
(assert (Jugar (random 1 7)))
(retract ?f)
)

(defrule comprobar_posible_jugada_aleatoria
?f <- (Jugar ?c)
(Tablero Juego 1 ?c M|J)
=>
(retract ?f)
(assert (Turno M))
)

(defrule clisp_juega_sin_criterio
(declare (salience -9999))
?f<- (Jugar ?c)
=>
(printout t "JUEGO en la columna (sin criterio) " ?c crlf)
(retract ?f)
(assert (Juega M ?c))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;  Comprobar si hay 4 en linea ;;;;;;;;;;;;;;;;;;;;;

(defrule cuatro_en_linea_horizontal
(declare (salience 9999))
(Tablero ?t ?i ?c1 ?jugador)
(Tablero ?t ?i ?c2 ?jugador) 
(test (= (+ ?c1 1) ?c2))
(Tablero ?t ?i ?c3 ?jugador)
(test (= (+ ?c1 2) ?c3))
(Tablero ?t ?i ?c4 ?jugador)
(test (= (+ ?c1 3) ?c4))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador horizontal ?i ?c1))
)

(defrule cuatro_en_linea_vertical
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i1 ?c ?jugador)
(Tablero ?t ?i2 ?c ?jugador)
(test (= (+ ?i1 1) ?i2))
(Tablero ?t ?i3 ?c  ?jugador)
(test (= (+ ?i1 2) ?i3))
(Tablero ?t ?i4 ?c  ?jugador)
(test (= (+ ?i1 3) ?i4))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador vertical ?i1 ?c))
)

(defrule cuatro_en_linea_diagonal_directa
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i ?c ?jugador)
(Tablero ?t ?i1 ?c1 ?jugador)
(test (= (+ ?i 1) ?i1))
(test (= (+ ?c 1) ?c1))
(Tablero ?t ?i2 ?c2  ?jugador)
(test (= (+ ?i 2) ?i2))
(test (= (+ ?c 2) ?c2))
(Tablero ?t ?i3 ?c3  ?jugador)
(test (= (+ ?i 3) ?i3))
(test (= (+ ?c 3) ?c3))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador diagonal_directa ?i ?c))
)

(defrule cuatro_en_linea_diagonal_inversa
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i ?c ?jugador)
(Tablero ?t ?i1 ?c1 ?jugador)
(test (= (+ ?i 1) ?i1))
(test (= (- ?c 1) ?c1))
(Tablero ?t ?i2 ?c2  ?jugador)
(test (= (+ ?i 2) ?i2))
(test (= (- ?c 2) ?c2))
(Tablero ?t ?i3 ?c3  ?jugador)
(test (= (+ ?i 3) ?i3))
(test (= (- ?c 3) ?c3))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador diagonal_inversa ?i ?c))
)

;;;;;;;;;;;;;;;;;;;; DESCUBRE GANADOR
(defrule gana_fila
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador horizontal ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la fila " ?i crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_columna
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador vertical ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la columna " ?c crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_diagonal_directa
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador diagonal_directa ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la diagonal que empieza la posicion " ?i " " ?c   crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_diagonal_inversa
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador diagonal_inversa ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la diagonal hacia arriba que empieza la posicin " ?i " " ?c   crlf)
(retract ?f)
(assert (muestra_posicion))
) 


;;;;;;;;;;;;;;;;;;;;;;;  DETECTAR EMPATE

(defrule empate
(declare (salience -9999))
(Turno ?X)
(Tablero Juego 1 1 M|J)
(Tablero Juego 1 2 M|J)
(Tablero Juego 1 3 M|J)
(Tablero Juego 1 4 M|J)
(Tablero Juego 1 5 M|J)
(Tablero Juego 1 6 M|J)
(Tablero Juego 1 7 M|J)
=>
(printout t "EMPATE! Se ha llegado al final del juego sin que nadie gane" crlf)
)

;;;;;;;;;;;;;;;;;;;;;; CONOCIMIENTO EXPERTO ;;;;;;;;;;
;;;;; ¡¡¡¡¡¡¡¡¡¡ Añadir conocimiento para que juege como vosotros jugariais !!!!!!!!!!!!

;EJERCICIO 2

;; En esta regla vamos a ver en qué fila caería la ficha si la intentamos
;; meter en una columna determinada
(defrule donde_caeria
    (declare (salience 99))
    ;; si es mi turno (el de la máquina)
    (Turno M)
    ;; Sacamos las jugadas
    (Tablero Juego ?f1 ?c ?v1)
    (Tablero Juego ?f2 ?c ?v2)
    ;; Comprobamos si la fila que jugamos es la de abajo del todo
    ;; o si hay una ficha debajo
    (test (or (= ?f1 6) (= (+ ?f1 1) ?f2)))
    ;; Comprobamos que esas casillas están vacías (o que la de abajo está rellena)
    (test (or (and ( = ?f1 6) (eq ?v1 _)) (and (eq ?v1 _) (neq ?v2 _))))
    =>
    (assert (caeria ?f1 ?c))
)


;EJERCICIO 3

;; En este ejercicio vamos a comprobar si el jugador tiene un dos en raya
;; Solo voy a comentar la primera regla porque las demás son iguales simplemente
;; cambia el procesamiento
(defrule dos_linea_horizontal
    (declare (salience 90))
    ;; Si es mi turno
    (Turno M)
    ;; Saco las jugadas
    (Tablero Juego ?f1 ?c1 ?v1)
    (Tablero Juego ?f2 ?c2 ?v2)
    ;; Compruebo que estan en la misma fila las dos y que una está encima
    ;; de otra y además que tienen el mismo valor (J, _ o M)
    (test (and (= ?f1 ?f2) (and (= ?c1 (+ ?c2 1)) (eq ?v1 ?v2))))
    ;; Comprobamos que hay ficha en en v1
    (test (not (eq ?v1 _)))
    =>
    (assert (conectado_2 Juego h ?f1 ?c1 ?f2 ?c2 ?v1))
)

(defrule dos_linea_vertical
    (declare (salience 90))
    (Turno M)
    (Tablero Juego ?f1 ?c1 ?v1)
    (Tablero Juego ?f2 ?c2 ?v2)
    (test (and (= ?c1 ?c2) (and (= ?f1 (+ ?f2 1)) (eq ?v1 ?v2))))
    (test (not (eq ?v1 _)))
    =>
    (assert (conectado_2 Juego v ?f1 ?c1 ?f2 ?c2 ?v1))
)

(defrule dos_linea_diagonal
    (declare (salience 90))
    (Turno M)
    (Tablero Juego ?f1 ?c1 ?v1)
    (Tablero Juego ?f2 ?c2 ?v2)
    (test (and (= ?c1 (+ ?c2 1)) (and (= ?f1 (+ ?f2 1)) (eq ?v1 ?v2))))
    (test (not (eq ?v1 _)))
    =>
    (assert (conectado_2 Juego d1 ?f1 ?c1 ?f2 ?c2 ?v1))
)

(defrule dos_linea_diagonal_inversa
    (declare (salience 90))
    (Turno M)
    (Tablero Juego ?f1 ?c1 ?v1)
    (Tablero Juego ?f2 ?c2 ?v2)
    (test (and (= ?c1 (- ?c2 1)) (and (= ?f1 (+ ?f2 1)) (eq ?v1 ?v2))))
    (test (not (eq ?v1 _)))
    =>
    (assert (conectado_2 Juego d2 ?f1 ?c1 ?f2 ?c2 ?v1))
)

;; EJERCICIO 4

;; Este ejercicio es prácticamente igual que el anterior solo que
;; en vez de comprobar si hay dos en raya comprobamos que hay 3

(defrule tres_linea_horizontal
    (declare (salience 80))
    ;; Si es mi turno
    (Turno M)
    ;; Saco las fichas que forman un 2 en raya
    ?x <- (conectado_2 Juego ?forma ?f1 ?c1 ?f2 ?c2 ?j)
    ?y <- (conectado_2 Juego ?forma ?f3 ?c3 ?f4 ?c4 ?j)
    ;Compruebo si están en la misma fila
    (test (and (= ?f2 ?f1) (and (= ?f3 ?f4) (= ?f2 ?f3))))
    ; Compruebo si son contiguas horizontalmente
    (test (and (= ?c1 (+ ?c2 1)) (= ?c3 (+ ?c4 1))))
    (test (= ?c2 ?c3))
    =>
    (assert (3_en_linea Juego h ?f1 ?c1 ?f4 ?c4 ?j))
    ;; Si están en 3 en raya eso significa que también estan
    ;; en 2 en raya por lo que ese conocimiento no nos sirve y lo voy a quitar
    (retract ?x)
    (retract ?y)
)

(defrule tres_linea_vertical
    (declare (salience 80))
    (Turno M)
    ?x <- (conectado_2 Juego ?forma ?f1 ?c1 ?f2 ?c2 ?j)
    ?y <- (conectado_2 Juego ?forma ?f3 ?c3 ?f4 ?c4 ?j)
    ;Compruebo si están en la misma fila
    (test (and (= ?c2 ?c1) (and (= ?c3 ?c4) (= ?c2 ?c3))))
    ; Compruebo si son contiguas verticalmente
    (test (and (= ?f1 (+ ?f2 1)) (= ?f3 (+ ?f4 1))))
    ; Compruebo si la ficha interemedia es la misma
    (test (= ?f2 ?f3))
    =>
    (assert (3_en_linea Juego v ?f1 ?c1 ?f4 ?c4 ?j))
    ;; Si están en 3 en raya eso significa que también estan
    ;; en 2 en raya por lo que ese conocimiento no nos sirve y lo voy a quitar
    (retract ?x)
    (retract ?y)
)

(defrule tres_linea_diagonal
    (declare (salience 80))
    (Turno M)
    ?x <- (conectado_2 Juego ?forma ?f1 ?c1 ?f2 ?c2 ?j)
    ?y <- (conectado_2 Juego ?forma ?f3 ?c3 ?f4 ?c4 ?j)

    ;Compruebo si están en diagonal, y el intermedio es el mismo
    (test (and (= ?c1 (+ ?c2 1)) (and (= ?c3 (+ ?c4 1)) (= ?c2 ?c3))))
    ; Compruebo si son contiguas verticalmente
    (test (and (= ?f1 (+ ?f2 1)) (= ?f3 (+ ?f4 1))))
    (test (= ?f2 ?f3))
    =>
    (assert (3_en_linea Juego d1 ?f1 ?c1 ?f4 ?c4 ?j))
    ;; Si están en 3 en raya eso significa que también estan
    ;; en 2 en raya por lo que ese conocimiento no nos sirve y lo voy a quitar
    (retract ?x)
    (retract ?y)
)

(defrule tres_linea_diagonal_inversa
    (declare (salience 80))
    (Turno M)
    ?x <- (conectado_2 Juego ?forma ?f1 ?c1 ?f2 ?c2 ?j)
    ?y <- (conectado_2 Juego ?forma ?f3 ?c3 ?f4 ?c4 ?j)

    ;Compruebo si están en diagonal, y el intermedio es el mismo
    (test (and (= ?c1 (- ?c2 1)) (and (= ?c3 (- ?c4 1)) (= ?c2 ?c3))))
    ; Compruebo si son contiguas verticalmente
    (test (and (= ?f1 (+ ?f2 1)) (= ?f3 (+ ?f4 1))))
    (test (= ?f2 ?f3))
    =>
    (assert (3_en_linea Juego d2 ?f1 ?c1 ?f4 ?c4 ?j))
    ;; Si están en 3 en raya eso significa que también estan
    ;; en 2 en raya por lo que ese conocimiento no nos sirve y lo voy a quitar
    (retract ?x)
    (retract ?y)
)

;; EJRECICIO 5

;; En este ejercicio vamos a comprobar si puedo ganar de alguna manera

(defrule puedo_ganar_linea_horizontal
	(declare (salience 70))
	(Turno M)
	(3_en_linea Juego h ?f1 ?c1 ?f2 ?c2 ?j)
	(Tablero Juego ?f3 ?c3 ?val)
	; estamos en la misma fila
	(test (and (= ?f2 ?f1) (= ?f3 ?f2) ))

	; las columnas son contiguas
	(test
		(or
			(= ?c1 (+ ?c3 1))
			(or
				(= ?c2 (+ ?c3 1))
				(or
					(= ?c1 (- ?c3 1))
					(= ?c2 (- ?c3 1))
				)
			)
		)
	)
    ;; Si podría caer donde queremos
	(caeria ?f3 ?c3)
	=>
	(printout t ?j " ganaria de forma  h  jugando en " ?c3 crlf)

	(assert (ganaria ?j ?c3))
)

(defrule puedo_ganar_linea_vertical
	(declare (salience 70))
	(Turno M)
	(3_en_linea Juego v ?f1 ?c1 ?f2 ?c2 ?j)
	(Tablero Juego ?f3 ?c3 ?val)
	; estamos en la misma columna
	(test (and (= ?c2 ?c1) (= ?c3 ?c2) ))

	; aqui solo comprobamos si c3 + 1 = f1 o f2, ya que no podemos poner fichas por debajo
	(test (or (= ?f1 (+ ?f3 1)) (= ?f2 (+ ?f3 1))  ) )

	(caeria ?f3 ?c3)
	=>
	(printout t ?j " ganaria de forma  v  jugando en " ?c3 crlf)

	(assert (ganaria ?j ?c3))
)

(defrule puedo_ganar_linea_diagonal
	(declare (salience 70))
	(Turno M)
	(3_en_linea Juego d1 ?f1 ?c1 ?f2 ?c2 ?j)
	(Tablero Juego ?f3 ?c3 ?val)
	(caeria ?f3 ?c3)

	; las filas están en diagonal si ni las filas ni las columnas son iguales

	(test
		(or
			(and (= ?f3 (+ ?f1 1)) (= ?c3 (+ ?c1 1)) )
			(and (= ?f3 (- ?f2 1)) (= ?c3 (- ?c2 1)) )
		)
	)

	=>
	(printout t ?j " ganaria de forma d1  jugando en " ?c3 crlf)

	(assert (ganaria ?j ?c3))

)

(defrule puedo_ganar_linea_diagonal_inversa
	(declare (salience 70))
	(Turno M)
	(3_en_linea Juego d2 ?f1 ?c1 ?f2 ?c2 ?j)
	(Tablero Juego ?f3 ?c3 ?val)
	(caeria ?f3 ?c3)

	; las filas están en diagonal si ni las filas ni las columnas son iguales

	(test
		(or
			(and (= ?f3 (+ ?f1 1)) (= ?c3 (- ?c1 1)) )
			(and (= ?f3 (- ?f2 1)) (= ?c3 (+ ?c2 1)) )
		)
	)

	=>
	(printout t ?j " ganaria de forma d2  jugando en " ?c3 crlf)
	(assert (ganaria ?j ?c3))

)


; Reglas que he creado para que la máquina juegue como juego yo


;; En este bloque de código defino las reglas para comprobar que puedo
;; hacer un 3 en línea

(defrule puede_hacer_3_linea_h
	(declare (salience 60))
    ;; Si es mi turno
	(Turno M)
    ;; Saco las fichas donde haya 2 conectados
	(conectado_2 Juego h ?f1 ?c1 ?f2 ?c2 ?j)
    ;; Sacamos en que columna podría caer
	(caeria ?f1 ?val1)

    ;; Compruebo que la columna en la que podría caer la ficha
    ;; Es adyacente a las columnas en las que ya hay ficha
	(test
		(or
			(= ?val1 (+ ?c1 1))
			(or
				(= ?val1 (+ ?c2 1))
				(or
					(= ?val1 (- ?c1 1))
					(= ?val1 (- ?c2 1))
				)
			)

		)
	)
	=>
	(assert (puede_3 h ?f1 ?val1 ?j))

)



(defrule puede_hacer_3_linea_v
	(declare (salience 60))
	(Turno M)
	(conectado_2 Juego v ?f1 ?c1 ?f2 ?c1 ?j)
	(caeria ?f3 ?c1)
	(test
		(or
			(= ?f3 (- ?f1 1))
			(= ?f3 (- ?f2 1))
		)
	)
	=>
	(assert (puede_3 v ?f3 ?c1 ?j))

)

(defrule puede_hacer_3_linea_d1
	(declare (salience 60))
	(Turno M)
	(conectado_2 Juego d1 ?f1 ?c1 ?f2 ?c2 ?j)
	(caeria ?f3 ?c3)
	(test
		(or
			(and (= ?f3 (+ ?f1 1)) (= ?c3 (+ ?c1 1)) )
			(and (= ?f3 (- ?f2 1)) (= ?c3 (- ?c2 1)) )
		)
	)
	=>
	(assert (puede_3 d1 ?f3 ?c3 ?j))

)

(defrule puede_hacer_3_linea_d2
	(declare (salience 60))
	(Turno M)
	(conectado_2 Juego d1 ?f1 ?c1 ?f2 ?c2 ?j)
	(caeria ?f3 ?c3)
	(test
		(or
			(and (= ?f3 (+ ?f1 1)) (= ?c3 (- ?c1 1)) )
			(and (= ?f3 (- ?f2 1)) (= ?c3 (+ ?c2 1)) )
		)
	)
	=>
	(assert (puede_3 d2 ?f3 ?c3 ?j))

)


; En esta regla compruebo si puedo ganar, si puedo juego esa jugada
(defrule M_juega_ganar
	(declare (salience 50))
	(Turno M)
	(ganaria M ?c)
	(not (Juega M ?x))
	=>
	(printout t "JUEGO en la columna " ?c crlf)
	(assert (Juega M ?c))
)

; si no puede ganar, le quita el 4 en raya al contrario
(defrule M_juega_J_no_gane
	(declare (salience 49))
	(Turno M)
	(ganaria J ?c)
	(not (Juega M ?x))
	=>
	(printout t "JUEGO en la columna " ?c crlf)
	(assert (Juega M ?c))
)


; si no puede ganar ni la máquina ni el jugador, evitamos que el contrario pueda hacer 3 en linea
(defrule M_juega_J_no_tenga_3
	(declare (salience 48))
	(Turno M)
	(puede_3 ?forma ?f ?c J)
	(not (Juega M ?x))
	=>
	(printout t "JUEGO en la columna " ?c crlf)
	(assert (Juega M ?c))
)

; si el contrario no puede unir 3, intentamos unirlas nosotros
(defrule M_juega_tener_3
	(declare (salience 47))
	(Turno M)
	(puede_3 ?forma ?f ?c M)
	(not (Juega M ?x))
	=>
	(printout t "JUEGO en la columna " ?c crlf)
	(assert (Juega M ?c))
)


; en caso de que no podamos hacer nada de lo anterior, jugamos en las casillas del centro

(defrule intenta_dominar_centro
	(declare (salience 12))
	(Turno M)
	(caeria ?f ?c)
	(test
		; intentamos dominar las columnas 3, 4 y 5
		(and
			(> ?c 2)
			(< ?c 6)
		)
	)
	(not (Juega M ?x))
	=>
	(printout t "JUEGO en la columna " ?c crlf)
	(assert (Juega M ?c))


)


; reglas para limpiar hechos que pueden variar cada turno

(defrule limpiar_ganaria
	(declare (salience 10))
	(Turno M)
	?x <- (ganaria ?j ?c)
	=>
	(retract ?x)
)

(defrule limpiar_caeria
	(declare (salience 10))
	(Turno M)
	?x <- (caeria ?f ?c)
	=>
	(retract ?x)
)


; una vez limpio, paso de turno (Juega M ya está en la base de hechos, pero tiene
; menos prioridad que los hechos de Turno M, así que elimino Turno M)

(defrule pasar_turno
	(declare (salience 5))
	?x <-(Turno M)
	=>
	(retract ?x)
)