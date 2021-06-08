;; RECOMENDADOR DE RAMA POR ÁNGEL CABEZA MARTÍN;; 

;; PARA REALIZAR ESTE EJERCICIO HE ESTABLECIDO UN SISTEMA DE PUNTOS EN EL QUE SEGÚN LO QUE EL USUARIO RESPONDA 
;; SE VAN SUMANDO MÁS O MENOS PUNTOS A CADA RAMA, ADEMÁS SI CONTESTAS ALGO CLAVE, SE TE HACEN PREGUNTAS
;; QUE ESTÁN ENFOCADAS A UNA RAMA EN CONCRETO. LAS RESPUESTAS CLAVES SON:
;; TE GUSTA MATES? ---> ME ENCANTA --> PREGUNTA EXCLUSIVA CSI
;; TE GUSTA HARDWWARE ---> ME ENCANTA --> RECOMENDAR HARDWARE
;; TE GUSTA PROGRAMAR ---> ME ENCANTA --> PREGUNTA EXCLUSIVA SI
;; PUNTUACION SI > 100 ---> TE GUSTAN LAS BASES DE DATOS? --> ME ENCANTA ---> RECOMENDAR SI 


;Voy a definir las puntuaciones de cada rama según mis criterios
(deffacts puntuaciones_CSI
    (puntuacion csi gusta_matematicas me_encanta 120)
	(puntuacion csi gusta_matematicas lo_soporto 30)
	(puntuacion csi gusta_matematicas lo_odio -100)
	(puntuacion csi gusta_matematicas no_se 0)

	(puntuacion csi gusta_ia me_encanta 120)
	(puntuacion csi gusta_ia lo_soporto 30)
	(puntuacion csi gusta_ia lo_odio -120)
	(puntuacion csi gusta_ia no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE SI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion csi gusta_bd me_encanta 0)
	(puntuacion csi gusta_bd lo_soporto 0)
	(puntuacion csi gusta_bd lo_odio 0)
	(puntuacion csi gusta_bd no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE IS EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion csi gusta_estruct me_encanta 0)
	(puntuacion csi gusta_estruct lo_soporto 0)
	(puntuacion csi gusta_estruct lo_odio 0)
	(puntuacion csi gusta_estruct no_se 0)

	(puntuacion csi gusta_programar me_encanta 50)
	(puntuacion csi gusta_programar lo_soporto 30)
	(puntuacion csi gusta_programar lo_odio -30)
	(puntuacion csi gusta_programar no_se 0)

	(puntuacion csi gusta_hardware me_encanta -70)
	(puntuacion csi gusta_hardware lo_soporto -20)
	(puntuacion csi gusta_hardware lo_odio 30)
	(puntuacion csi gusta_hardware no_se 0)

	(puntuacion csi nota_media alta 30)
	(puntuacion csi nota_media media 15)
	(puntuacion csi nota_media baja 0)
	(puntuacion csi nota_media no_quiero_contestar 0)

	(puntuacion csi gustaria_trabajar docencia 20)
	(puntuacion csi gustaria_trabajar investigacion 30)
	(puntuacion csi gustaria_trabajar empresa_privada 15)
	(puntuacion csi gustaria_trabajar empresa_publica 15)
	(puntuacion csi gustaria_trabajar no_quiero_contestar 0)

	(puntuacion csi trabajador mucho 30)
	(puntuacion csi trabajador algo 10)
	(puntuacion csi trabajador nada -10)
	(puntuacion csi trabajador no_quiero_contestar 0)
)

(deffacts puntuaciones_IS
	; puntuacion_is para cada pregunta
	(puntuacion is gusta_matematicas me_encanta -60)
	(puntuacion is gusta_matematicas lo_soporto -20)
	(puntuacion is gusta_matematicas lo_odio 50)
	(puntuacion is gusta_matematicas no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE CSI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion is gusta_ia me_encanta 0)
	(puntuacion is gusta_ia lo_soporto 0)
	(puntuacion is gusta_ia lo_odio 0)
	(puntuacion is gusta_ia no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE SI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion is gusta_bd me_encanta 0)
	(puntuacion is gusta_bd lo_soporto 0)
	(puntuacion is gusta_bd lo_odio 0)
	(puntuacion is gusta_bd no_se 0)

	(puntuacion is gusta_estruct me_encanta 120)
	(puntuacion is gusta_estruct lo_soporto 50)
	(puntuacion is gusta_estruct lo_odio -70)
	(puntuacion is gusta_estruct no_se 0)

	(puntuacion is gusta_programar me_encanta 100)
	(puntuacion is gusta_programar lo_soporto 40)
	(puntuacion is gusta_programar lo_odio -70)
	(puntuacion is gusta_programar no_se 0)

	(puntuacion is gusta_hardware me_encanta -80)
	(puntuacion is gusta_hardware lo_soporto -20)
	(puntuacion is gusta_hardware lo_odio 30)
	(puntuacion is gusta_hardware no_se 0)

	(puntuacion is nota_media alta 20)
	(puntuacion is nota_media media 20)
	(puntuacion is nota_media baja 10)
	(puntuacion is nota_media no_quiero_contestar 0)

	(puntuacion is gustaria_trabajar docencia 40)
	(puntuacion is gustaria_trabajar investigacion 30)
	(puntuacion is gustaria_trabajar empresa_privada 20)
	(puntuacion is gustaria_trabajar empresa_publica 30)
	(puntuacion is gustaria_trabajar no_quiero_contestar 0)

	(puntuacion is trabajador mucho 30)
	(puntuacion is trabajador algo 10)
	(puntuacion is trabajador nada -20)
	(puntuacion is trabajador no_quiero_contestar 0)
)

(deffacts puntuaciones_IC
	; puntuacion_csies para cada pregunta
	(puntuacion ic gusta_matematicas me_encanta 20) ; para CSI o IS
	(puntuacion ic gusta_matematicas lo_soporto 10) ; IS o CSI
	(puntuacion ic gusta_matematicas lo_odio -10) ; ramas sin matemáticas: IC, TI o SI
	(puntuacion ic gusta_matematicas no_se 0) ; no se que hacer

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE CSI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion ic gusta_ia me_encanta 0)
	(puntuacion ic gusta_ia lo_soporto 0)
	(puntuacion ic gusta_ia lo_odio 0)
	(puntuacion ic gusta_ia no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE SI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion ic gusta_bd me_encanta 0)
	(puntuacion ic gusta_bd lo_soporto 0)
	(puntuacion ic gusta_bd lo_odio 0)
	(puntuacion ic gusta_bd no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE IS EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion ic gusta_estruct me_encanta 0)
	(puntuacion ic gusta_estruct lo_soporto 0)
	(puntuacion ic gusta_estruct lo_odio 0)
	(puntuacion ic gusta_estruct no_se 0)

	(puntuacion ic gusta_programar me_encanta 20)
	(puntuacion ic gusta_programar lo_soporto 10)
	(puntuacion ic gusta_programar lo_odio 0)
	(puntuacion ic gusta_programar no_se 0)

	(puntuacion ic gusta_hardware me_encanta 160)
	(puntuacion ic gusta_hardware lo_soporto 40)
	(puntuacion ic gusta_hardware lo_odio -70)
	(puntuacion ic gusta_hardware no_se 0)

	(puntuacion ic nota_media alta 40)
	(puntuacion ic nota_media media 30)
	(puntuacion ic nota_media baja 10)
	(puntuacion ic nota_media no_quiero_contestar 0)

	(puntuacion ic gustaria_trabajar docencia 20)
	(puntuacion ic gustaria_trabajar investigacion 30)
	(puntuacion ic gustaria_trabajar empresa_privada 20)
	(puntuacion ic gustaria_trabajar empresa_publica 10)
	(puntuacion ic gustaria_trabajar no_quiero_contestar 0)

	(puntuacion ic trabajador mucho 20)
	(puntuacion ic trabajador algo 10)
	(puntuacion ic trabajador nada -10)
	(puntuacion ic trabajador no_quiero_contestar 0)
)

(deffacts puntuaciones_TI
	; puntuacion_csies para cada pregunta
	(puntuacion ti gusta_matematicas me_encanta -60) ; para CSI o IS
	(puntuacion ti gusta_matematicas lo_soporto 10) ; IS o CSI
	(puntuacion ti gusta_matematicas lo_odio 60) ; ramas sin matemáticas: IC, TI o SI
	(puntuacion ti gusta_matematicas no_se 0) ; no se que hacer

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE CSI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion ti gusta_ia me_encanta 0)
	(puntuacion ti gusta_ia lo_soporto 0)
	(puntuacion ti gusta_ia lo_odio 0)
	(puntuacion ti gusta_ia no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE SI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion ti gusta_bd me_encanta 0)
	(puntuacion ti gusta_bd lo_soporto 0)
	(puntuacion ti gusta_bd lo_odio 0)
	(puntuacion ti gusta_bd no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE IS EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion ti gusta_estruct me_encanta 0)
	(puntuacion ti gusta_estruct lo_soporto 0)
	(puntuacion ti gusta_estruct lo_odio 0)
	(puntuacion ti gusta_estruct no_se 0)

	(puntuacion ti gusta_programar me_encanta -30)
	(puntuacion ti gusta_programar lo_soporto 10)
	(puntuacion ti gusta_programar lo_odio 70)
	(puntuacion ti gusta_programar no_se 0)

	(puntuacion ti gusta_hardware me_encanta -10)
	(puntuacion ti gusta_hardware lo_soporto 10)
	(puntuacion ti gusta_hardware lo_odio 30)
	(puntuacion ti gusta_hardware no_se 0)

	(puntuacion ti nota_media alta 5)
	(puntuacion ti nota_media media 20)
	(puntuacion ti nota_media baja 30)
	(puntuacion ti nota_media no_quiero_contestar 0)

	(puntuacion ti gustaria_trabajar docencia 20)
	(puntuacion ti gustaria_trabajar investigacion 30)
	(puntuacion ti gustaria_trabajar empresa_privada 40)
	(puntuacion ti gustaria_trabajar empresa_publica 40)
	(puntuacion ti gustaria_trabajar no_quiero_contestar 0)

	(puntuacion ti trabajador mucho 20)
	(puntuacion ti trabajador algo 20)
	(puntuacion ti trabajador nada 10)
	(puntuacion ti trabajador no_quiero_contestar 0)
)

(deffacts puntuaciones_SI
	; puntuacion_sies para cada pregunta
	(puntuacion si gusta_matematicas me_encanta -40) ; para CSI o IS
	(puntuacion si gusta_matematicas lo_soporto -20) ; IS o CSI
	(puntuacion si gusta_matematicas lo_odio 20) ; ramas sin matemáticas: IC, TI o SI
	(puntuacion si gusta_matematicas no_se 0) ; no se que hacer

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE CSI EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion si gusta_ia me_encanta 0)
	(puntuacion si gusta_ia lo_soporto 0)
	(puntuacion si gusta_ia lo_odio 0)
	(puntuacion si gusta_ia no_se 0)

	(puntuacion si gusta_bd me_encanta 100)
	(puntuacion si gusta_bd lo_soporto 50)
	(puntuacion si gusta_bd lo_odio -50)
	(puntuacion si gusta_bd no_se 0)

	;; ESTA PREGUNTA SE HACE SOLO CUANDO AL USUARIO LE GUSTAN LAS MATEMATICAS
	;; Y SOLO TIENE VALOR PARA LA RAMA DE IS EN EL RESTO DE RAMAS TIENE PUNTUACIÓN 0
	(puntuacion si gusta_estruct me_encanta 0)
	(puntuacion si gusta_estruct lo_soporto 0)
	(puntuacion si gusta_estruct lo_odio 0)
	(puntuacion si gusta_estruct no_se 0)


	(puntuacion si gusta_programar me_encanta 50)
	(puntuacion si gusta_programar lo_soporto 25)
	(puntuacion si gusta_programar lo_odio 10)
	(puntuacion si gusta_programar no_se 0)

	(puntuacion si gusta_hardware me_encanta 5)
	(puntuacion si gusta_hardware lo_soporto 40)
	(puntuacion si gusta_hardware lo_odio 15)
	(puntuacion si gusta_hardware no_se 0)

	(puntuacion si nota_media alta 10)
	(puntuacion si nota_media media 30)
	(puntuacion si nota_media baja 20)
	(puntuacion si nota_media no_se 0)

	(puntuacion si gustaria_trabajar docencia 20)
	(puntuacion si gustaria_trabajar investigacion 30)
	(puntuacion si gustaria_trabajar empresa_privada 40)
	(puntuacion si gustaria_trabajar empresa_publica 30)
	(puntuacion si gustaria_trabajar no_se 0)

	(puntuacion si trabajador mucho 30)
	(puntuacion si trabajador algo 20)
	(puntuacion si trabajador nada 10)
	(puntuacion si trabajador no_quiero_contestar 0)
)

;; REGLAS PARA INICIAR RECOMENDAR RAMA O ASIGNATURAS
(defrule cargarModulo
    (declare (salience 9999))
    (not (modulo ?x))
    =>
        (printout t "Indica si quieres que te recomiende una rama o uan asignatura: [rama/asignatura]: " crlf)
        (bind ?res (read))
        (assert (modulo ?res))
)

(defrule comprobarModuloElegido
    (declare (salience 9999))
    ?x <- (modulo ?valor)
    (not (menu_comprobado))
    =>
    (if (and (neq ?valor rama) (neq ?valor asignatura))
        then
                (printout t "Introduce rama o asignatura " crlf)
                (retract ?x)
        else
                (assert 
                    (menu_comprobado)
                )    
    )
)

;; PARTE A PRACTICA
(defrule comienzo
	(declare (salience 9999))
	(not (comienzo))
	(not (ya_aconsejado))
    (modulo rama)
    =>
        (assert
            (comienzo)
            (recomendacion csi 0)
            (recomendacion is 0)
            (recomendacion ic 0)
            (recomendacion ti 0)
            (recomendacion si 0)        
        )
)

; reglas para hacer las preguntas de las que vamos a extraer el conocimiento para empezar a puntuar
;; Simplemente se comprueba que no lo he preguntado ya, que no le he aconsejado rama ya y que no he
;; hecho todas las preguntas ya (hago lo mismo en todas, es un copia y pega de la primera)
(defrule preguntar_mates
	(declare (salience 6))
    (not (ya_aconsejado))
    (not (gusta matematicas ?x))
    (not (ha_respondido_todo))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)
    =>
    (printout t "Te gustan las matematicas? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
    (assert (gusta matematicas (read)))
)

;; ESTA REGLA SOLO SE LANZA SI TE ENCANTAN LAS MATEMATICAS Y 
;; LA USO PARA QUE SI TAMBIÉN TE GUSTA LA IA ACONSEJARTE CSI Y NO HACERTE MÁS PREGUNTAS
(defrule preguntar_IA
	(declare (salience 6))
	(not (ya_aconsejado))
	(gusta matematicas ?x)
	(not (gusta ia ?x))
	(not (ha_respondido_todo))
	(test (eq ?x me_encanta))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)	=>
	(printout t "Te gusta el mundo de la Inteligencia Artificial? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
	(assert (gusta ia (read)))
)

(defrule preguntar_EstructurarSoft
	(declare (salience 6))
	(not (ya_aconsejado))
	(gusta programar ?x)
	(not (gusta estruct ?x))
	(not (ha_respondido_todo))
	(test (eq ?x me_encanta))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)
	=>
	(printout t "Te gusta estructurar software? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
	(assert (gusta estruct (read)))
)

(defrule preguntar_hardware
	(declare (salience 5))
    (not (ya_aconsejado))
    (not (gusta hardware ?x))
    (not (ha_respondido_todo))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)    =>
    (printout t "Te gusta el hardware? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
    (assert (gusta hardware (read)))
)

(defrule preguntar_programar
	(declare (salience 4))
    (not (ya_aconsejado))
    (not (gusta programar ?x))
    (not (ha_respondido_todo))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)    
	=>
    (printout t "Te gusta programar? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
    (assert (gusta programar (read)))
)

(defrule preguntar_nota
	(declare (salience 3))
    (not (ya_aconsejado))
    (not (nota ?x))
    (not (ha_respondido_todo))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)    
	=>
    (printout t "Cual es tu nota media? [alta/media/baja/no_se]" crlf)
    (assert (nota (read)))
)

(defrule preguntar_trabajo
	(declare (salience 2))
    (not (ya_aconsejado))
    (not (trabajar en ?x))
    (not (ha_respondido_todo))
		(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)
    =>
    (printout t "Donde te gustaria trabjar? [docencia/investigacion/empresa_publica/empresa_privada/no_se]" crlf)
    (assert (trabajar en (read)))
)

;; Esta pregunta solo se lanzará cuando la puntuación en la rama SI sea alta (> 100)
(defrule preguntar_BD
	(declare (salience 6))
	(not (ya_aconsejado))
	(not (gusta bd ?x))
	(not (ha_respondido_todo))
	(recomendacion si ?puntuacion)
	(test (or (> ?puntuacion 100) (eq ?puntuacion 100)))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)
	=>
	(printout t "Te gustan las bases de datos? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
	(assert (gusta bd (read)))
)

(defrule preguntar_trabajador
	(declare (salience 1))
    (not (ya_aconsejado))
    (not (es_trabajador ?x))
    (not (ha_respondido_todo))
	(or 
		(modulo rama)
		(and
			(modulo asignatura)
			(modulo preguntas_usuario)
		)
	)
    =>
    (printout t "Eres trabajador? [mucho/algo/nada/no_quiero_contestar]" crlf)
    (assert (es_trabajador (read)))
)

;; Comprobacion de las entradas para cada pregunta
;; En estas reglas cojo lo que el usuario me ha contestado y compruebo que sea igual
;; a los valores que le he dicho que introduzca, si no es igual los quito de la base de hechos
;; y los vuelvo a preguntar (en todos hago lo mismo lo único que cambia son los valores con los que
;; comparo la cadena que ha introducido el usuario)
(defrule entradaNormal
	(declare (salience 9999))
    (not (ya_aconsejado))
    ?x <- (gusta ?algo ?valor)
    (test (and (neq ?valor me_encanta) (and (neq ?valor lo_soporto) (and (neq ?valor lo_odio) (neq ?valor no_se)))))
    =>
    (printout t "Debe responder me_encanta, lo_soporto, lo_odio o no_se" crlf)
    (retract ?x)
)

(defrule entradaNota
	(declare (salience 9999))
    (not (ya_aconsejado))
    ?x <- (nota ?valor)
    (test (and (neq ?valor alta) (and (neq ?valor media) (and (neq ?valor baja) (neq ?valor no_se)))))
    =>
    (printout t "Debe responder alta, media, baja o no_se" crlf)
    (retract ?x)
)

(defrule entradaTrabajo
	(declare (salience 9999))
    (not (ya_aconsejado))
    ?x <- (trabajar en ?valor)
    (test (and (neq ?valor docencia) (and (neq ?valor empresa_publica) (and (neq ?valor empresa_privada) (and (neq ?valor investigacion) (neq ?valor no_se))))))
    =>
    (printout t "Debe responder docencia, empresa_publica, empresa_privada, investigacion o no_se" crlf)
    (retract ?x)
)

(defrule entradaTrabajador
	(declare (salience 9999))
    (not (ya_aconsejado))
    ?x <- (es_trabajador ?valor)
    (test (and (neq ?valor mucho) (and (neq ?valor algo) (and (neq ?valor nada) (neq ?valor no_quiero_contestar)))))
    =>
    (printout t "Debe responder mucho, algo, poco o no_quiero_contestar" crlf)
    (retract ?x)
)

(defrule comprobarRespuestaTodo
	(declare (salience 9999))
	(es_trabajador ?x1)
	(nota ?x2)
	(gusta matematicas ?x3)
	(gusta hardware ?x4)
	(gusta programar ?x5)
	(trabajar en ?x6)
	(ya_aconsejado)
	=>
	(assert (todoRespondido))
)

;; Con estas reglas voy a añadir la puntuación a las distintas
;; ramas según vaya respondiendo el usuario
;; no hace nada complejo la regla
;; solo comprueba que no he puntuado ya la respuesta
;; y cogee la puntuación que tenía antes y le suma lo que corresponda
;; todas las reglas tienen la misma estructura
(defrule puntuarMates
	(declare (salience 8000))
	(gusta matematicas ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama gusta_matematicas ?val ?valor)
	(not (he_puntuado_mates ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_mates ?rama)
	)
)

(defrule puntuarIA
	(declare (salience 8000))
	(gusta ia ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama gusta_ia ?val ?valor)
	(not (he_puntuado_ia ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_ia ?rama)
	)
)

(defrule puntuarEstructSoftware
	(declare (salience 8000))
	(gusta estruct ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama gusta_estruct ?val ?valor)
	(not (he_puntuado_estruct ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_estruct ?rama)
	)
)

(defrule puntuarBD
	(declare (salience 8000))
	(gusta bd ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama gusta_bd ?val ?valor)
	(not (he_puntuado_bd ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_bd ?rama)
	)
)

(defrule puntuarHardware
	(declare (salience 8000))
	(gusta hardware ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama gusta_hardware ?val ?valor)
	(not (he_puntuado_hardware ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_hardware ?rama)
	)
)

(defrule puntuarProgramar
	(declare (salience 8000))
	(gusta programar ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama gusta_programar ?val ?valor)
	(not (he_puntuado_programar ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_programar ?rama)
	)
)

(defrule puntuarNota
	(declare (salience 8000))
	(nota ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama nota_media ?val ?valor)
	(not (he_puntuado_nota ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_nota ?rama)
	)
)

(defrule puntuarTrabajador
	(declare (salience 8000))
	(es_trabajador ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama trabajador ?val ?valor)
	(not (he_puntuado_trabajador ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_trabajador ?rama)
	)
)

(defrule puntuarTrabajo
	(declare (salience 8000))
	(trabajar en ?val)
	?x <- (recomendacion ?rama ?puntuacion)
	(puntuacion ?rama gustaria_trabajar ?val ?valor)
	(not (he_puntuado_trabajo ?rama))
	=>
	(retract ?x)
	(assert
			(recomendacion ?rama (+ ?puntuacion ?valor))
			(he_puntuado_trabajo ?rama)
	)
)

;; Este conjunto de reglas es el que se encargará de mirar la puntuación
;; en cada rama y si es mayor que 160 añadirá un hecho que hará que 
;; deje de preguntar y te recomendará una rama

(defrule puntuacionAltaCSI
	(declare (salience 7000))
	(recomendacion csi ?puntuacion)
	(test (> ?puntuacion 160))
	(modulo rama)
	=>
	(assert (consejo csi te_gusta_mates_y_ia Angel)
	(ya_aconsejado))
)

(defrule puntuacionAltaIS
	(declare (salience 7000))
	(recomendacion is ?puntuacion)
	(test (> ?puntuacion 160))
	(modulo rama)
	=>
	(assert (consejo is te_programar_y_estructurar_software Angel)
	(ya_aconsejado))
)

(defrule puntuacionAltaSI
	(declare (salience 7000))
	(recomendacion si ?puntuacion)
	(test (> ?puntuacion 160))
	(modulo rama)
	=>
	(assert (consejo si te_gusta_programar_y_gusta_BD Angel)
	(ya_aconsejado))
)

(defrule puntuacionAltaIC
	(declare (salience 7000))
	(recomendacion ic ?puntuacion)
	(test (> ?puntuacion 160))
	(modulo rama)
	=>
	(assert (consejo ic te_gusta_el_hardware Angel)
	(ya_aconsejado))
)

(defrule puntuacionAltaTI
	(declare (salience 7000))
	(recomendacion ti ?puntuacion)
	(test (> ?puntuacion 160))
	(modulo rama)
	=>
	(assert (consejo ti no_te_gusta_programar_ni_hardware_ni_matematicas Angel)
	(ya_aconsejado))
)


;; Los siguientes bloques de código harán lo mismo que el anterior pero cuando ya hemos hecho
;; todas las preguntas. Comprobarán la rama con la puntuación más alta y la recomendarán
;; Parece algo lioso la instrucción test pero simplemente estoy comprobando que el valor de la rama x
;; es mayor que los valores de todas las ramas
(defrule todoRespondidoRecomendarCSI
	(todoRespondido)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
	(modulo rama)
	(test (and
				(> ?val_csi ?val_is)
				(and
					(> ?val_csi ?val_si)
						(and 
							(> ?val_csi ?val_ic)
							(> ?val_csi ?val_ti)
						)
				)
			)
	)
	=>
	(assert (consejo csi te_gustan_mates_y_ia Angel)
	(ya_aconsejado))
)

(defrule todoRespondidoRecomendarIS
	(todoRespondido)
	(not (ya_he_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
	(modulo rama)
	(test (and
				(> ?val_is ?val_csi)
				(and
					(> ?val_is ?val_si)
						(and 
							(> ?val_is ?val_ic)
							(> ?val_is ?val_ti)
						)
				)
			)
	)
	=>
	(assert (consejo is te_gusta_programar Angel)
	(ya_aconsejado))
)

(defrule todoRespondidoRecomendarIC
	(todoRespondido)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
	(modulo rama)
	(test (and
				(> ?val_ic ?val_is)
				(and
					(> ?val_ic ?val_si)
						(and 
							(> ?val_ic ?val_csi)
							(> ?val_ic ?val_ti)
						)
				)
			)
	)
	=>
	(assert (consejo ic te_gusta_hardware Angel)
	(ya_aconsejado))
)

(defrule todoRespondidoRecomendarSI
	(todoRespondido)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
	(modulo rama)
	(test (and
				(> ?val_si ?val_is)
				(and
					(> ?val_si ?val_ic)
						(and 
							(> ?val_si ?val_csi)
							(> ?val_si ?val_ti)
						)
				)
			)
	)
	=>
	(assert (consejo si te_gusta_programar_y_gusta_BD Angel)
	(ya_aconsejado))
)


(defrule todoRespondidoRecomendarTI
	(todoRespondido)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
	(modulo rama)
	(test (and
				(> ?val_ti ?val_is)
				(and
					(> ?val_ti ?val_si)
						(and 
							(> ?val_ti ?val_csi)
							(> ?val_ti ?val_ic)
						)
				)
			)
	)
	=>
	(assert (consejo ti no_te_gusta_programar_ni_hardware_ni_matematicas Angel)
	(ya_aconsejado))
)

(defrule todoRespondidoNoHayRecomendacion
	(declare (salience 0))
	(todoRespondido)
	(not (ya_aconsejado))
	(modulo rama)
	=>
	(printout t "El experto Angel no ha sido capaz de aconsejarte ninguna rama, lo siento" crlf)
	(assert (ya_aconsejado))
)

;;Regla que te dice qué regla te aconsejo el motivo y quién te lo aconseja
(defrule mostrarRamaAconsejada
	(declare (salience 9990))
	(not (ya_aconsejado))
	(consejo ?rama ?motivo ?apodo)
	=>
	(printout t "El experto " ?apodo " te aconseja escoger la rama " ?rama " ya que " ?motivo crlf)
	(assert (ya_aconsejado))
)

;; PARTE B DE LA PRÁCTICA
(deffacts ASIGNATURAS
    ;asignaturas de 1º en el primer cuatrimestre
    (asig FP primero primer_cuatri)
    (asig FS primero primer_cuatri)
    (asig FFT primero primer_cuatri)
    (asig ALEM primero primer_cuatri)
    (asig CA primero primer_cuatri)

    ; asignaturas de 1º en el segundo cuatrimestre
    (asig LMD primero segundo_cuatri)
    (asig MP primero segundo_cuatri)
    (asig ES primero segundo_cuatri)
    (asig TOC primer segundo_cuatri)
    (asig IES primero segundo_cuatri)

    ;asignaturas de 2º en el primer cuatrimestre
    (asig SO segundo primer_cuatri)
    (asig SGD segundo primer_cuatri)
    (asig EC segundo primer_cuatri)
	(asig PDOO segundo primer_cuatri)
	(asig ED segundo primer_cuatri)

	; asginaturas de 2º en el segundo cuatrimestre
	(asig IA segundo segundo_cuatri)
	(asig FBD segundo segundo_cuatri)
	(asig FIS segundo segundo_cuatri)
	(asig ALG segundo segundo_cuatri)
	(asig AC segundo segundo_cuatri)
)

; inicio de la base de conocimiento para preguntar por las asigntauras
(defrule inicioAsignaturas
	(declare (salience 9999))
	(modulo asignatura)
	=>
	(assert
		; comenzamos preguntando las asignaturas candidatas
		(printout t " AAA")
		(modulo preguntar_asignaturas)
	)
)

(defrule preguntaAsignatura1
	(declare (salience 9998))
	(modulo asignatura)
	(modulo preguntar_asignaturas)
	(not (primeraasignatura ?x))
=>
   (printout t "Escriba la asignatura sobre la que tenga dudas [ALEM/CA/FFT/FP/FS/IES/ES/LMD/MP/TOC/PDOO/EC/ED/SO/SCD/IA/FIS/AC/FBD/ALG] " crlf)
   (assert (primeraasignatura (read)))
   
)
   
   ;;;;; Solicitamos el nombre de la segunda persona 
 
(defrule preguntaAsignatura2
	(declare (salience 9998))
	(modulo asignatura)
	(modulo preguntar_asignaturas)
	(primeraasignatura ?x)
	(not (segundaasignatura ?y))
=>
   (printout t "Escriba la asignatura sobre la que tenga dudas [ALEM/CA/FFT/FP/FS/IES/ES/LMD/MP/TOC/PDOO/EC/ED/SO/SCD/IA/FIS/AC/FBD/ALG] " crlf)
   (assert (segundaasignatura (read)))
   (assert (modulo recomendar_defecto))
)

(defrule comprobarAsignatura1
	(declare (salience 9999))
	(modulo preguntar_asignaturas)
	(modulo asignatura)
	?x <- (primeraasignatura ?asig)
	(test 
		(and (neq ?asig ALEM)
		(and (neq ?asig CA)
		(and (neq ?asig FFT)
		(and (neq ?asig FP)
		(and (neq ?asig FS)

		(and (neq ?asig IES)
		(and (neq ?asig ES)
		(and (neq ?asig LMD)
		(and (neq ?asig TOC)
		(and (neq ?asig MP)

		(and (neq ?asig SCD)
		(and (neq ?asig ED)
		(and (neq ?asig EC)
		(and (neq ?asig SO)
		(and (neq ?asig PDOO)

		(and (neq ?asig FBD)
		(and (neq ?asig FIS)
		(and (neq ?asig IA)
		(and (neq ?asig ALG)
		(neq ?asig AC)

	))))))))))))))))))))
	=>
	(printout t "Introduce una asignatura de la lista" crlf)
	(retract ?x)

)

(defrule comprobarAsignatura2
	(declare (salience 9999))
	(modulo preguntar_asignaturas)
	(modulo asignatura)
	?x <- (primeraasignatura ?asig)
	?y <- (segundaasignatura ?asig2)
	(test 
		(and (neq ?asig2 ALEM)
		(and (neq ?asig2 CA)
		(and (neq ?asig2 FFT)
		(and (neq ?asig2 FP)
		(and (neq ?asig2 FS)

		(and (neq ?asig2 IES)
		(and (neq ?asig2 ES)
		(and (neq ?asig2 LMD)
		(and (neq ?asig2 TOC)
		(and (neq ?asig2 MP)

		(and (neq ?asig2 SCD)
		(and (neq ?asig2 ED)
		(and (neq ?asig2 EC)
		(and (neq ?asig2 SO)
		(and (neq ?asig2 PDOO)

		(and (neq ?asig2 FBD)
		(and (neq ?asig2 FIS)
		(and (neq ?asig2 IA)
		(and (neq ?asig2 ALG)
		(neq ?asig2 AC)

	))))))))))))))))))))
	=>
	(printout t "Introduce una asignatura de la lista" crlf)
	(retract ?y)

)

(defrule recomendarDefectoBasicasAsig1
	(declare (salience 9999))
	(modulo asignatura)
	?mod <- (modulo recomendar_defecto)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(and
			(asig ?asig1 primero ?cuatrimestre1)
			(not (asig ?asig2 primero ?cuatrimestre2))
	)

	=>
	(bind ?recomendacion " ya que es de formacion basica y la otra no.")
	(retract ?mod)
	(assert (recomendar ?asig1 ?recomendacion por_defecto)
	(modulo preguntas_usuario))
)


(defrule recomendarDefectoBasicasAsig2
	(declare (salience 9999))
	(modulo asignatura)
	?mod <- (modulo recomendar_defecto)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(and 
		(asig ?asig2 primero ?cuatrimestre1)
		(not (asig ?asig1 primero ?cuatrimestre2))
	)

	=>
	(bind ?recomendacion " ya que es de formacion basica y la otra no".)
	(retract ?mod)
	(assert (recomendar ?asig2 ?recomendacion por_defecto)
	(modulo preguntas_usuario))
)


(defrule recomendarDefectoObligatoriasSueltasAsig1
	(declare (salience 9999))
	(modulo asignatura)
	?mod <- (modulo recomendar_defecto)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)

	(test (or (eq ?asig1 FBD) (or (eq ?asig1 FIS) (eq ?asig1 ED) )))
	(test (and (neq ?asig2 FBD) (and (neq ?asig2 FIS) (neq ?asig2 ED) )))

	=>
	(bind ?recomendacion " ya que aunque no es basica no depende de otras.")
	(retract ?mod)
	(assert (recomendar ?asig1 ?recomendacion por_defecto)
	(modulo preguntas_usuario))
)

(defrule recomendarDefectoObligatoriasSueltasAsig2
	(declare (salience 9999))
	(modulo asignatura)
	?mod <- (modulo recomendar_defecto)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(test (or (eq ?asig2 FBD) (or (eq ?asig2 FIS) (eq ?asig2 ED) )))
	(test (and (neq ?asig1 FBD) (and (neq ?asig1 FIS) (neq ?asig1 ED) )))
	=>
	(bind ?recomendacion " ya que aunque no es basica no depende de otras.")
	(retract ?mod)
	(assert (recomendar ?asig2 ?recomendacion por_defecto)
	(modulo preguntas_usuario))
)

(defrule recomendarPrimeraAsigPorDefecto
	(declare (salience 9998))
	(modulo asignatura)
	?mod <- (modulo recomendar_defecto)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	=>
	(bind ?recomendacion " porque sin informacion adicional da igual la que te cojas, son parecidas.")
	(retract ?mod)
	(assert (recomendar ?asig1 ?recomendacion por_defecto)
	(modulo preguntas_usuario))
)

(defrule cambiarModuloMostrarRecomendaciones
	(declare (salience 9998))
	(modulo asignatura)
	?mod <- (modulo preguntas_usuario)
	(or 
		(ya_aconsejado)
		(todoRespondido)
	)
	=>
	(retract ?mod)
	(assert 
		(modulo mostrar_recomendaciones)	
	)
)

(defrule mostrarRecomendacionesDefecto
	(declare (salience 9999))
	(modulo asignatura)
	(modulo mostrar_recomendaciones)
	?x <- (recomendar ?asig1 ?recomendacion por_defecto)
	=>
	(printout t " Te recomiendo la asignatura " ?asig1 " " ?recomendacion " Esta recomendacion la he hecho por defecto" crlf)
	(retract ?x)
)

(defrule mostrarRecomendacionPregunta
	(declare (salience 9998))
	(modulo asignatura)
	(modulo mostrar_recomendaciones)
	(ya_aconsejado)
	?x <- (recomendar ?asig1 ?recomendacion por_preguntas)
	=>
	(printout t " Te recomiendo la asignatura " ?asig1 " " ?recomendacion " Esta recomendacion la he hecho por las preguntas" crlf)
	(retract ?x)
)

(defrule recomendarMatematicasAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta matematicas ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig1 ALEM) (or (eq ?asig1 LMD) (or (eq ?asig1 CA) (eq ?asig1 ES)))))
	(test (and (neq ?asig2 ALEM) (and (neq ?asig2 LMD) (and (neq ?asig2 CA) (neq ?asig2 ES)))))

	=>
	(bind ?recomendacion " porque te gustan las matematicas y estas asignaturas son las mas importantes")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarMatematicasAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta matematicas ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig2 ALEM) (or (eq ?asig2 LMD) (or (eq ?asig2 CA) (eq ?asig2 ES)))))
	(test (and (neq ?asig1 ALEM) (and (neq ?asig1 LMD) (and (neq ?asig1 CA) (neq ?asig1 ES)))))
	=>
	(bind ?recomendacion " porque te gustan las matematicas y estas asignaturas son las mas importantes")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarHardwareBasicasAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta hardware ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (eq ?asig1 TOC))
	(test (neq ?asig2 TOC))

	=>
	(bind ?recomendacion " porque te gusta el hardware y esta asignatura es basica")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarHardwareBasicasAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta hardware ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (eq ?asig2 TOC))
	(test (neq ?asig1 TOC))

	=>
	(bind ?recomendacion " porque te gusta el hardware y esta asignatura es basica")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarHardwareAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta hardware ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig1 EC) (eq ?asig1 AC)))
	(test (and (neq ?asig2 EC) (neq ?asig2 AC)))
	=>
	(bind ?recomendacion " porque te gusta el hardware ")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarHardwareAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta hardware ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig2 EC) (eq ?asig2 AC)))
	(test (and (neq ?asig1 EC) (neq ?asig1 AC)))
	=>
	(bind ?recomendacion " porque te gusta el hardware ")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarIAAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta ia ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig1 IA) (eq ?asig1 ALG)))
	(test (and (neq ?asig2 IA) (neq ?asig2 ALG)))
	=>
	(bind ?recomendacion " porque te gusta la inteligencia artificial y esta asginatura esta relacionada ")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarIAAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta ia ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig2 IA) (eq ?asig2 ALG)))
	(test (and (neq ?asig1 IA) (neq ?asig1 ALG)))
	=>
	(bind ?recomendacion " porque te gusta la inteligencia artificial y esta asginatura esta relacionada ")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarProgramarBasicasAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta programar ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig1 FP) (eq ?asig1 MP)))
	(test (and (neq ?asig2 FP) (neq ?asig2 MP)))

	=>
	(bind ?recomendacion " porque te gusta programar y esta asignatura es basica")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarProgramarBasicasAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta programar ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig2 FP) (eq ?asig2 MP)))
	(test (and (neq ?asig1 FP) (neq ?asig1 MP)))

	=>
	(bind ?recomendacion " porque te gusta programar y esta asignatura es basica")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarProgramarAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta programar ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig1 PDOO) (or (eq ?asig1 ED) (eq ?asig1 SCD))))
	(test (and (neq ?asig2 PDOO) (and (neq ?asig2 ED) (neq ?asig2 SCD))))

	=>
	(bind ?recomendacion " porque te gusta programar ")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarProgramarAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta programar ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig2 PDOO) (or (eq ?asig2 ED) (eq ?asig2 SCD))))
	(test (and (neq ?asig1 PDOO) (and (neq ?asig1 ED) (neq ?asig1 SCD))))

	=>
	(bind ?recomendacion " porque te gusta programar ")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarEstructSoftwareAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta estruct ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig1 PDOO) (eq ?asig1 FIS)))
	(test (and (neq ?asig2 PDOO) (neq ?asig2 FIS)))

	=>
	(bind ?recomendacion " porque te estructurar software ")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)

)

(defrule recomendarEstructSoftwareAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(gusta estruct ?valor)

	(test (or (eq ?valor me_encanta) (eq ?valor lo_soporto)))

	(test (or (eq ?asig2 PDOO) (eq ?asig2 FIS)))
	(test (and (neq ?asig1 PDOO) (neq ?asig1 FIS)))

	=>
	(bind ?recomendacion " porque te gusta estructurar software ")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)

)

(defrule recomendarNotaAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(nota ?valor)

	(test (or (eq ?valor alta) (eq ?valor media)))

	(test (or (eq ?asig1 ALG) (or (eq ?asig1 SO) (eq ?asig1 FFT))))
	(test (and (neq ?asig2 ALG) (and (neq ?asig2 SO) (neq ?asig2 FFT))))

	=>
	(bind ?recomendacion " porque te nota es alta y estas asignaturas son dificiles ")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)

)

(defrule recomendarNotaAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(nota ?valor)

	(test (or (eq ?valor alta) (eq ?valor media)))

	(test (or (eq ?asig2 ALG) (or (eq ?asig2 SO) (eq ?asig2 FFT))))
	(test (and (neq ?asig1 ALG) (and (neq ?asig1 SO) (neq ?asig1 FFT))))

	=>
	(bind ?recomendacion " porque te nota es alta y estas asignaturas son dificiles ")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)

)

(defrule recomendarEmpresaEnAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(trabajar en ?valor)

	(test (or (eq ?valor empresa_publica) (eq ?valor empresa_privada)))

	(test (or (eq ?asig1 IES) (eq ?asig1 FIS)))
	(test (and (neq ?asig2 IES) (neq ?asig2 FIS)))

	=>
	(bind ?recomendacion " porque quieres trabajar en una empresa ")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarEmpresaEnAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(trabajar en ?valor)

	(test (or (eq ?valor empresa_publica) (eq ?valor empresa_privada)))

	(test (or (eq ?asig2 IES) (eq ?asig2 FIS)))
	(test (and (neq ?asig1 IES) (neq ?asig1 FIS)))

	=>
	(bind ?recomendacion " porque quieres trabajar en una empresa ")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarInvesDocenEnAsig1
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(trabajar en ?valor)

	(test (or (eq ?valor investigacion) (eq ?valor docencia)))

	(test (or (eq ?asig1 IA) (eq ?asig1 SCD)))
	(test (and (neq ?asig2 IA) (neq ?asig2 SCD)))

	=>
	(bind ?recomendacion " porque no quieres trabajar en una empresa ")
	(assert
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarInvesDocenEnAsig2
	(declare (salience 9999))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(trabajar en ?valor)

	(test (or (eq ?valor investigacion) (eq ?valor docencia)))

	(test (or (eq ?asig2 IA) (eq ?asig2 SCD)))
	(test (and (neq ?asig1 IA) (neq ?asig1 SCD)))

	=>
	(bind ?recomendacion " porque no quieres trabajar en una empresa ")
	(assert
		(recomendar ?asig2 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)

(defrule recomendarNadaPreguntas
	(declare (salience 1))
	(modulo asignatura)
	(modulo preguntas_usuario)
	(primeraasignatura ?asig1)
	(segundaasignatura ?asig2)
	(not (ya_aconsejado))

	=>
	(bind ?recomendacion " no se que recomendarte despues de las preguntas, asi que te recomiendo la primera ")
	(assert 
		(recomendar ?asig1 ?recomendacion por_preguntas)
		(ya_aconsejado)
	)
)