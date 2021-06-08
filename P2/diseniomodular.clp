; ESTAS SON LAS REGLAS QUE HE UTILIZADO PARA MODULARIZAR EL EJERCICIO
; EL RESTO DE CÓDGIO ES UN COPIA Y PEGA DE LA ENTREGA DE LA P1 DE:
; ÁNGEL CABEZA MARTÍN (YO)
; TERESA DEL CARMEN CHECA MARABOTTO

;; REGLAS PARA INICIAR RECOMENDAR RAMA O ASIGNATURAS
(defrule cargarModulo
    (declare (salience 9999))
    (not (modulo ?x))
    =>
        (printout t "Indica si quieres que te recomiende una rama o uan asignatura: [Angel/Teresa]: " crlf)
        (bind ?res (read))
        (assert (modulo ?res))
)

(defrule comprobarModuloElegido
    (declare (salience 9999))
    ?x <- (modulo ?valor)
    (not (menu_comprobado))
    =>
    (if (and (neq ?valor Angel) (neq ?valor Teresa))
        then
                (printout t "Introduce Angel o Teresa " crlf)
                (retract ?x)
        else
                (assert 
                    (menu_comprobado)
                )    
    )
)









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

(defrule comienzo
	(declare (salience 9999))
	(modulo Angel)
	(not (comienzo))
	(not (ya_aconsejado))
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
	(modulo Angel)
    (not (ya_aconsejado))
    (not (gusta matematicas ?x))
    (not (ha_respondido_todo))
    =>
    (printout t "Te gustan las matematicas? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
    (assert (gusta matematicas (read)))
)

;; ESTA REGLA SOLO SE LANZA SI TE ENCANTAN LAS MATEMATICAS Y 
;; LA USO PARA QUE SI TAMBIÉN TE GUSTA LA IA ACONSEJARTE CSI Y NO HACERTE MÁS PREGUNTAS
(defrule preguntar_IA
	(declare (salience 6))
	(modulo Angel)
	(not (ya_aconsejado))
	(gusta matematicas ?x)
	(not (gusta ia ?x))
	(not (ha_respondido_todo))
	(test (eq ?x me_encanta))
	=>
	(printout t "Te gusta el mundo de la Inteligencia Artificial? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
	(assert (gusta ia (read)))
)

(defrule preguntar_EstructurarSoft
	(declare (salience 6))
	(modulo Angel)
	(not (ya_aconsejado))
	(gusta programar ?x)
	(not (gusta estruct ?x))
	(not (ha_respondido_todo))
	(test (eq ?x me_encanta))
	=>
	(printout t "Te gusta estructurar software? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
	(assert (gusta estruct (read)))
)

(defrule preguntar_hardware
	(declare (salience 5))
	(modulo Angel)
    (not (ya_aconsejado))
    (not (gusta hardware ?x))
    (not (ha_respondido_todo))
    =>
    (printout t "Te gusta el hardware? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
    (assert (gusta hardware (read)))
)

(defrule preguntar_programar
	(declare (salience 4))
	(modulo Angel)
    (not (ya_aconsejado))
    (not (gusta programar ?x))
    (not (ha_respondido_todo))
    =>
    (printout t "Te gusta programar? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
    (assert (gusta programar (read)))
)

(defrule preguntar_nota
	(declare (salience 3))
	(modulo Angel)
    (not (ya_aconsejado))
    (not (nota ?x))
    (not (ha_respondido_todo))
    =>
    (printout t "Cual es tu nota media? [alta/media/baja/no_se]" crlf)
    (assert (nota (read)))
)

(defrule preguntar_trabajo
	(declare (salience 2))
	(modulo Angel)
    (not (ya_aconsejado))
    (not (trabajar en ?x))
    (not (ha_respondido_todo))
    =>
    (printout t "Donde te gustaria trabjar? [docencia/investigacion/empresa_publica/empresa_privada/no_se]" crlf)
    (assert (trabajar en (read)))
)

;; Esta pregunta solo se lanzará cuando la puntuación en la rama SI sea alta (> 100)
(defrule preguntar_BD
	(declare (salience 6))
	(modulo Angel)
	(not (ya_aconsejado))
	(not (gusta bd ?x))
	(not (ha_respondido_todo))
	(recomendacion si ?puntuacion)
	(test (or (> ?puntuacion 100) (eq ?puntuacion 100)))
	=>
	(printout t "Te gustan las bases de datos? [me_encanta/lo_soporto/lo_odio/no_se]" crlf)
	(assert (gusta bd (read)))
)

(defrule preguntar_trabajador
	(declare (salience 1))
	(modulo Angel)
    (not (ya_aconsejado))
    (not (es_trabajador ?x))
    (not (ha_respondido_todo))
    =>
    (printout t "Eres trabajador? [mucho/algo/nada/no_quiero_contestar]" crlf)
    (assert (es_trabajador (read)))
	(assert (ha_respondido_todo))
)

;; Comprobacion de las entradas para cada pregunta
;; En estas reglas cojo lo que el usuario me ha contestado y compruebo que sea igual
;; a los valores que le he dicho que introduzca, si no es igual los quito de la base de hechos
;; y los vuelvo a preguntar (en todos hago lo mismo lo único que cambia son los valores con los que
;; comparo la cadena que ha introducido el usuario)
(defrule entradaNormal
	(declare (salience 9999))
	(modulo Angel)
    (not (ya_aconsejado))
    ?x <- (gusta ?algo ?valor)
    (test (and (neq ?valor me_encanta) (and (neq ?valor lo_soporto) (and (neq ?valor lo_odio) (neq ?valor no_se)))))
    =>
    (printout t "Debe responder me_encanta, lo_soporto, lo_odio o no_se" crlf)
    (retract ?x)
)

(defrule entradaNota
	(declare (salience 9999))
	(modulo Angel)
    (not (ya_aconsejado))
    ?x <- (nota ?valor)
    (test (and (neq ?valor alta) (and (neq ?valor media) (and (neq ?valor baja) (neq ?valor no_se)))))
    =>
    (printout t "Debe responder alta, media, baja o no_se" crlf)
    (retract ?x)
)

(defrule entradaTrabajo
	(declare (salience 9999))
	(modulo Angel)
    (not (ya_aconsejado))
    ?x <- (trabajar en ?valor)
    (test (and (neq ?valor docencia) (and (neq ?valor empresa_publica) (and (neq ?valor empresa_privada) (and (neq ?valor investigacion) (neq ?valor no_se))))))
    =>
    (printout t "Debe responder docencia, empresa_publica, empresa_privada, investigacion o no_se" crlf)
    (retract ?x)
)

(defrule entradaTrabajador
	(declare (salience 9999))
	(modulo Angel)
    (not (ya_aconsejado))
    ?x <- (trabajador ?algo ?valor)
    (test (and (neq ?valor mucho) (and (neq ?valor algo) (and (neq ?valor nada) (neq ?valor no_quiero_contestar)))))
    =>
    (printout t "Debe responder mucho, algo, poco o no_quiero_contestar" crlf)
    (retract ?x)
)

(defrule comprobarRespuestaTodo
	(declare (salience 9999))
	(modulo Angel)
	(not (todoRespondido))
	(trabajador ?x1)
	(nota ?x2)
	(gusta matematicas ?x3)
	(gusta hardware ?x4)
	(gusta programar ?x5)
	(trabajar en ?x6)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
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
	(modulo Angel)
	(recomendacion csi ?puntuacion)
	(test (> ?puntuacion 160))
	=>
	(assert (consejo csi te_gusta_mates_y_ia Angel))
)

(defrule puntuacionAltaIS
	(declare (salience 7000))
	(modulo Angel)
	(recomendacion is ?puntuacion)
	(test (> ?puntuacion 160))
	=>
	(assert (consejo is te_programar_y_estructurar_software Angel))
)

(defrule puntuacionAltaSI
	(declare (salience 7000))
	(modulo Angel)
	(recomendacion si ?puntuacion)
	(test (> ?puntuacion 160))
	=>
	(assert (consejo si te_gusta_programar_y_gusta_BD Angel))
)

(defrule puntuacionAltaIC
	(declare (salience 7000))
	(modulo Angel)
	(recomendacion ic ?puntuacion)
	(test (> ?puntuacion 160))
	=>
	(assert (consejo ic te_gusta_el_hardware Angel))
)

(defrule puntuacionAltaTI
	(declare (salience 7000))
	(modulo Angel)
	(recomendacion ti ?puntuacion)
	(test (> ?puntuacion 160))
	=>
	(assert (consejo ti no_te_gusta_programar_ni_hardware_ni_matematicas Angel))
)


;; Los siguientes bloques de código harán lo mismo que el anterior pero cuando ya hemos hecho
;; todas las preguntas. Comprobarán la rama con la puntuación más alta y la recomendarán
;; Parece algo lioso la instrucción test pero simplemente estoy comprobando que el valor de la rama x
;; es mayor que los valores de todas las ramas
(defrule todoRespondidoRecomendarCSI
    (modulo Angel)
	(ha_respondido_todo)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
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
	(assert (consejo csi te_gustan_mates_y_ia Angel))
)

(defrule todoRespondidoRecomendarIS
    (modulo Angel)
	(ha_respondido_todo)
	(not (ya_he_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
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
	(assert (consejo is te_gusta_programar Angel))
)

(defrule todoRespondidoRecomendarIC
    (modulo Angel)
	(ha_respondido_todo)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
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
	(assert (consejo ic te_gusta_hardware Angel))
)

(defrule todoRespondidoRecomendarSI
    (modulo Angel)
	(ha_respondido_todo)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
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
	(assert (consejo si te_gusta_programar_y_gusta_BD Angel))
)


(defrule todoRespondidoRecomendarTI
    (modulo Angel)
	(ha_respondido_todo)
	(not (ya_aconsejado))
	(recomendacion csi ?val_csi)
	(recomendacion is ?val_is)
	(recomendacion si ?val_si)
	(recomendacion ic ?val_ic)
	(recomendacion ti ?val_ti)
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
	(assert (consejo ti no_te_gusta_programar_ni_hardware_ni_matematicas Angel))
)

(defrule todoRespondidoNoHayRecomendacion
	(declare (salience -9999))
	(modulo Angel)
	(ha_respondido_todo)
	(not (ya_aconsejado))
	=>
	(printout t "El experto Angel no ha sido capaz de aconsejarte ninguna rama, lo siento" crlf)
	(assert (ya_aconsejado))
)

;;Regla que te dice qué regla te aconsejo el motivo y quién te lo aconseja
(defrule mostrarRamaAconsejada
	(declare (salience 9990))
	(modulo Angel)
	(not (ya_aconsejado))
	(consejo ?rama ?motivo ?apodo)
	=>
	(printout t "El experto " ?apodo " te aconseja escoger la rama " ?rama " ya que " ?motivo crlf)
	(assert (ya_aconsejado))
)






;;;;PRACTICA 3: RECOMENDACIÓN DE RAMAS ;;;;
;;;;TERESA DEL CARMEN CHECA MARABOTTO ;;;;
;;;;;;;Siguiendo el árbol de clasificacion, se realizaran una serie de preguntas para llegar hasta una decisión sobre la rama que se recomienda 

;;;;;;;Primero, crearemos los hecho de las ramas
(deffacts Ramas
(Rama Computación_y_Sistemas_Inteligentes)
(Rama Ingeniería_del_Software)
(Rama Ingeniería_de_Computadores)
(Rama Sistemas_de_Información)
(Rama Tecnologías_de_la_Información)
)

;;;;;;;Ahora crearemos un template que usaremos para recopilar los consejos que dará el sistema dependiendo de las respuestas del alumno
;;;;;;;Este template tiene la estructura: (Consejo <nombre de la rama> “<texto del motivo>”  “apodo del experto”)

(deftemplate Consejo 
  (slot rama)
  (slot motivo)
  (slot experto)
)

;;;;;;;Como nuestra primera pragunta será si le gustan las matemáticas o no, de forma que podremos clasificar en dos grupos las asignaturas, podemos declarar los primeros 5 consejos (uno para cada rama), puesto que se dará bajo cualquier circunstancia
(deffacts consejos
  (Consejo (rama Computación_y_Sistemas_Inteligentes) (motivo "Te gustan las matematicas") (experto SISTEMA))
  (Consejo (rama Ingeniería_del_Software) (motivo "Te gustan las matematicas") (experto SISTEMA))
  (Consejo (rama Ingeniería_de_Computadores) (motivo "No te gustan las matematicas") (experto SISTEMA))
  (Consejo (rama Sistemas_de_Información) (motivo "No te gustan las matematicas") (experto SISTEMA))
  (Consejo (rama Tecnologías_de_la_Información) (motivo "No te gustan las matematicas") (experto SISTEMA))



)

   
;;;;;;;Nuestra primera pregunta será acerca de si le gustan o no las matemáticas, si nos responde que sí, pasaremos a preguntarle si es trabajador, si nos responde que no, pasamos a preguntarle si le gusta el hardware

(defrule matematicas
	(declare (salience 1000)) 
	(modulo Teresa)
=>
   	(printout t ">>>>>¿Te gustan las matematicas? (SI/NO)" crlf)
   	(assert (mates (read)))

)
 

;;;;;;;Según el arbol de clasificación, podemos preguntarle si es trabajador, tanto si nos responde que sí le gustan las matemáticas, como si nos recponde que no le ha gustado el hardware. 
;;;;;;;Si le gustaban las matemáticas: Si responde MUCHO o NORMAL, pasamos a preguntarle por su nota media del expediente, si nos responde POCO podemos decidir recomendarle la rama de ingeniería del software
;;;;;;;Si no le gustaba el hardware: Si nos reponde MUCHO o NORMAL, pasamos a preguntarle si le gusta programar. Si nos responde POCO, podemos decidir recomendarle la rama de TSI
;;;;;;;Como llegamos a esta pregunta si le gustan las matemáticos o si no le gusta el hardware, pordemos crear los consejos correspondientes para las ramas con las que llegaríamos a esta pregunta. Como el consejo de las matemáticas ya lo tenemos introducido, no hace falta que volvamos a crearlo

(defrule trabajador
	(declare (salience 1000)) 
	(modulo Teresa)
	(or (mates SI) (hardware NO))

=>
   	(printout t ">>>>>¿Eres trabajador? (MUCHO/NORMAL/POCO)" crlf)
   	(assert (trabajador (read)))
   	(assert(Consejo (rama Tecnologías_de_la_Información) (motivo "No te gusta el hardware") (experto SISTEMA)))
   	(assert (Consejo (rama Sistemas_de_Información) (motivo "No te gusta el hardware") (experto SISTEMA)))

)


;;;;;;;Ahora, podemos preguntarle por su nota media del expediente. Esta pregunta sólo nos interesa si le gustan las matemáticas y si es trabajador (MUCHO o NORMAL)
;;;;;;;Como sólo llegamos a esta pregunta con las ramas de CSI y IS, y sabiendo que es trabajador, podemos declarar los respectivos consejos
;;;;;;;Si nos responde ALTA o MEDIA, pasamos a preguntarle si le gusta programar. Si nos responde BAJA, le podemos recomendar la rama de IS

(defrule nota_media
	(declare (salience 1000)) 
	(modulo Teresa)
	(mates SI)
	(or(trabajador MUCHO) (trabajador NORMAL))
=>
  	(printout t ">>>>>¿Como es tu nota media? (ALTA/MEDIA/BAJA)" crlf)
	(assert (nota(read)))
	(assert(Consejo (rama Computación_y_Sistemas_Inteligentes) (motivo "Trabajas bastante") (experto SISTEMA)))
	(assert (Consejo (rama Ingeniería_del_Software) (motivo "Trabajas bastante") (experto SISTEMA)))

)

;;;;;;;Podemos preguntarle si le gusta programar: en el caso de que le hayan gustado las matemáticas, sea trabajados, tenga una nota media normal o alta, o en el caso de que no le gusten las matemáticas, no le guste el hardware y sea trabajador
;;;;;;;Si es el primer caso y nos responde que no, le recomendamos CSI, si nos responde que si, le preguntamos si prefiere las clases teóricas o prácticas
;;;;;;;Si es el segundo caso, nos interesa saber si le gusta programar, puesto que con los datos anteriores, si le gusta programar, le recomendamos SI, si no le gusta, le recomendamos TI
;;;;;;;Al igual que en los anteriores, creamos los respectivos consejos
(defrule programacion
	(declare (salience 1000))
	(modulo Teresa)
	   (or 
	      (or
	         (nota ALTA) 
	         (nota MEDIA)
	      ) 
	      (and 
	         (hardware NO) 
	         (or 
        	    (trabajador MUCHO) 
	            (trabajador NORMAL)
	         )
	      )
	   )
=>
  	(printout t ">>>>>¿Te gusta programar? (SI/NO)" crlf)
	(assert (programar(read)))
	(assert(Consejo (rama Computación_y_Sistemas_Inteligentes) (motivo "Tienes buena nota media") (experto SISTEMA)))
   	(assert (Consejo (rama Ingeniería_del_Software) (motivo "Tienes buena nota media") (experto SISTEMA)))
   	(assert(Consejo (rama Tecnologías_de_la_Información) (motivo "Trabajas bastante") (experto SISTEMA)))
   	(assert (Consejo (rama Sistemas_de_Información) (motivo "Trabajas bastante") (experto SISTEMA)))


)


;;;;;;;Esta pregunta nos interesa hacerla: si le gustanlas matemáticas, es trabajador, tiene buena nota media y le gusta programar, o si no le gustan las matemáticas pero sí el hardware
;;;;;;;Si es el primer caso y nos responde TEORICAS, con la información anterior, podemos recomendarle CSI, si nos responde PRACTICAS, podemos recomendarle IS
;;;;;;;Si es el segundo caso y nos responde TEORICAS, podemos recomendarle la rama de TI, si nos responde PRACTICAS, podemos recomendarle IC
;;;;;;;Como en los anteriores, de los datos anteriores, creamos los respectivos consejos

(defrule teoricas_o_practicas
(declare (salience 1000))
(modulo Teresa)
(or (and (programar SI) (mates SI)) (hardware SI))
=>
   (printout t ">>>>>¿Prefieres las clases teoricas o practicas? (TEORICAS/PRACTICAS)" crlf)
   (assert (teoricas_practicas(read)))
      (assert(Consejo (rama Computación_y_Sistemas_Inteligentes) (motivo "Te gusta programar") (experto SISTEMA)))
   (assert (Consejo (rama Ingeniería_del_Software) (motivo "Te gusta programar") (experto SISTEMA)))
   (assert(Consejo (rama Tecnologías_de_la_Información) (motivo "Te gusta el hardware") (experto SISTEMA)))
   (assert (Consejo (rama Ingeniería_de_Computadores) (motivo "Te gusta el hardware") (experto SISTEMA)))

)


;;;;;;;Por último le preguntamos si le gusta el hardware o no. Esta pregunta nos interesa sólo si no le gustan las matemáticas.
;;;;;;;Si nos responde que sí le gusta el hardware, pasamos a preguntarle si prefiere las asignaturas teóricas o prácticas.
;;;;;;;Si nos responde que no le gusta el hardware, pasamos a preguntarñe si es o no trabajador

(defrule gusta_hardware
(declare (salience 1000))
(modulo Teresa)
(mates NO)
=>
   (printout t ">>>>>¿Te gusta el hardware? (SI/NO)" crlf)
   (assert (hardware(read)))

)


;;;;;;;Con la información anterior, dependiendo de lo que ha respondido el alumno, creamos los consejos que no hemos podido crear antes o que entran en conflicto con los datos anteriores puesto que se puede recomendar una rama por muchas razone distintas, no siempre van a ser las mismas
(defrule check_consejos
(modulo Teresa)
(hardware NO)
(trabajador POCO)

=>
   (assert (Consejo (rama Tecnologías_de_la_Información) (motivo "No te gusta el hardware") (experto SISTEMA)))
   (assert (Consejo (rama Tecnologías_de_la_Información) (motivo "Trabajas poco") (experto SISTEMA)))


)

(defrule check_consejos2
(modulo Teresa)
(hardware NO)
(programar SI)

=>
   (assert (Consejo (rama Sistemas_de_Información) (motivo "No te gusta el hardware") (experto SISTEMA)))
(assert (Consejo (rama Sistemas_de_Información) (motivo "Te gusta programar") (experto SISTEMA)))


)

(defrule check_consejos3
(modulo Teresa)
(hardware SI)
(teoricas_practicas TEORICAS)

=>
   (assert (Consejo (rama Tecnologías_de_la_Información) (motivo "Te gustan mas las clases teoricas que las practicas") (experto SISTEMA)))
)

(defrule check_consejos4
(modulo Teresa)
(hardware SI)
(teoricas_practicas PRACTICAS)

=>
   (assert (Consejo (rama Ingeniería_de_Computadores) (motivo "Te gustan mas las clases practicas que las teoricas") (experto SISTEMA)))
)

(defrule check_consejos5
(modulo Teresa)
(mates SI)
(programar NO)

=>
   (assert (Consejo (rama Computación_y_Sistemas_Inteligentes) (motivo "No te gusta programar") (experto SISTEMA)))
)

(defrule check_consejos6
(modulo Teresa)
(mates SI)
(teoricas_practicas TEORICAS)

=>
   (assert (Consejo (rama Computación_y_Sistemas_Inteligentes) (motivo "Te gustan mas las clases teoricas que las practicas") (experto SISTEMA)))
)


(defrule check_consejos7
(modulo Teresa)
(mates SI)
(teoricas_practicas PRACTICAS)

=>
   (assert (Consejo (rama Ingeniería_del_Software) (motivo "Te gustan mas las clases pacticas que las teoricas") (experto SISTEMA)))
)

(defrule check_consejos8
(modulo Teresa)
(mates SI)
(trabajador POCO)

=>
   (assert (Consejo (rama Ingeniería_del_Software) (motivo "Trabajas poco") (experto SISTEMA)))
)

(defrule check_consejos9
(modulo Teresa)
(mates SI)
(nota BAJA)

=>
   (assert (Consejo (rama Ingeniería_del_Software) (motivo "Tu nota media es baja") (experto SISTEMA)))
)


;;;;;;;Una vez hayamos declarado todos los consejos, podemos pasar a comprobar la información que nos ha dado el alumno. Por cada rama, programamos los distintos caminos que aparecen en el árbol para que, si las respuestas del alumno coinciden con estas, se jaga un assert Recomendar "Rama". Con ello, pasaremos a sacar el mensaje con la rama recomendada

(defrule recomiendo_CSI
	(modulo Teresa)
   (mates SI)
   (or 
      (trabajador MUCHO) 
      (trabajador NORMAL)
    )
   (or 
      (nota ALTA) 
      (nota MEDIA)
   )
   (or 
      (programar NO) 
      (and 
          (programar SI) 
          (teoricas_practicas TEORICAS)
      )
    )

=> 
  (printout t "------Se recomienda realizar la rama de: COMPUTACION Y SISTEMAS INTELIGENTES por los siquientes motivos: " crlf)
  (assert (Recomendar Computación_y_Sistemas_Inteligentes))
)


(defrule recomiendo_IS
    (modulo Teresa)
  (mates SI)
  (or
     (or 
        (trabajador POCO)
        (and
            (or
               (trabajador MUCHO)
               (trabajador NORMAL)
            )
           (nota BAJA)
         )
      )
      (and
         (or
            (nota ALTA)
            (nota MEDIA)
          )
          (and
              (programar SI)
              (teoricas_practicas PRACTICAS)
          )
      )
   )
     
   
=> 
  (printout t "------Se recomienda realizar la rama de: INGENIERIA DEL SOFTWARE por los siquientes motivos: " crlf)
  (assert (Recomendar Ingeniería_del_Software))

)


(defrule recomiendo_CI
    (modulo Teresa)
    (mates NO)
    (hardware SI)
    (teoricas_practicas PRACTICAS)
     
   
=> 
  (printout t "------Se recomienda realizar la rama de: INGENIERIA DE COMPUTADORES por los siquientes motivos: " crlf)
  (assert (Recomendar Ingeniería_de_Computadores))


)



(defrule recomiendo_SI
    (modulo Teresa)
    (mates NO)
    (hardware NO)
    (or 
       (trabajador MUCHO)
       (trabajdor NORMAL)
     )
     (programar SI)
     
   
=> 
  (printout t "------Se recomienda realizar la rama de: SISTEMAS DE INFORMACION por los siquientes motivos: " crlf)
  (assert (Recomendar Sistemas_de_Información))

)

(defrule recomiendo_TI
    (modulo Teresa)
    (mates NO)
    (or
       (and
           (hardware SI)
           (teoricas_practicas TEORICAS)
        )
        (and
            (hardware NO)
            (or
               (trabajador POCO)
               (and
                   (or
                      (trabajador MUCHO)
                      (trabajador NORMAL)
                    )
                    (programar NO)
                )
            )
         )
     )
     
   
=> 
  (printout t "------Se recomienda realizar la rama de: TECNOLOGIAS DE LA INFORMACION por los siquientes motivos: " crlf)
  (assert (Recomendar Tecnologías_de_la_Información))

)

;;;;;;;Una vez sepamos la rama, recorreremos los distintos Consejos creados de forma que saquemos los motivos por los que hemos tomado la decisión de recomendarle una rama u otra.

(defrule justificacion_recomendacion
    (modulo Teresa)
   (Recomendar ?c)
   (Rama ?c)
   (Consejo (rama ?c)  (motivo ?t) (experto SISTEMA))
=>
    (printout t "     - " ?t crlf)

)

;;;;;;;Con la siguiente regla, mostraremos un mensaje de error en la sintaxis, cuando las respuestas del alumno no se ajusten a las opciones que se le ofrecen



(defrule error_mates
	(declare (salience 1))
	(modulo Teresa)
	(mates ?c)
        (test (and (neq ?c SI) (neq ?c NO)))
=>
        (printout t "Tienes que responder SI o NO" crlf)
   	(assert (mates (read)))
)


(defrule error_trabajador
	(declare (salience 1))
	(modulo Teresa)
	(trabajador ?c)
        (test (and (neq ?c MUCHO) (and (neq ?c NORMAL)(neq ?c POCO))))
=>
        (printout t "Tienes que responder MUCHO, NORMAL o POCO" crlf)
   	(assert (trabajador (read)))
)


(defrule error_nota
	(declare (salience 1))
	(modulo Teresa)
	(nota ?c)
        (test (and (neq ?c ALTA) (and (neq ?c MEDIA)(neq ?c BAJA))))
=>
        (printout t "Tienes que responder ALTA, MEDIA o BAJA" crlf)
   	(assert (nota (read)))
)


(defrule error_programacion
	(declare (salience 1))
	(modulo Teresa)
	(programar ?c)
        (test (and (neq ?c SI) (neq ?c NO)))
=>
        (printout t "Tienes que responder SI o NO" crlf)
   	(assert (programar (read)))
)


(defrule error_teo_prac
	(declare (salience 1))
	(modulo Teresa)
	(teoricas_practicas ?c)
        (test (and (neq ?c TEORICAS) (neq ?c PRACTICAS)))
=>
        (printout t "Tienes que responder TEORICAS o PRACTICAS" crlf)
   	(assert (teoricas_practicas (read)))
)


(defrule error_hardware
	(declare (salience 1))
	(modulo Teresa)
	(hardware ?c)
        (test (and (neq ?c SI) (neq ?c NO)))
=>
        (printout t "Tienes que responder SI o NO" crlf)
   	(assert (hardware (read)))
)