;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FUNCIONES
(deffunction encadenado (?fc_antecedente ?fc_regla)
    (if (> ?fc_antecedente 0)
        then
            (bind ?rv (* ?fc_antecedente ?fc_regla))
        else
            (bind ?rv 0)
    )
    ?rv
)

(deffunction combinacion (?fc1 ?fc2)
(if (and (> ?fc1 0) (> ?fc2 0))
    then
        (bind ?rv (-(+ ?fc1 ?fc2) (* ?fc1 ?fc2)))
    else
        (if (and (< ?fc1 0) (< ?fc2 0))
            then
                (bind ?rv (+ (+ ?fc1 ?fc2) (* ?fc1 ?fc2) ))
            else
                (bind ?rv (/ (+ ?fc1 ?fc2) (- 1 (min (abs ?fc1) (abs ?fc2))) ))
        )
        
)
?rv
)

; REGLAS
(defrule comienzo
    (declare (salience 9999))
    (not (comienzo))
    =>
    (assert (comienzo)
        (FactorCerteza problema_bujias si 0 recomendacion1)
        (FactorCerteza problema_bateria si 0 recomendacion2)
        (FactorCerteza motor_llega_gasolina si 0 recomendacion3)
        (FactorCerteza problema_starter si 0 recomendacion4)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; REGLAS PARA MOSTRAR POR PANTALLA EL PROBLEMA CON EL MAYOR FACTOR DE CERTEZA
(defrule bujiasMaxFc
    (not (acabado))
    (FactorCerteza problema_bujias si ?fc1 ?r1)
    (FactorCerteza problema_bateria si ?fc2 ?r2)
    (FactorCerteza motor_llega_gasolina si ?fc3 ?r3)
    (FactorCerteza problema_starter si ?fc4 ?r4)
    (test   (and
                (> ?fc1 ?fc2)
                (and 
                    (> ?fc1 ?fc3)
                    (> ?fc1 ?fc4)
                )
            )
    )
    =>
        (printout t "Su coche ha tenido un problema en las bujias con un factor de certeza de: " ?fc1 " " ?r1 crlf)
        (assert (acabado))
)

(defrule bateriaMaxFc
    (not (acabado))
    (FactorCerteza problema_bujias si ?fc1 ?r1)
    (FactorCerteza problema_bateria si ?fc2 ?r2)
    (FactorCerteza motor_llega_gasolina si ?fc3 ?r3)
    (FactorCerteza problema_starter si ?fc4 ?r4)
    (test   (and
                (> ?fc2 ?fc1)
                (and 
                    (> ?fc2 ?fc3)
                    (> ?fc2 ?fc4)
                )
            )
    )
    =>
        (printout t "Su coche ha tenido un problema con la bateria con un factor de certeza de: " ?fc2 " " ?r2 crlf)
        (assert (acabado))
)

(defrule gasolinaMaxFc
    (not (acabado))
    (FactorCerteza problema_bujias si ?fc1 ?r1)
    (FactorCerteza problema_bateria si ?fc2 ?r2)
    (FactorCerteza motor_llega_gasolina si ?fc3 ?r3)
    (FactorCerteza problema_starter si ?fc4 ?r4)
    (test   (and
                (> ?fc3 ?fc2)
                (and 
                    (> ?fc3 ?fc1)
                    (> ?fc3 ?fc4)
                )
            )
    )
    =>
        (printout t "A su coche le llega la gasolina con un factor de certeza de: " ?fc3 " " ?r3 crlf)
        (assert (acabado))
)

(defrule starterMaxFc
    (not (acabado))
    (FactorCerteza problema_bujias si ?fc1 ?r1)
    (FactorCerteza problema_bateria si ?fc2 ?r2)
    (FactorCerteza motor_llega_gasolina si ?fc3 ?r3)
    (FactorCerteza problema_starter si ?fc4 ?r4)
    (test   (and
                (> ?fc4 ?fc2)
                (and 
                    (> ?fc4 ?fc3)
                    (> ?fc4 ?fc1)
                )
            )
    )
    =>
        (printout t "Su coche ha tenido un problema en el starter con un factor de certeza de: " ?fc4 " " ?r4 crlf)
        (assert (acabado))
)

(defrule noCasuistica
    (declare (salience -9999))
    (not (acabado))
    (FactorCerteza problema_bujias si ?fc1 ?r1)
    (FactorCerteza problema_bateria si ?fc2 ?r2)
    (FactorCerteza motor_llega_gasolina si ?fc3 ?r3)
    (FactorCerteza problema_starter si ?fc4 ?r4)
    =>
        (printout t "El caso que ha puesto no entra dentro de las casuisticas del problema por lo tanto no hay factores de certeza" crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; REGLAS DE LOS FACTORES DE CERTEZA IMPLEMENTADAS EN CLISP
(defrule R1
    (FactorCerteza motor_llega_gasolina si ?f1)
    (FactorCerteza gira_motor si ?f2)
    (test (and (> ?f1 0) (> ?f2 0)))
    =>
    (bind ?recomendacion " porque al motor le llega gasolina y el motor gira")
    (assert (FactorCerteza problema_bujias si (encadenado (* ?f1 ?f2) 0.7) ?recomendacion))
)

(defrule R2 
    (FactorCerteza gira_motor no ?f1)
    =>
    (bind ?recomendacion " porque el motor no gira")
    (assert (FactorCerteza problema_bateria si (encadenado ?f1 0.8) ?recomendacion))

)

(defrule R3 
    (FactorCerteza encienden_las_luces no ?f1)
    =>
    (bind ?recomendacion " porque no se encienden las luces")
    (assert (FactorCerteza problema_bateria si (encadenado ?f1 0.9) ?recomendacion))
)

(defrule R4
    (FactorCerteza hay_gasolina_en_deposito si ?f1)
    =>
    (bind ?recomendacion " porque hay gasolina en el deposito")
    (assert (FactorCerteza motor_llega_gasolina si (encadenado ?f1 0.9) ?recomendacion))
)

(defrule R5 
    (FactorCerteza hace_intentos_arrancar si ?f1)
    =>
    (bind ?recomendacion " porque hace intentos de arrancar")
    (assert (FactorCerteza problema_starter si (encadenado ?f1 -0.6) ?recomendacion))
)

(defrule R6 
    (FactorCerteza hace_intentos_arrancar si ?f1)
    =>
    (assert (FactorCerteza problema_bateria si (encadenado ?f1 0.5)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule combinar 
    (declare (salience 4))
    ?f <- (FactorCerteza ?h ?r ?fc1)
    ?g <- (FactorCerteza ?h ?r ?fc2)
    (test (neq ?fc1 ?fc2))
    =>
        (retract ?f ?g)
        (assert (FactorCerteza ?h ?r (combinacion ?fc1 ?fc2)))
)

(defrule certeza_evidencias
    (declare (salience 5))
    (Evidencia ?e ?r)
    =>
        (assert (FactorCerteza ?e ?r 1.0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preguntas evidencias
(defrule preguntaArrancar
    (declare (salience 10))
    (not (preguntadoArrancar))
    =>
    (printout t "Hace intentos de arrancar? [si/no]" crlf)
    (assert 
        (Evidencia hace_intentos_arrancar (read))
        (preguntadoArrancar)
    )
)

(defrule preguntaGasolina
    (declare (salience 10))
    (preguntadoArrancar)
    (not (preguntadoGasolina))
    =>
    (printout t "Hay gasolina en el deposito? [si/no]" crlf)
    (assert 
        (preguntadoGasolina)
        (Evidencia hay_gasolina_en_deposito (read))
    )
)

(defrule preguntaLuces
    (declare (salience 10))
    (preguntadoArrancar)
    (preguntadoGasolina)
    (not (preguntadoLuces))
    =>
    (printout t "Se encienden las luces? [si/no]" crlf)
    (assert 
        (preguntadoLuces)
        (Evidencia encienden_las_luces (read))
    )
)

(defrule preguntaGiraMotor
    (declare (salience 10))
    (preguntadoArrancar)
    (preguntadoGasolina)
    (preguntadoLuces)
    (not (preguntadoGiraMotor))
    =>
    (printout t "El motor gira? [si/no]" crlf)
    (assert 
        (preguntadoGiraMotor)
        (Evidencia gira_motor (read))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; REGLAS PARA COMPROBAR QUE LA RESPUESTA A LAS PREGUNTAS ES SI O NO
(defrule comprobarPreguntaArrancar
    (declare (salience 50))
    ?y <- (preguntadoArrancar)
    (not (preguntadoGiraMotor))
    (not (preguntadoGasolina))
    (not (preguntadoLuces))
    ?x <- (Evidencia hace_intentos_arrancar ?resp)
    (test (and (neq ?resp si) (neq ?resp no)))
    =>
        (printout t "Debe responder si o no" crlf)
        (retract ?x)
        (retract ?y)
)

(defrule comprobarPreguntaLuces
    (declare (salience 50))
    (preguntadoArrancar)
    (preguntadoGasolina)
    (not (preguntadoGiraMotor))
    ?y <- (preguntadoLuces)
    ?x <- (Evidencia encienden_las_luces ?resp)
    (test (and (neq ?resp si) (neq ?resp no)))
    =>
        (printout t "Debe responder si o no" crlf)
        (retract ?x)
        (retract ?y)
)

(defrule comprobarPreguntaGasolina
    (declare (salience 50))
    (preguntadoArrancar)
    ?y <- (preguntadoGasolina)
    (not (preguntadoGiraMotor))
    (not (preguntadoLuces))
    ?x <- (Evidencia hay_gasolina_en_deposito ?resp)
    (test (and (neq ?resp si) (neq ?resp no)))
    =>
        (printout t "Debe responder si o no" crlf)
        (retract ?x)
        (retract ?y)
)

(defrule comprobarPreguntaGiraMotor
    (declare (salience 50))
    (preguntadoArrancar)
    (preguntadoGasolina)
    ?y <- (preguntadoGiraMotor)
    (preguntadoLuces)
    ?x <- (Evidencia gira_motor ?resp)
    (test (and (neq ?resp si) (neq ?resp no)))
    =>
        (printout t "Debe responder si o no" crlf)
        (retract ?x)
        (retract ?y)
)
