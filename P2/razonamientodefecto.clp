;HECHOS
(deffacts datos
    (ave gorrion)
    (ave paloma)
    (ave aguila)
    (ave pinguino)
    (mamifero vaca)
    (mamifero perro)
    (mamifero caballo)
    (vuela pinguino no seguro)
)

; REGLAS SEGURAS

;Las aves son animales
(defrule aves_son_animales
    (ave ?x)
    =>
    (assert (animal ?x))
    (bind ?expl (str-cat "sabemos que un " ?x " es un animal porque las aves son un tipo de animal"))
    (assert(explicacion animal ?x ?expl))
)

; Los mamiferos son animales (A3)
(defrule mamiferos_son_animales
    (mamifero ?x)
    =>
    (assert (animal ?x))
    (bind ?expl (str-cat "sabemos que un " ?x " es un animal porque los mamiferos son un tipo de animal"))
    (assert (explicacion animal ?x ?expl))    
)

; REGLAS POR DEFECTO
; Casi todas las aves vuelan

(defrule ave_vuela_por_defecto
    (declare (salience -1))
    (ave ?x)
    => 
    (assert (vuela ?x si por_defecto))
    (bind ?expl (str-cat "asumo que un " ?x " vuela porque casi todas las aves vuelan"))
    (assert (explicacion vuela ?x ?expl))  
)

; La mayor parte de los animales no vuelan -> puede interesarme asumir por defecto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; que un animal no va a volar
(defrule mayor_parte_animales_no_vuelan
    (declare (salience -2))
    (animal ?x)
    (not (vuela ?x ? ?))
    =>
    (assert (vuela ?x no por_defecto))
    (bind ?expl (str-cat "asumo que " ?x " no vuela, porque la mayor parte de los animales no vuelan"))
    (assert (explicacion vuela ?x ?expl))  

)

(defrule preguntar_animal
    (declare (salience -5))
    (not (ha_preguntado ?))
    =>
    (printout t "Di un animal ")
    (bind ?anim (read))
    (assert (ha_preguntado ?anim))
)

(defrule animal_en_base
    (declare (salience -10))
    (ha_preguntado ?anim)
    (animal ?anim)
    (explicacion vuela ?anim ?expl)
    (vuela ?anim ?si_no ?estoy_seguro)
    =>
    (printout t ?anim " " ?si_no " vuela ya que " ?expl crlf)
)

(defrule pregunta_ave_mamifero
    (declare (salience -11))
    (ha_preguntado ?anim)
    (not (animal ?anim))
    (not (tipo ?anim ?x))
    =>
    (printout t ?anim " es un mamifero o un ave ? [mamifero/ave/no_se]")
    (assert 
        (tipo ?anim (read))
    )
)

(defrule comprobar_respuesta_ave_mamifero
    (declare (salience 99))
    ?x <- (tipo ?anim ?respuesta)
    (test 
        (and (neq ?respuesta mamifero) (and (neq ?respuesta ave) (neq ?respuesta no_se)))
    )
    =>
    (printout t "Debe responder literalmente una de estas opciones: [mamifero/ave/no_se]" crlf)
    (retract ?x)
)

(defrule contestacion_ave
    (declare (salience -15))
    ?x <- (tipo ?anim ?respuesta)
    (test (eq ?respuesta ave))
    =>
    (assert (ave ?anim))
    (retract ?x)
)

(defrule contestacion_mamifero
    (declare (salience -15))
    ?x <- (tipo ?anim ?respuesta)
    (test (eq ?respuesta mamifero))
    =>
    (assert (mamifero ?anim))
    (retract ?x)
)

(defrule contestacion_nose
    (declare (salience -15))
    ?x <- (tipo ?anim ?respuesta)
    (test (eq ?respuesta no_se))
    =>
    (assert (animal ?anim))
    (retract ?x)
)