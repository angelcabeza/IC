;;;; HECHOS GENERALES DEL SISTEMA ;;;;;
;;;;(seran v�lidos para todas las ejecuciones del sistema ;;;;

; Listado de personas de la familia en cuestion introducidas con la propiedad unaria de hombre o mujer

(deffacts personas
   (hombre Francisco) ; "Francisco es un hombre"
   (hombre Luis)
   (hombre Jaime)
   (hombre Eduardo)
   (hombre JoseIgnacio)
   (hombre Fernando)
   (hombre Victor)
   (hombre Pablo)
   (hombre Julio)
   (hombre JuanFrancisco)
   (mujer Paula)         ; Paula es una mujer
   (mujer Marina)
   (mujer Helena)
   (mujer Ana)
   (mujer Marta)
   (mujer Macarena)
   (mujer Teresa)
   (mujer Carmen) )

(deffacts listarelaciones
   (listarelacion HIJO)
   (listarelacion PADRE)
   (listarelacion ABUELO)
   (listarelacion NIETO)
   (listarelacion HERMANO)
   (listarelacion ESPOSO)
   (listarelacion PRIMO)
   (listarelacion TIO)
   (listarelacion SOBRINO)
   (listarelacion CUNIADO)
   (listarelacion YERNO)
   (listarelacion SUEGRO)
)

(deffacts relacionesGenero
   (relacionmasculino HIJO)
   (relacionmasculino PADRE)
   (relacionmasculino ABUELO)
   (relacionmasculino NIETO)
   (relacionmasculino HERMANO)
   (relacionmasculino ESPOSO)
   (relacionmasculino PRIMO)
   (relacionmasculino TIO)
   (relacionmasculino SOBRINO)
   (relacionmasculino CUNIADO)
   (relacionmasculino YERNO)
   (relacionmasculino SUEGRO)
   (relacionfemenino HIJA)
   (relacionfemenino MADRE)
   (relacionfemenino ABUELA)
   (relacionfemenino NIETA)
   (relacionfemenino HERMANA)
   (relacionfemenino ESPOSA)
   (relacionfemenino PRIMA)
   (relacionfemenino TIA)
   (relacionfemenino SOBRINA)
   (relacionfemenino CUNIADA)
   (relacionfemenino YERNA)
   (relacionfemenino SUEGRA)
)

(deffacts nombres
   (nombre Francisco)
   (nombre Luis)
   (nombre Jaime)
   (nombre Eduardo)
   (nombre JoseIgnacio)
   (nombre Fernando)
   (nombre Victor)
   (nombre Pablo)
   (nombre Julio)
   (nombre JuanFrancisco)
   (nombre Paula)
   (nombre Marina)
   (nombre Helena)
   (nombre Ana)
   (nombre Marta)
   (nombre Macarena)
   (nombre Teresa)
   (nombre Carmen)
)

;;;;; Plantilla t�pica de Relaciones binarias, ajustada a relaciones de parentesco restringiendo los valores de tipo de relacion a estas. Se usa para registrar "El <sujeto> es <tipo de relacion> de <objeto>", por ejemplo "Juan es TIO de Marina" 

(deftemplate Relacion 
  (slot tipo (type SYMBOL) (allowed-symbols HIJO PADRE ABUELO NIETO HERMANO ESPOSO PRIMO TIO SOBRINO  CUNIADO YERNO SUEGRO))
  (slot sujeto)
  (slot objeto))

;;;;; Datos de la relacion HIJO y ESPOSO en mi familia que es suficiente para el problema, pues el resto se deduce de estas

(deffacts relaciones
   (Relacion (tipo HIJO) (sujeto Luis) (objeto Francisco)) ; "Luis es HIJO de Francisco
   (Relacion (tipo HIJO) (sujeto Marina) (objeto Francisco))
   (Relacion (tipo HIJO) (sujeto Francisco) (objeto Fernando))
   (Relacion (tipo HIJO) (sujeto Eduardo) (objeto Fernando))
   (Relacion (tipo HIJO) (sujeto Paula) (objeto JuanFrancisco))
   (Relacion (tipo HIJO) (sujeto Jaime) (objeto JuanFrancisco))
   (Relacion (tipo HIJO) (sujeto Victor) (objeto Jaime))
   (Relacion (tipo HIJO) (sujeto Pablo) (objeto Jaime))
   (Relacion (tipo HIJO) (sujeto Julio) (objeto Jaime))
   (Relacion (tipo HIJO) (sujeto Teresa) (objeto Eduardo))
   (Relacion (tipo HIJO) (sujeto JoseIgnacio) (objeto Eduardo))
   (Relacion (tipo ESPOSO) (sujeto Francisco) (objeto Paula)) ; "Francisco es ESPOSO de Paula"
   (Relacion (tipo ESPOSO) (sujeto Jaime) (objeto Carmen)) 
   (Relacion (tipo ESPOSO) (sujeto JuanFrancisco) (objeto Helena))
   (Relacion (tipo ESPOSO) (sujeto Fernando) (objeto Macarena))
   (Relacion (tipo ESPOSO) (sujeto Eduardo) (objeto Ana)))


;;;;;;; Cada relacion tiene una relacion dual que se produce al cambiar entre si objeto y sujeto. Por ejejmplo, Si x es HIJO de y, y es PADRE de x". Para poder deducirlo con una sola regla metemos esa informacion como hechos con la etiqueta dual, "Dual de HIJO PADRE", y asi con todas las relaciones consideradas
 
(deffacts duales
(dual HIJO PADRE) (dual ABUELO NIETO) (dual HERMANO HERMANO) (dual ESPOSO ESPOSO) (dual PRIMO PRIMO) (dual TIO SOBRINO) (dual CUNIADO CUNIADO) (dual YERNO SUEGRO))

;;;;;; Para deducir las reglas que se aplican son de composicion, del tipo "el HERMANO del PADRE es un TIO". Por comodidad, en lugar de crear una regla por cada posible composici�n, metemos como hechos la relacion que se obtiene por composicion. Solo metemos unas cuantas composiciones que sean suficientes para deducir cualquier cosa

(deffacts compuestos
(comp HIJO HIJO NIETO) (comp PADRE PADRE ABUELO) (comp ESPOSO PADRE PADRE)(comp HERMANO PADRE TIO) (comp HERMANO ESPOSO CUNIADO) (comp ESPOSO HIJO YERNO) (comp ESPOSO HERMANO CUNIADO) (comp HIJO PADRE HERMANO) (comp ESPOSO CUNIADO CUNIADO) (comp ESPOSO TIO TIO)  (comp HIJO TIO PRIMO)  ) 


;;;;;; Para que cuando digamos por pantalla el parentesco lo espresemos correctamente, y puesto que el nombre que hemos puesto a cada relacion es el caso masculino, vamos a meter como hechos como se diaria esa relacion en femenino mediante la etiqueta femenino

(deffacts femenino
(femenino HIJO HIJA) (femenino PADRE MADRE) (femenino ABUELO ABUELA) (femenino NIETO NIETA) (femenino HERMANO HERMANA) (femenino ESPOSO ESPOSA) (femenino PRIMO PRIMA) (femenino TIO TIA) (femenino SOBRINO SOBRINA) (femenino CUNIADO CUNIADA) (femenino YERNO NUERA) (femenino SUEGRO SUEGRA)) 


;;;;; REGLAS DEL SISTEMA ;;;;;

;;;; La dualidad es simetrica: si r es dual de t, t es dual de r. Por eso solo metimos como hecho la dualidad en un sentidos, pues en el otro lo podiamos deducir con esta regla

(defrule autodualidad
      (razonar)
      (dual ?r ?t)
=> 
   (assert (dual ?t ?r)))

(defrule autonumero
   (razonar)
   (dual ?r ?t)
=>
   (assert (singular ?t =r)))
;;;; Si  x es R de y, entonces y es dualdeR de x

(defrule dualidad
   (razonar)
   (Relacion (tipo ?r) (sujeto ?x) (objeto ?y))
   (dual ?r ?t)
=> 
   (assert (Relacion (tipo ?t) (sujeto ?y) (objeto ?x))))


;;;; Si  y es R de x, y x es T de z entonces y es RoT de z
;;;; a�adimos que z e y sean distintos para evitar que uno resulte hermano de si mismo y cosas asi.

(defrule composicion
   (razonar)
   (Relacion (tipo ?r) (sujeto ?y) (objeto ?x))
   (Relacion (tipo ?t) (sujeto ?x) (objeto ?z))
   (comp ?r ?t ?u)
   (test (neq ?y ?z))
=> 
   (assert (Relacion (tipo ?u) (sujeto ?y) (objeto ?z))))

;;;;; Como puede deducir que tu hermano es tu cu�ado al ser el esposo de tu cu�ada, eliminamos los cu�ados que sean hermanos

(defrule limpiacuniados
    (Relacion (tipo HERMANO) (sujeto ?x) (objeto ?y))
    ?f <- (Relacion (tipo CUNIADO) (sujeto ?x) (objeto ?y))
=>
	(retract ?f) )

;;;;; Solicitamos el nombre de la primera persona sobre el que se desea informacion y guardamos y a�adimos ese hecho 
 
(defrule pregunta1
(declare (salience 9999))
(informacion)
(not (primerapersona ?x))
=>
   (printout t "Escriba la persona sobre la que quiera obtener informacion: " crlf)
   (assert (primerapersona (read))))
   
   ;;;;; Solicitamos el nombre de la segunda persona 
 
(defrule pregunta2
(declare (salience 9999))
(informacion)
(primerapersona ?primero)
(not (relacion ?r))
=>
   (printout t "Dime la relacion que quiera obtener de  " ?primero crlf)
   (assert (relacion (read)))
   (assert (razonar))  )

;;;;; Hacemos que nos diga por pantalla la relacion entre las persona introducidas. Como la forma de expresarlo dependera del sexo, usamos dos reglas, una para cada sexo

(defrule relacionmasculino
   ?f <- (razonar)
   (informacion)
   (primerapersona ?x)
   (relacion ?r)
   (relacionmasculino ?r)
   (Relacion (tipo ?r) (sujeto ?x) (objeto ?y))
   (hombre ?y)
 =>
   (printout t ?y " es " ?r " de " ?x crlf)
   (assert (encontrado)) 
   )

(defrule relacionfemenino
   ?f <- (razonar)
   (informacion)
   (primerapersona ?x)	
   (relacion ?r)	
   (relacionfemenino ?r)
   (femenino ?t ?r)
   (Relacion (tipo ?t) (sujeto ?x) (objeto ?y))
   (mujer ?y)
 =>
   (printout t ?y " es " ?r " de " ?x crlf)
   (assert(encontrado))
)

;; Esta regla comprueba que el nombre existe en la familia y si la relación existe
;; si no existe alguno de los dos te avisa y quitamos los hechos de la base para que te los vuelva a pedir
(defrule controlErroresRelacion
   (informacion)
   (relacion ?r)
   (primerapersona ?x)
   ?val1 <- (relacion ?r)
   ?val2 <- (primerapersona ?x)
   (not (listarelacion ?r))
   (not (nombre ?x))
=>
   (printout t crlf " Por favor revise que haya introducido la relacion y el nombre de manera correcta" crlf)
   (printout t "Los nombres validos (debe escribirlos de la misma manera que a continuacion) son: " crlf)
   (printout t " Francisco, Luis, Jaime, Eduardo, JoseIgnacio, Fernando, Victor, Pablo, Julio, JuanFrancisco, Paula, Marina, Helena, Macarena, Teresa, Carmen" crlf)
   (printout t "Las relaciones validas (debe escribirlas tal y como vienen a continuacion) son: " crlf)
   (printout t "HIJO, PADRE, ABUELO, NIETO, HERMANO, ESPOSO, PRIMO, TIO, SOBRINO, CUNIADO, YERNO, SUEGRO" crlf)
   (printout t "!!ES MUY IMPORTANTE RESPETAR LAS MAYUSCULAS!!" crlf)
   (retract ?val1)
   (retract ?val2)
)


;; Esta será la primera regla que se lance y solo se lanzará una vez
;; Informa de los nombres de la familia
(defrule informacion
=>
   (printout t crlf " Esta familia esta compuesta por: " crlf)
   (printout t " Francisco, Luis, Jaime, Eduardo, JoseIgnacio, Fernando, Victor, Pablo, Julio, JuanFrancisco, Paula, Marina, Helena, Macarena, Teresa, Carmen" crlf)
   (assert (informacion))
)

;; Esta regla mira si no hay ninguna relación existente y avisa de que no hay parentesco
(defrule noParentesco
   (not (encontrado))
   (relacion ?r)
   (primerapersona ?x)
=>
   (printout t crlf ?x " no tiene ningun " ?r crlf)
)