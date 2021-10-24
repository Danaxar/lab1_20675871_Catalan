#lang racket
; TDA documento
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")



; Implementación:
; Representación

; Constructores
; Función que construye un tda documento
; Dominio: string x fecha x entero x (list integer) x (list tda user)

#|En versionesAnteriores se puede pensar como que en ese parametro ya existen los mismos
parametros escritos en la función, por lo que debe ser una lista, entonces esta lista hay
que juntarla como elemento único con los mismos parametros que se están ingresando acá|#

#|Otra forma de verlo es que este constructor solo se encarga de consolidar o agrupar los datos
la forma en la que se van a obtener las versiones anteriores va a ser otro tema|#


; DEJAR PARA DESPUÉS

(define (document fecha nombreDoc contenido)
  (if (and 
  			(string? nombreDoc) 
			(fecha? fecha) 
			(string? contenido)
  		)
        (list fecha nombreDoc contenido)
        null
  )
)

; Pertenencia
; Función que verifica si el parametro de entrada es un documento
(define (document? documento)
	(if (= (length documento) 3)
		(if (and (fecha? (car documento))
				( string?(cadr documento))
				(string? (caddr documento))
			)

			#t
			#f
		)
		#f
	)
)


; Selectores

; Modificadores
; Otras operaciones
(provide document document?)
