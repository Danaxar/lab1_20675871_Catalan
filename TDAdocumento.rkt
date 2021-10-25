#lang racket
; TDA documento
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAparadigmadocs1.rkt")
(require "TDAlistaaccesos.rkt")



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

(define (document fecha nombreDoc contenido creador accesos id)
  (if (and 
       (string? nombreDoc) 
       (fecha? fecha) 
       (string? contenido)
       (user? creador) ; -> sesión activa
       (accesess? accesos)
       (integer? id)
       )
      (list fecha nombreDoc contenido creador accesos id)
      null
      )
  )

; Pertenencia
; Función que verifica si el parametro de entrada es un documento
(define (document? documento)
  (if (= (length documento) 6)
      (if
       ; Condiciones
       (and (fecha? (car documento))
            ( string?(cadr documento))
            (string? (caddr documento))
            (user? (cadddr documento))
            (accesess? (caddddr documento))
            (integer? id)
            )

       #t
       #f
       )
      
      #f
      )
  )


; Selectores
; Obtener la fecha del documento
(define (obtenerFechaDocumento documento)
  (list-ref documento 0)
  )
; Obtener el nombre del documento
(define (obtenerNombreDocumento documento)
  (list-ref documento 1)
  )
; Obtener el contenido del documento
(define (obtenerContenidoDocumento documento)
  (list-ref documento 2)
  )
; Obtener el creador del documento
(define (obtenerCreadorDocumento documento)
  (list-ref documento 3)
  )
; Obtener la lista de accesos del documento (lista de usuarios)
(define (obtenerListaAccesos documento)
  (list-ref documento 4)
  )
; Obtener id del documento
(define (obtenerListaAccesos documento)
  (list-ref documento 5)
  )

; Modificadores
;(define (agregarTexto))
; Otras operaciones

; Exportar
(provide document)
(provide document?)
(provide obtenerFechaDocumento obtenerNombreDocumento obtenerContenidoDocumento obtenerCreadorDocumento obtenerListaAccesos obtenerListaAccesos)
