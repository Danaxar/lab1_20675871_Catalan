#lang racket
; TDA documento
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAacceso.rkt")
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

(define (document fecha nombreDoc contenido creador accesos id idAnt)
  (if (and
       (fecha? fecha) 
       (string? nombreDoc)
       (string? contenido)
       (user? creador) ; -> sesión activa
       (accesess? accesos)
       (integer? id)
       (integer? idAnt)
       )
      (list fecha nombreDoc contenido creador accesos id idAnt)
      null
      )
  )

; Pertenencia
; Función que verifica si el parametro de entrada es un documento
(define (document? documento)
  (if (= (length documento) 7)
      ; Verdadero
      (if
       ; Condiciones
       (and (fecha? (list-ref documento 0))
            ( string?(list-ref documento 1))
            (string? (list-ref documento 2))
            (user? (list-ref documento 3))
            (accesess? (list-ref documento 4))
            (integer? (list-ref documento 5))
            (integer? (list-ref documento 6))
            )

       #t ; Verdadero
       #f ; Falso
       )
      
      #f ; Falso
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
(define (obtenerIdDocumento documento)
  (list-ref documento 5)
  )

; Obtener id anterior del documento
(define (obtenerIdAntDocumento documento)
  (list-ref documento 6)
  )

; Modificadores
; Agregar texto al final del documento
; Dominio = document X string
(define (agregarTexto documento texto)
  (document
   (obtenerFechaDocumento documento)
   (obtenerNombreDocumento documento)
   (string-append (obtenerContenidoDocumento documento) texto) ; Aqui hacer el cambio
   (obtenerCreadorDocumento documento)
   (obtenerListaAccesos documento)
   (+ (obtenerIdDocumento documento) 1) ; Crear un nuevo id
   (obtenerIdDocumento documento)
   )
  )


#|Quitar todos los permisos externos de un documento (dejar solo los del propietario|#
(define (quitarTodosLosPermisosDocumento documento)
  (document
   (obtenerFechaDocumento documento)
   (obtenerNombreDocumento documento)
   (obtenerContenidoDocumento documento)
   (obtenerCreadorDocumento documento)
   (accesess ; Hago una lista de accesos
    (access
     (obtenerNombre (obtenerCreadorDocumento documento)) ; Nombre de usuario
     #\w ; Tipo de permiso -> escritura
     ))
   (obtenerIdDocumento documento)
   (obtenerIdDocumento documento)
   )
  )
(provide quitarTodosLosPermisosDocumento)



#|
; Agregar un permiso
(define (agregarPermiso documento acceso)
  (document
   (obtenerFechaDocumento documento)
   (obtenerNombreDocumento documento)
   (obtenerContenidoDocumento documento)
   (obtenerCreadorDocumento documento)
   (agregarAcceso (obtenerListaAccesos documento) acceso)
   (obtenerIdDocumento documento))
  )
|#
; Quitar un permiso
; Otras operaciones

; Confirmar si un usuario es propietario del documento
(define (propietario? documento usuario)
  (if (eqv? (obtenerNombre (obtenerCreadorDocumento documento)) (obtenerNombre usuario))
      #t
      #f
      )
  )
(provide propietario?)



; Exportar
(provide document)
(provide document?)
(provide obtenerFechaDocumento obtenerNombreDocumento obtenerContenidoDocumento obtenerCreadorDocumento obtenerListaAccesos obtenerListaAccesos)
(provide obtenerIdDocumento agregarTexto obtenerIdAntDocumento)