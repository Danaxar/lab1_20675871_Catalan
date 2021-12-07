#lang racket

; TDA documento

(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAacceso.rkt")
(require "TDAlistaaccesos.rkt")

; ----------------- REPRESENTACIÓN ------------------------
; date X string X string X user X listaAccesos X int X int


; ------------------ CONSTRUCTOR ----------------------------
; Función que construye un tda documento
; Dominio: string x fecha x entero x (list integer) x (list tda user)
; Recorrido: document
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

; ------------------ PERTENENCIA ------------------
; Función que verifica si el parametro de entrada es un documento
; Dominio: document
; Recorrido: boolean
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




; ------------------ SELECTORES ------------------
; Obtener la fecha del documento
; Dominio: document
; Recorrido: date
(define (obtenerFechaDocumento documento)
  (list-ref documento 0)
  )

; Obtener el nombre del documento
; Dominio: document
; Recorrido: string
(define (obtenerNombreDocumento documento)
  (list-ref documento 1)
  )


; Obtener el contenido del documento
; Dominio: document
; Recorrido: string
(define (obtenerContenidoDocumento documento)
  (list-ref documento 2)
  )

; Obtener el creador del documento
; Dominio: document
; Recorrido: user
(define (obtenerCreadorDocumento documento)
  (list-ref documento 3)
  )

; Obtener la lista de accesos del documento
; Dominio: document
; Recorrido: listaAccesos
(define (obtenerListaAccesos documento)
  (list-ref documento 4)
  )

; Obtener el id del documento
; Dominio: document
; Recorrido: int
(define (obtenerIdDocumento documento)
  (list-ref documento 5)
  )

; Obtener el id de la versión anterior del documento
; Dominio: document
; Recorrido: int
(define (obtenerIdAntDocumento documento)
  (list-ref documento 6)
  )

; ------------------ MODIFICADORES ------------------
; Agregar texto al final del documento
; Dominio = document X string
; Recorrido = document
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


#|Quitar todos los permisos externos de un documento (dejar solo los del propietario)
Dominio = documento
Recorrido = documento
|#
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


; ------------------ OTROS ------------------------------
#|Confirmar si un usuario es propietario del documento
Dominio = document X user
Recorrido = boolean|#
(define (propietario? documento usuario)
  (if (eqv? (obtenerNombre (obtenerCreadorDocumento documento)) (obtenerNombre usuario))
      #t
      #f
      )
  )
(provide propietario?)

#|Exportar información de un documento a string
Dominio = document X Function
Recorrido = string|#
(define (document->string documento desencriptador)
  (string-append
   "Documento:\n"
   ; Nombre
   "\tNombre del documento: "
   (obtenerNombreDocumento documento) "\n"
   ; Fecha
   "\tFecha de creación: "
   (fecha->string (obtenerFechaDocumento documento)) "\n"
   ; Creador
   "\tCreado por: "
   (user->string (obtenerCreadorDocumento documento)) "\n"
   ; Lista de accesos
   "\tPersonas con acceso:\n"
   (listaAcceso->string (obtenerListaAccesos documento)) "\n"  
   ; Id del documento
   "\tId: "
   (number->string (obtenerIdDocumento documento)) "\n"
   "\tId ver. anterior: "
   (number->string (obtenerIdAntDocumento documento)) "\n"
   "contenido:\n"
   (desencriptador (obtenerContenidoDocumento documento)) "\n"
   )
  )
(provide document->string)




; Exportar
(provide document)
(provide document?)
(provide obtenerFechaDocumento obtenerNombreDocumento obtenerContenidoDocumento obtenerCreadorDocumento obtenerListaAccesos obtenerListaAccesos)
(provide obtenerIdDocumento agregarTexto obtenerIdAntDocumento)