#lang racket
(require "TDAacceso.rkt")
;TDA lista de accesess

; Constructor
(define (accesess acceso . demas)
  (if (null? demas)
      ; Si solo hay un acceso
      (if (access? acceso)
          (list acceso)
          null
          )
      ; Si hay más
      (list acceso demas)
      )
  
  )

; Pertenencia
; Verifica que todos los accesos corresponden al tda
(define (accesess? lista_accesos)
  (if (null? lista_accesos)
      #t
      (if (access? (car lista_accesos))
          (accesess? (cdr lista_accesos))
          #f)
      )
  )




; Selectores
; Obtener acceso dado el nombre del usuario
(define (obtenerAcceso lista_accesos nombre)
  (if (null? lista_accesos)
      null
      (if (eqv? nombre (obtenerNombreAc (car lista_accesos))) ; Si el nombre buscado coincide con el acceso n
          (car lista_accesos)   
          (obtenerAcceso (cdr lista_accesos) nombre)
          )
      )
  )

; Obtener primer acceso
(define (obtenerPrimerAcceso lista_acceso)
  (list-ref lista_acceso 0)
  )

; Obtener que tipo de acceso tiene tal usuario buscando por nombre
(define (obtenerTipoAcceso lista_accesos nombre)
  (obtenerPermisoAc ; Saco el permiso
   (obtenerAcceso lista_accesos nombre) ; Obtengo el acceso
   )
  )

; Modificadores
; Agregar acceso a la lista
#| versión anterior
(define (agregarAcceso lista_accesos acceso)
  (if (accesess? lista_accesos)
      (append lista_accesos acceso)
      null
      )
  )
|#
(define (eliminarPrimerAcceso lista_acceso)
  (cdr lista_acceso)
  )

(define (agregarAcceso lista_accesos acceso)
  (if (null? lista_accesos)
      (cons acceso null)
      (cons (car lista_accesos) (agregarAcceso (cdr lista_accesos) acceso))
      )
  )

(define (agregarAcceso2 lista_accesos acceso demas)
  (if (null? demas)
      ; V -> Solo hay un acceso
      (append lista_accesos (cons acceso null))
      ; F -> Hay más de un acceso
      (append lista_accesos (cons acceso null) demas)
      )
  )


; Verifica si el usuario tiene un acceso dentro de la lista
(define (tienePermiso lista_accesos nombreUsuario tipoPermiso)
  (if (null? lista_accesos)
      #f
      (if (and
           (eqv? (obtenerNombreAc(obtenerPrimerAcceso lista_accesos)) nombreUsuario)
           (eqv? (obtenerNombreAc(obtenerPrimerAcceso lista_accesos)) tipoPermiso))
          #t
          (tienePermiso (cdr lista_accesos) nombreUsuario tipoPermiso)
          )
      )
  )

(define permiso1 (access "daniel" #\w))
(define permiso2 (access "fran" #\r))
(define permiso3 (access "paula" #\c))
(define accesoPrim (accesess permiso1))

(provide accesess accesess? obtenerAcceso obtenerTipoAcceso agregarAcceso tienePermiso)
(provide agregarAcceso2)

