#lang racket
(require "TDAacceso.rkt")
;TDA lista de accesess

; Constructor
(define (accesess acceso)
  (if (access? acceso)
      (list acceso)
      null
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
; Obtener que tipo de acceso tiene tal usuario buscando por nombre

(define (obtenerTipoAcceso lista_accesos nombre)
  (obtenerPermisoAc ; Saco el permiso
   (obtenerAcceso lista_accesos nombre) ; Obtengo el acceso
   )
  )

; Modificadores
; Agregar acceso a la lista
(define (agregarAcceso lista_accesos acceso)
  (if (accesess? lista_accesos)
      (append lista_accesos acceso)
      null
      )
  )


(define permiso1 (access "daniel" #\w))
(define accesoPrim (accesess permiso1))

(provide accesess accesess? obtenerAcceso obtenerTipoAcceso agregarAcceso)

