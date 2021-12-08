#lang racket
(require "TDAacceso_20675871_CatalanValenzuela.rkt")

;TDA lista de accesess

; ----------------- REPRESENTACIÓN ------------------------
; (list accesos...)

; ------------------ CONSTRUCTOR ----------------------------
#|Función que crea un tda listaAcceso
Dominio = acceso X accesos...
Recorrido = listaAcceso|#
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

; ------------------ PERTENENCIA ------------------
#|Verifica que todos los accesos corresponden al tda
Dominio = listaAcceso
Recorrido = boolean|#
(define (accesess? lista_accesos)
  (if (null? lista_accesos)
      #t
      (if (access? (car lista_accesos))
          (accesess? (cdr lista_accesos))
          #f)
      )
  )




; ------------------ SELECTORES ------------------
; Obtener acceso dado el nombre del usuario
#|Dominio = listaAcceso X string
Recorrido = acceso|#
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
#|dominio = listaAcceso
recorrid = acceso|#
(define (obtenerPrimerAcceso lista_acceso)
  (list-ref lista_acceso 0)
  )

; Obtener que tipo de acceso tiene tal usuario buscando por nombre
#|dominio = listaAcceso X string
recorrido = char|#
(define (obtenerTipoAcceso lista_accesos nombre)
  (obtenerPermisoAc ; Saco el permiso
   (obtenerAcceso lista_accesos nombre) ; Obtengo el acceso
   )
  )

; ------------------ MODIFICADORES ------------------
; Agregar acceso a la lista
#|dominio = listaAcceso
recorrido = listaAcceso|#
(define (eliminarPrimerAcceso lista_acceso)
  (cdr lista_acceso)
  )

#|Agrega un acceso a la lista de accesos
Dominio = listaAcceso X access
Recorrido = listaAcceso|#
(define (agregarAcceso lista_accesos acceso)
  (if (null? lista_accesos)
      (cons acceso null)
      (cons (car lista_accesos) (agregarAcceso (cdr lista_accesos) acceso))
      )
  )

#|Función que agrega un acceso y además una lista de ellos
Dominio = listaAcceso acceso X accesos...
Recorrido = listaAcceso|#
(define (agregarAcceso2 lista_accesos acceso demas)
  (if (null? demas)
      ; V -> Solo hay un acceso
      (append lista_accesos (cons acceso null))
      ; F -> Hay más de un acceso
      (append lista_accesos (cons acceso null) demas)
      )
  )



; ------------------ OTROS ----------------------------------
; Verifica si el usuario tiene un acceso dentro de la lista
#|Dominio = listaAcceso X string X char
Recorrido = boolean|#
(define (tienePermiso lista_accesos nombreUsuario tipoPermiso)
  (if (null? lista_accesos)
      #f
      (if (and 
           (eqv? ; Si los nombres son iguales
            (obtenerNombreAc(obtenerPrimerAcceso lista_accesos))
            nombreUsuario
            )
           (eqv? ; Y si 
            (obtenerPermisoAc(obtenerPrimerAcceso lista_accesos))
            tipoPermiso)
           )
          ; T
          #t
          ; F -> Seguir recorriendo
          (tienePermiso (cdr lista_accesos) nombreUsuario tipoPermiso)
          )
      )
  )


; Exportar la lista de accesos como string
#|Dominio = listaAccecso
Recorrido = string|#
(define (listaAcceso->string lista_accesos)
  
  (define (listaAcceso->stringEncapsulado lista_accesos resultado)
    (if (= (length lista_accesos) 0)
        ; V
        resultado
        ; F
        (listaAcceso->stringEncapsulado
         (eliminarPrimerAcceso lista_accesos) ; Seguir recorriendo

         ; Guardar resultado
         (string-append
          resultado
          "\t\t"
          (acceso->string (obtenerPrimerAcceso lista_accesos))
          "\n")
         )
        )
    )
  ; Llamar a la función
  (listaAcceso->stringEncapsulado lista_accesos "")
  )
(provide listaAcceso->string)
(provide accesess accesess? obtenerAcceso obtenerTipoAcceso agregarAcceso tienePermiso)
(provide agregarAcceso2)

