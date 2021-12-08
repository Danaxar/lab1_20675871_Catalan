#lang racket
(require "TDAusuario_20675871_CatalanValenzuela.rkt")

; TDA ACCESO

; ----------------- REPRESENTACIÓN ------------------------
; string X char



; ------------------ CONSTRUCTOR ----------------------------
#|
Función que construye el tda acceso
Dominio = string X char
Recorrido = acceso
|#
(define (access nombreUsuario tipoPermiso)
    (if (and (string? nombreUsuario) 
        (or (eqv? tipoPermiso #\r)  ; Lectura
            (eqv? tipoPermiso #\w)  ; Escritura
            (eqv? tipoPermiso #\c)) ; Comentarios
        )
        ; Caso verdadero
        (cons nombreUsuario tipoPermiso)
        null
    )
)


; ------------------ PERTENENCIA ------------------
#|
Función que verifica si el argumento dado pertenece al tda acceso o no
Dominio = access
Recorrido = boolean
|#

(define (access? acceso)
  (if (cons? acceso)
      ; Caso verdadero 
      (if (and (string? (car acceso))
          (or (eqv? (cdr acceso) #\r) 
              (eqv? (cdr acceso) #\w)  
              (eqv? (cdr acceso) #\c)) 
          )
          ; V
          #t
          ; F
          #f
      
      )
      ; Caso falso
      #f
  )
  )


; ------------------ SELECTORES ------------------

#|
Función que obtiene el nombre de un acceso
Dominio = acceso 
Recorrido = string o null
|#
(define (obtenerNombreAc ac)
  (if (access? ac)
      (car ac)
      null
      )
  )

#|
Función que obtiene el tipo de permiso de un accceso
Dominio = acceso
Recorrido = char o null
|#
(define (obtenerPermisoAc ac)
  (if (access? ac)
      (cdr ac)
      null
      )
  )


; ------------------ OTROS ------------------------------
#|
Función que exporta un tda acceso a información en string
Dominio = access
Recorrido = string
|#
(define (acceso->string acceso)
  (string-append (obtenerNombreAc acceso) " " (string (obtenerPermisoAc acceso)) " ")
  )
(provide acceso->string)
(provide access access? obtenerNombreAc obtenerPermisoAc)