#lang racket
(require "TDAusuario.rkt")

; TDA acceso

; representación
; string X char

; Constructor
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

; Pertenencia
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


; Selectores
; Obtener el nombre de un acceso
(define (obtenerNombreAc ac)
  (if (access? ac)
      (car ac)
      null
      )
  )

; Obtener el tipo de permiso de un acceso
(define (obtenerPermisoAc ac)
  (if (access? ac)
      (cdr ac)
      null
      )
  )

(define pruebaAcceso (access "daniel" #\r))
(provide access access? obtenerNombreAc obtenerPermisoAc)