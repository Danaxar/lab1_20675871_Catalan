; TDA acceso

; representación
; string X char

; Constructor
(define (access nombreUsuario tipoPermiso)
    (if (and (string? nombreUsuario) 
        (or (eqv? tipoPermiso #\r) ; lectura
            (eqv? tipoPermiso #\w)    ; Escritura
            (eqv? tipoPermiso #\c)) ; Comentarios
        )
        ; Caso verdadero
        (cons nombreUsuario tipoPermiso)
        null
    )
)

)