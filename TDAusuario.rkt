; TDA USUARIO

; Representacion
; (string X entero)
; (cons usuario contraseña) 


; Constructores
; Función que construye el TDA usuario
; Dominio: string x entero
; Recorrido: par: user
(define (user name pass)
    (if (and (string? name) (integer? pass))
        (cons name pass)
        null
    )
)
; FUNCIONA

; Pertenencia
; Función que evalua si el parametro entrado es un TDA usuario
; Dominio: user: string X entero
; Recorrido: boolean
(define (user? par)
    (if (and (cons? par) (string? (car par)) (integer? (cdr par)))
        #t
        #f
    )
)

; Selectores
; Función que obtiene el nombre de un usuario
; Dominio: user: string x entero
; Recorrido: string X boolean
(define (obtenerNombre usuario)
    (if (user? usuario)
        (car usuario)
        #f
    )
)
; Funciona

; Función que obtiene la contraseña de un usuario
; Dominio: user: string x entero
; Recorrido: entero X boolean
(define (obtenerPass usuario)
    (if (user? usuario)
        (cdr usuario)
        #f
    )
)

; Modificadores
; Función que modifica el nombre de un usuario
; Dominio: user: string X entero
; Recorrido: user X boolean
(define (modificarNombre usuario nuevoNombre)
    (if (user? usuario)
        (user nuevoNombre (obtenerPass usuario))
        #f
    )
)
; Funciona

; Función que modifica la contraseña de un usuario
; Dominio: user: string X entero
; Recorrido: user X boolean
(define (modificarPass usuario nuevaPass)
    (if (user? usuario)
        (user (obtenerNombre usuario) nuevaPass)
        #f
    )
)
; Funciona
; Otras operaciones