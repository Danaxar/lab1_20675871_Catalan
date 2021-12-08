#lang racket
; TDA USUARIO

; ----------------- REPRESENTACIÓN ------------------------
; (string X entero)
; (cons usuario contraseña) 


; ------------------ CONSTRUCTOR ----------------------------
; Función que construye el TDA usuario
; Dominio: string x entero
; Recorrido: par: user
(define (user name pass)
    (if (and (string? name) (string? pass))
        (cons name pass)
        null
    )
)


; ------------------ PERTENENCIA ------------------
; Función que evalua si el parametro entrado es un TDA usuario
; Dominio: user: string X entero
; Recorrido: boolean
(define (user? par)
    (if (cons? par)  ; Verificar primero que sea cons
        ; Caso verdadero
        (if (and (string? (car par)) (string? (cdr par)))
            #t
            #f
        )
        ; Caso falso
        #f
    )
)

; ------------------ SELECTORES ------------------
; Función que obtiene el nombre de un usuario
; Dominio: user: string x entero
; Recorrido: string X boolean
(define (obtenerNombre usuario)
    (if (user? usuario)
        (car usuario)
        #f
    )
)


; Función que obtiene la contraseña de un usuario
; Dominio: user: string x entero
; Recorrido: entero X boolean
(define (obtenerPass usuario)
    (if (user? usuario)
        (cdr usuario)
        #f
    )
)

; ------------------ MODIFICADORES ------------------
; Función que modifica el nombre de un usuario
; Dominio: user: string X entero
; Recorrido: user X boolean
(define (modificarNombre usuario nuevoNombre)
    (if (user? usuario)
        (user nuevoNombre (obtenerPass usuario))
        #f
    )
)

; Función que modifica la contraseña de un usuario
; Dominio: user: string X entero
; Recorrido: user X boolean
(define (modificarPass usuario nuevaPass)
    (if (user? usuario)
        (user (obtenerNombre usuario) nuevaPass)
        #f
    )
)

; ------------------ OTROS -------------------------
; Exportar usuario como string
; dominio-> user
; recorrido-> string
(define (user->string usuario)
  (string-append
   (obtenerNombre usuario) " "
   (obtenerPass usuario) " "
   )
  )
(provide user->string)

; Exportar
(provide user) ; Constructor
(provide user?) ; Pertenencia
(provide obtenerNombre obtenerPass) ; Selectores
(provide modificarNombre modificarPass) ; Modificadores
