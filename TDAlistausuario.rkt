#lang racket
(require "TDAusuario.rkt")


; TDA lista de usuarios

; Otras funciones
; Comprueba si el usuario ya existe
(define (cmpcons c1 c2)
  (if (and (eqv? (car c1) (car c2)) (eqv? (cdr c1) (cdr c2)))
      #t
      #f
  )
)

(define (existeUser? listaUsuarios user)
    (if (null? listaUsuarios)
        #f
        (if (cmpcons (car listaUsuarios) user)  ; Aqui está el error, no compara los cons
            #t
            (existeUser? (cdr listaUsuarios) user)
        )
    )
)

; Representación
; (list usuario1 usuario2 ... usuarioN)

; Constructor
(define (crearListaUsuario usuario)
    (if (user? usuario)
        (list usuario)
        null
    )
)

; Pertenencia
; Hacer una función que verifique que todos son usuarios
(define (listaUsuarios? listaUsers)
    (if (null? listaUsers)
        #t
        (if (user? (car listaUsers))
            (listaUsuarios? (cdr listaUsers))
            #f
        )
    )
)


; Selectores
(define (obtenerUser listaUsuarios indice)
    (list-ref listaUsuarios indice)
)

; Modificadores
(define (agregarUser listaUsuarios usuario)
    (if (and (user? usuario) (listaUsuarios? listaUsuarios))
        (append listaUsuarios (list usuario)) ; Caso verdadero, el usuario es agregado a la lista
        listaUsuarios ; Caso falso -> el parametro usuario no corresponde al tda usuario
    )
)

(define usuario (user "daniel" "rucio025"))
(define lista (crearListaUsuario usuario))
(define lista2 (agregarUser lista (user "paula" "2501")))
(define lista3 (agregarUser lista2 (user "monse" "123")))



(provide obtenerUser agregarUser crearListaUsuario existeUser?)

