#lang racket
(require "TDAusuario.rkt")

; TDA lista de usuarios

; ----------------- REPRESENTACIÓN ------------------------
; (list usuario1 usuario2 ... usuarioN)

; ------------------ OTROS ------------------------
#| Compara dos usuarios
dominio = user X user
recorrido = boolean|#
(define (cmpcons c1 c2)
  (if (and (eqv? (car c1) (car c2)) (eqv? (cdr c1) (cdr c2)))
      #t
      #f
      )
  )

; Comprueba si el usuario ya existe
#|dominio = listaUsuarios X user
recorrido = boolean|#
(define (existeUser? listaUsuarios user)
  (if (null? listaUsuarios)
      #f
      (if (cmpcons (car listaUsuarios) user)  ; Aqui está el error, no compara los cons
          #t
          (existeUser? (cdr listaUsuarios) user)
          )
      )
  )




; ------------------ CONSTRUCTOR ----------------------------
#|Función que crea una lista de usuarios a partir de un usuario existente
dominio: user
recorrido: listaUsuario|#
(define (crearListaUsuario usuario)
  (if (user? usuario)
      (list usuario)
      null
      )
  )

; ------------------ PERTENENCIA ------------------
; Hacer una función que verifique que todos son usuarios
#|dominio = listaUsuarios
recorrido = boolean|#
(define (listaUsuarios? listaUsers)
  (if (null? listaUsers)
      #t
      (if (user? (car listaUsers))
          (listaUsuarios? (cdr listaUsers))
          #f
          )
      )
  )


; ------------------ SELECTORES ------------------
#|Función que obtiene un usuario según una posición en la lista
domino: listaUsuarios X int
recorrido: user|#
(define (obtenerUser listaUsuarios indice)
  (list-ref listaUsuarios indice)
  )

#|Función que obtiene el primer usuario de la lista
Dominio: listaUsuario
recorrido: user|#
(define (obtenerPrimerUser lista_usuarios)
  (car lista_usuarios)
  )

#|Función que quita el primer usuario de la lista
dominio: listaUsuarios
recorrido: listaUsuarios|#
(define (quitarPrimerUser lista_usuarios)
  (cdr lista_usuarios)
  )
(provide obtenerPrimerUser)


; ------------------ MODIFICADORES ------------------
#|Función que agrega un usuario a la lista
dominio: listaUsuarios X user
recorrido: listUsuarios|#
(define (agregarUser listaUsuarios usuario)
  (if (and (user? usuario) (listaUsuarios? listaUsuarios))
      (append listaUsuarios (list usuario)) ; Caso verdadero, el usuario es agregado a la lista
      listaUsuarios ; Caso falso -> el parametro usuario no corresponde al tda usuario
      )
  )



; Exportar la lista de usuarios como string
#|Función que exporta la información de la lista como string
dominio: listaUsuarios
recorrido: string|#
(define (listaUsuarios->string lista_usuarios)
  
  (define (listaUsuarios->stringEncapsulado lista_usuarios resultado)
    (if (= (length lista_usuarios) 0)
        ; V
        resultado
        
        ; F
        (listaUsuarios->stringEncapsulado
         ; Argumento1 -> Avanzar en la lista
         (quitarPrimerUser lista_usuarios) 
         ; Argumento2 -> Agregar el string y guardar el resultado
         (string-append
          resultado
          "\t"
          (user->string (obtenerPrimerUser lista_usuarios))
          "\n"
          )
         
         )
        )
    )
    

  ; Llamar a la función
  (listaUsuarios->stringEncapsulado lista_usuarios "")
   
  )
(provide listaUsuarios->string)
(provide obtenerUser agregarUser crearListaUsuario existeUser?)

