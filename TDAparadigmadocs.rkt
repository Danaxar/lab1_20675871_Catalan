#lang racket
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAlistausuario.rkt")

; Tda programa
; Contiene:
    ; Usuarios -> tda lista de usuario
    ; Sesion activa -> tda usuario
    ; Documentos   -> tda lista de documento
    ; Versiones de los documentos  -> lista de documentos


; Encriptador
(define (encriptador texto)
    (list->string (reverse (string->list texto)))
)

; Desencriptador
(define (desencriptador texto)
    (encriptador texto)
)


; Implementación:
; Representación
; "nombreDelDocs" X fecha X encriptador X desencriptador

; Constructor paradigmadocs
; Se crea una versión inicial de paradigmadocs


(define
  ; Nombre de la función
  (paradigmadocsEncapsulado
   ; Entradas
   nombreDocs fecha encriptador
   desencriptador listaUsuarios)

  ; Verificadores
  (if (and (string? nombreDocs) (fecha? fecha))
      ; Caso verdadero
        (list nombreDocs fecha encriptador desencriptador listaUsuarios)
      ; Caso falso
        null
  )
)

; Constructor paradigmadocs
; Inicial -> no hay usuarios registrados
(define (paradigmadocsIncial nombreDocs fecha encriptador desencriptador)
    (paradigmadocsEncapsulado
     nombreDocs fecha encriptador desencriptador
     (crearListaUsuario (user "administrador" "usach")))
)

; Principal -> resto de ejecuciones
(define (paradigmadocs nombreDocs fecha encriptador desencriptador listaUsuarios)
  (if (and (string? nombreDocs) (fecha? fecha))
      ; Caso verdadero
        (list nombreDocs fecha encriptador desencriptador listaUsuarios)
      ; Caso falso
        null
  )
)

; Pertenencia
(define (paradigmadocs? base)
   (if (= (length base) 5)
       #t
       #f
       ) 
)


; Selectores
; Obtener el nombre del sistema
(define (obtenerNombreDocs base)
  (if (paradigmadocs? base)
      (car base)
      null
      )
  )

; Obtener la fecha del sistema
(define (obtenerFecha base)
  (if (paradigmadocs? base)
      (cadr base)
      null
      )
  )

; Obtener el encriptador
(define (obtenerEncriptador base)
  (if (paradigmadocs? base)
      (caddr base)
      null
      )
  )
; Obtener el desencriptador
(define (obtenerDesencriptador base)
  (if (paradigmadocs? base)
      (caddr base)
      null
      )
  )
; Obtener la lista de usuarios registrados
(define (obtenerListaUsersDoc base)
  (if (paradigmadocs? base)
      (list-ref base 4) ; Retornar ultimo elemento
      null
      )
)


; Modificadores
; 1) FUNCIÓN REGISTER
(define (register tdaParadocs fecha nombreUsuario pass)
    (if (and
         ; Verificadores
         (fecha? fecha)
         (not (existeUser? (obtenerListaUsersDoc tdaParadocs) (user nombreUsuario pass)))
         )
        ; Caso verdadero -> se puede registrar
        (paradigmadocs (obtenerNombreDocs tdaParadocs)
                       fecha
                       (obtenerEncriptador tdaParadocs)
                       (obtenerDesencriptador tdaParadocs)
                       (agregarUser (obtenerListaUsersDoc tdaParadocs) (user nombreUsuario pass))
        )
        ; Caso falso -> no se puede registrar
        null
        
    )
)

#|
; Register uso de máquina
; Lista = tdaParadocs fecha nombreUsuario pass
(define (registerMaquina lista) ; -> Usar recursión natural de máximo 5 llamados
  (if (null? (cdr lista)) ; Si queda un elemento
      (car lista) ; Retorno el último elemento
      (list (car lista) (registerMaquina (cdr lista))) ; Concadeno la cabeza con el resto
      )
  
  )

(define (registerMaquina lista) ; -> Usar recursión natural de máximo 5 llamados
  (if (null? (cdr lista)) ; Si queda un elemento
      (car lista) ; Retorno el último elemento
      (paradigmadocs ) ; Concadeno la cabeza con el resto
     )
  
  )

; Register uso de usuario
(define (register tdaParadocs fecha nombreUsuario pass)
  (if 
        ; Verificadores
        (and
            
            (fecha? fecha) ; Fecha
            ; Existencia de usuario
            (not (existeUser? (obtenerListaUsersDoc tdaParadocs) (user nombreUsuario pass)))
        )
    ; Caso verdadero
    (registerMaquina (list tdaParadocs fecha nombreUsuario pass))
    ; Caso falso
    null
 )
)
|#


; 2) FUNCIÓN LOGIN
; Función que le permite al usuario entrar en la plataforma
; Dominio = paradigmadocs X string X string X function
; Recorrido = function
; Recorrido final = paradigmadocs
(define (login tdaParadigmaDocs usuario pass funcion)
  (if (existeUser? (obtenerListaUsersDoc tdaParadigmaDocs) (user usuario pass))
      ; Cambiar sesión activa y retornar funcion
      funcion
      (write "El usuario no existe")
      )
  )




; Otras operaciones
; 3) FUNCIÓN CREATE
; Función que permite al usuario crear un documento en la plataforma
; Dominio = paradigmadocs X fecha X string (nombre del documento) X string (contenido)
; Recorrido = paradigmadocs

#|
(define (create tdaParadigmaDoc fecha nombre contenido)
  
  )
|#




#|MAIN|#

(define admin(user "administrador" "usach"))
(define yo (user "danaxar" "rucio025"))
(define mama (user "paula" "2501"))
(define fran (user "matsu" "2501"))
(define hoy (date 23 10 2021))
(define sistema (paradigmadocs "paradoc" hoy
                               encriptador
                               desencriptador
                               (agregarUser (agregarUser (crearListaUsuario yo) mama) fran)))


(define usuarios_totales (crearListaUsuario admin))
;(define lista_documentos ())

; Principal -> resto de ejecuciones
(define (paradigmadocs1 nombreDocs fecha encriptador desencriptador)
  (if (and (string? nombreDocs) (fecha? fecha))
      ; Caso verdadero
        (list nombreDocs fecha encriptador desencriptador usuarios_totales)
      ; Caso falso
        null
  )
)


(provide paradigmadocs encriptador desencriptador register)
