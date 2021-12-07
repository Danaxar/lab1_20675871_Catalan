#lang racket
; TDA de registro de ids

; ------------------ OTROS
; Verificar que el id existe
; dominio = listaId X int
; recorrido = boolean
(define (existeID listaID id)
    (if (null? listaID)
        #f
        (if (= (car listaID) id)
            #t
            (existeID (cdr listaID) id)
        )
    )
)


; ----------------- REPRESENTACIÓN ------------------------
; integer X integer X ... X integer

; ------------------ CONSTRUCTOR ----------------------------
#|función que construye el tda registroID
dominio-> vacio
recorrido-> registroId|#
(define (registroID) (list 0))  ; Usar cuando se crea paradigmadocs


; ------------------ PERTENENCIA ------------------
#|Función que verifica que el elemento pasado como parámetro sea un tda registro id
dominio-> listaId
recorrido-> boolean|#
(define (isRegistroID listaID)
    (if (null? listaID)
        #t
        (if (integer? (car listaID))
            (isRegistroID (cdr listaID))
            #f
            )
    )
)



; ------------------ SELECTORES ------------------
#|Función que obtiene un id dado un indice
dominio-> listaId
recorrido-> int|#
(define (obtenerID listaid indice)
    (list-ref listaid indice)
)

#|Función que obtiene el primer id de la lista
dominio-> listaId
recorrido-> int|#
(define (obtenerPrimerID listaid)
    (list-ref listaid 0)
)

#|Función que obtiene el ultimo id de la lista
dominio-> listaId
recorrido-> int|#
(define (obtenerUltimoID listaid)
    (list-ref listaid (- (length listaid) 1))
)

; ------------------ MODIFICADORES ------------------
#|Función que agrega un id a la lista
domino-> listId
recorrido-> listaId|#
(define (agregarID listaid)
    (append listaid (list (+ (obtenerUltimoID listaid) 1)) ) ; Usar cuando se crea un documento
)

#|Función que quita un id de la lista dado un id de referencia
dominio-> listaId X int
recorrido-> listaId|#
(define (quitarID listaid id)  ; Usar cuando se borra un documento
    (remove id listaid)
)



(provide registroID) ; Constructor
(provide isRegistroID) ; Pertenencia
(provide agregarID quitarID) ; Modificadores
 