#lang racket

; TDA de registro de ids

; Constructor
(define (registroID) (list 0))  ; Usar cuando se crea paradigmadocs

; Pertenencia
(define (isRegistroID listaID)
    (if (map integer? listaID)
        #t
        #f
    )
)

; Selectores
(define (obtenerID listaid indice)
    (list-ref listaid indice)
)

(define (obtenerPrimerID listaid)
    (list-ref listaid 0)
)

(define (obtenerUltimoID listaid)
    (list-ref listaid (- (length listaid) 1))
)

; Modificadores
(define (agregarID listaid)
    (append listaid (list (+ (obtenerUltimoID listaid) 1)) ) ; Usar cuando se crea un documento
)

(define (quitarID listaid id)  ; Usar cuando se borra un documento
    (remove id listaid)
)

(provide registroID) ; Constructor
(provide isRegistroID) ; Pertenencia
(provide agregarID quitarID) ; Modificadores
 