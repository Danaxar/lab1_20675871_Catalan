; Tda lista documento

#lang racket

(require "TDAdocumento.rkt")
(require "TDAfecha.rkt")

; Representación
; (list documento1 documento2 ... documentoN)

; Constructor
(define (crearListaDocumento documento)
    (if (document? documento)
        (list documento)
        null
    )
)

; Pertenencia
(define (listaDocumento? listDoc)
    (if (null? (cdr listDoc)) ; Si queda un elemento
        #t
        (if (document? (car listDoc))
            (listaDocumento? (cdr listDoc))
            #f
        )
    )
)

; Selectores
(define (obtenerDocumento lista indice)
    (list-ref lista indice)
)

; Modificadores
(define (agregarDocumento lista documento)
    (if (document? documento)
        (append lista (list documento))
        lista
    )
)

(provide crearListaDocumento listaDocumento? obtenerDocumento agregarDocumento)