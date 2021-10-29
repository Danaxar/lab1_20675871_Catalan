; Tda lista documento

#lang racket

(require "TDAdocumento.rkt")
(require "TDAfecha.rkt")
; Otros
(define (EliminarPrimerDoc lista_documentos)
  (cdr lista_documentos)
  )


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

(define (obtenerUltimoDocumento lista_documentos)
  (list-ref lista_documentos (- (length lista_documentos) 1))
  )

(define (obtenerPrimerDocumento lista_documentos)
  (list-ref lista_documentos 0)
  )

; Buscador de documento por id
(define (buscarDocumento lista_documentos id)
  (if (null? lista_documentos)
      ; Caso verdadero
      null
      ; Caso falso
      (if (= (obtenerIdDocumento (obtenerPrimerDocumento lista_documentos))  id)
          ; Caso verdadero
          (obtenerPrimerDocumento lista_documentos)
          ; Caso falso
          (buscarDocumento (EliminarPrimerDoc lista_documentos) id)
      )
  )
)

; Obtener el indice de un documento
(define (obtenerIndiceDocumento lista_documentos documento)
  (define (obtenerIndiceDocumentoEncapsulado lista_documentos documento indice)
    (if (null? lista_documentos)
        null
        ; Si los id de los documentos son iguales
        (if (= (obtenerIdDocumento documento) (obtenerIdDocumento (obtenerPrimerDocumento lista_documentos)))
            indice
            (obtenerIndiceDocumentoEncapsulado (EliminarPrimerDoc lista_documentos) documento (+ indice 1))
            )
        )
    )
  (obtenerIndiceDocumentoEncapsulado lista_documentos documento 0)
  )

; Modificadores
(define (agregarDocumento lista documento)
    (if (document? documento)
        (append lista (list documento))
        lista
    )
)

; Reemplazar un documento

(define (reemplazarDocumento lista_documentos documentoInicial documentoFinal)
  (append (remove documentoInicial lista_documentos) documentoFinal)
  )
#|
; Tengo que practicar el uso de . en las funciones
(define (agregarPermisoDoc lista_documentos documento acceso)
  ()
  )
|#
(provide crearListaDocumento listaDocumento? obtenerDocumento agregarDocumento)
(provide obtenerUltimoDocumento obtenerIndiceDocumento reemplazarDocumento buscarDocumento)