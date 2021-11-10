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

; Función que verifica existencia de version
(define (existeVersion lista1 id)
  (if (null? lista1)
      #f
      (if (eqv? (obtenerIdDocumento (obtenerPrimerDocumento lista1)) id)
          #t
          (existeVersion (cdr lista1) id)
          )
      )
  )
(provide existeVersion)


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
; Retorna el numero de la posición que ocupa un documento en una lista
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

; Obtener el documento con mayor indice
(define (obtenerMaxIndiceDocumento lista_documentos)
  
  ; Encapsulación
  (define (obtenerMaxIndiceDocumentoEncapsulado lista_documentos maximo)
    (if (null? lista_documentos)
        ; V -> cuando la lista esté vacía retorno el maximo resultante
        maximo
        ; F
        (if
         ; Condición -> Si el id del primer documento es mayor que el maximo
         (> (obtenerIdDocumento (obtenerPrimerDocumento lista_documentos)) maximo)
         ; V -> Aqui cambia el max 
         (obtenerMaxIndiceDocumentoEncapsulado
          (cdr lista_documentos) ; Avanzo en la lista
          (obtenerIdDocumento (obtenerPrimerDocumento lista_documentos)) ; Cambio el maximo
          ) 
         ; F -> Aqui no cambia el max
         (obtenerMaxIndiceDocumentoEncapsulado
          (cdr lista_documentos) ; Avanzo en la lista
          maximo)
         )
      
        )
    )
  
  ; Llamar a la función
  (obtenerMaxIndiceDocumentoEncapsulado
   lista_documentos
   (obtenerIdDocumento
    (obtenerPrimerDocumento lista_documentos)))
  )

; Modificadores
(define (agregarDocumento lista documento)
  (if (document? documento)
      (append lista (list documento)) ; Notar el "list" que es clave para que funcione
      lista
      )
  )

; (buscarDocumento lista_documentos id)
; Retornar la versión anterior de un documento
(define (versionAnteriorDocumento lista_documentos id)
  (buscarDocumento
   lista_documentos ; lista
   (obtenerIdAntDocumento (buscarDocumento lista_documentos id)) ; id
   )
  )

; Eliminar todas las versiones superiores hasta llegar al id indicado (restaurar versión)
; Usando recursión natural

; Si el id del documento es equivalente al id del documento anterior buscado

; Dominio = listaDocumentos X entero X entero
; Recorrido = listaDocumentos

#|
Condiciones para eliminar un documento:
    Los nombres deben ser iguales
    Si dado que los nombres son iguales
        Si el id es mayor que el buscado
            Quitar el documento de la lista
|#
#|
(define (restaurarVersionDocumento lista_documentos idDoc version lista2)
  (if
   (eqv? ; Comparar nombres    
    (obtenerNombreDocumento ; Nombre primer documento
     (obtenerPrimerDocumento lista_documentos))
    (obtenerNombreDocumento ; Nombre documento buscado
     (buscarDocumento lista_documentos idDoc))
    )
    
   ; V  -> Comparar ids
   (if
    ; Comparar ids
    (> ; Si el id del primer documento es mayor -> borrar
     (obtenerIdDocumento (obtenerPrimerDocumento lista_documentos)) ; Primer documento
     (obtenerIdDocumento (buscarDocumento lista_documento idDoc)) ; Documento buscado
     )
    ; V -> Caso arbitrario -> borrar
    (restaurarVersionDocumento
     (remove ; lista
      (obtenerPrimerDocumento lista_documentos)
      lista_documentos)
     idDoc ; idDoc
     version ; Version
     )
    
    ; F -> 
    
   
       
    )
   )
  )
|#

; Asumiendo que existe el id
(define (restaurarVersionDocumento lista1 idDoc version)
  ; Encapsulación
  (define (restaurarVersionDocumentoEncapsulado lista1 idDoc version lista2 contador)
    ; Cuerpo de la función
    (if
     (eqv? ; Comparar nombres    
      (obtenerNombreDocumento (obtenerPrimerDocumento lista1)) ; Nombre primer documento
      (obtenerNombreDocumento (buscarDocumento lista2 idDoc )) ; Nombre documento buscado
      )
    
     ; V  -> Comparar ids
     (if
      (> ; Si el id del primer documento es mayor -> borrar
       (obtenerIdDocumento (obtenerPrimerDocumento lista1)) ; Primer documento
       (obtenerIdDocumento (buscarDocumento lista2 idDoc)) ; Documento buscado
       )
      ; V -> Quitar elemento
      (restaurarVersionDocumentoEncapsulado
       (cdr lista1) ; Recorrer documento
       idDoc version
       (remove (obtenerDocumento lista2 contador) lista2)
       contador
       )
      ; F -> Es igual
      lista2
      )
     ; F -> Seguir recorriendo
     (restaurarVersionDocumentoEncapsulado (cdr lista1) idDoc version lista2 (+ contador 1))
     )
    )
  ; Llamado a la función
  (restaurarVersionDocumentoEncapsulado lista1 idDoc version lista1 0)
  )

(provide restaurarVersionDocumento)




#|Función de reemplazo inicial   
; Reemplazar un documento -> Aqui está el error! Lo hace mal
(define (reemplazarDocumento lista_documentos documentoInicial documentoFinal)
  (append (remove documentoInicial lista_documentos) documentoFinal)
  )
|#

; Función de reemplazo en mejora
(define (reemplazarDocumento lista_documentos documentoInicial documentoFinal)
  (append
   (remove documentoInicial lista_documentos) ; Esto retorna la lista sin el elemento
   (list documentoFinal) ; Lo junto con el documento final
   )
  )


(provide crearListaDocumento listaDocumento? obtenerDocumento agregarDocumento)
(provide obtenerUltimoDocumento obtenerIndiceDocumento reemplazarDocumento buscarDocumento)
(provide obtenerPrimerDocumento)
(provide obtenerMaxIndiceDocumento)