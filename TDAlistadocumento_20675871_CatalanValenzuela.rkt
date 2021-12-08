#lang racket

(require "TDAdocumento_20675871_CatalanValenzuela.rkt")
(require "TDAfecha_20675871_CatalanValenzuela.rkt")

; Tda lista documento

; ----------------- REPRESENTACIÓN ------------------------
; (list documento1 documento2 ... documentoN)

; ------------------ CONSTRUCTOR ----------------------------
#|Función que crea un tda listaDocumento
Dominio = document
Recorrido = listaDocument|#
(define (crearListaDocumento documento)
  (if (document? documento)
      (list documento)
      null
      )
  )

; ------------------ PERTENENCIA ------------------
#|Función que verifica que la lista sea de solo documentos
Dominio = listaDocumento
Recorrido = boolean|#
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
#|Dominio = listaDocumento X int
Recorrido = boolean|#
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


; ------------------ SELECTORES ------------------
#|Función que obtiene el documento enésimo de la lista
Dominio = listaDocumento X int
Recorrido = document|#
(define (obtenerDocumento lista indice)
  (list-ref lista indice)
  )

#|Función que obtiene el último documento de la lista
Dominio = listaDocumento
Recorrido = document|#
(define (obtenerUltimoDocumento lista_documentos)
  (list-ref lista_documentos (- (length lista_documentos) 1))
  )

#|Función que obtiene el primer documento de la lista
Dominio = listaDocumento
Recorrido = document|#
(define (obtenerPrimerDocumento lista_documentos)
  (list-ref lista_documentos 0)
  )


; ------------------ OTROS ---------------------------
#|Función que elimina el primer documento de una lista de documentos
Dominio = listaDocumento
Recorrido = listaDocumento|#
(define (EliminarPrimerDoc lista_documentos)
  (cdr lista_documentos)
  )


; Buscador de documento por id
#|Dominio = listaDocumento
Recorrido = document|#
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
#|Dominio = listaDocumento X documento
Recorrido = int|#
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
#|Dominio = listaDocumento
Recorrido = document|#
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

; ------------------ MODIFICADORES ------------------
#|Función que agrega un documento a la lista
Dominio = listaDocumento X document
Recorrido = listaDocumento|#
(define (agregarDocumento lista documento)
  (if (document? documento)
      (append lista (list documento)) ; Notar el "list" que es clave para que funcione
      lista
      )
  )

#|Función que elimina un documento de la lista
Dominio = listaDocumento X documento
Recorrido = listaDocumento|#
(define (eliminarDocumento lista doc)
  (remove doc lista)
  )

; (buscarDocumento lista_documentos id)
; Retornar la versión anterior de un documento
#|Dominio = listaDocumento X int
Recorrido = document|#
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

; Asumiendo que existe el id
#|Dominio = listaDocumento X int X int
Recorrido = listaDocumento|#
(define (restaurarVersionDocumento lista1 idDoc version)
  ; Encapsulación
  (define (restaurarVersionDocumentoEncapsulado lista1 idDoc version lista2 contador)
    ; Cuerpo de la función
    (if (null? lista1)
        ; V -> Terminó de recorrer
        lista2
        
        ; F -> Seguir recorriendo
        (if (eqv? ; Comparar nombres    
             (obtenerNombreDocumento (obtenerPrimerDocumento lista1)) ; Nombre primer documento
             (obtenerNombreDocumento (buscarDocumento lista2 idDoc )) ; Nombre documento buscado
             )
    
            ; V  -> Comparar ids
            (if (> ; Si el id del primer documento es mayor -> borrar
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


    
    
    )
  ; Llamado a la función
  (restaurarVersionDocumentoEncapsulado lista1 idDoc version lista1 0)
  )

(provide restaurarVersionDocumento)

; Quitar permisos de un usuario
#|Dominio = listaDocumento X user
Recorrido = Document|#
(define (quitarPermisosUsuarios lista_documentos usuario)
  
  (define (quitarPermisosUsuariosEncapsulado lista_documentos usuario lista_nueva)
    ; Cuerpo de función
    (if
     ; Si las listas tienen la misma cantidad de elementos
     (= (length lista_documentos) (length lista_nueva))
     ; V
     lista_nueva ; Retorno la nueva lista
     
     ; F -> Sigo procesando -> copiar/modificar documentos a la lista nueva
     (quitarPermisosUsuariosEncapsulado
      lista_documentos
      usuario

      (append
       lista_nueva
       (list
        ; Documento modificado o no
        (if
         ; Comprar creadores del documento
         (eqv?
          (obtenerCreadorDocumento
           (obtenerPrimerDocumento lista_documentos))
          usuario
          )
         ; V -> Quitar acceso
         (quitarTodosLosPermisosDocumento (obtenerPrimerDocumento lista_documentos))
         ; F -> Hacer nada
         (obtenerPrimerDocumento lista_documentos)
         )
        )
       )

      )
     
     )
    )


  
  ; Llamar a la función
  (quitarPermisosUsuariosEncapsulado lista_documentos usuario (list))
  )
(provide quitarPermisosUsuarios)

; Función de reemplazo de documento
#|Dominio = listaDocumento X document X document
Recorrido = listaDocument|#
(define (reemplazarDocumento lista_documentos documentoInicial documentoFinal)
  (append
   (remove documentoInicial lista_documentos) ; Esto retorna la lista sin el elemento
   (list documentoFinal) ; Lo junto con el documento final
   )
  )


#|Función que quita el primer documento de una lista
dominio = listaDocumento
recorrido = listaDocumento
|#
(define (quitarPrimerDocumento lista_documentos)
  (cdr lista_documentos)
  )

#|Exportar la lista de documentos como string
Dominio = listaDocumento X Function
Recorrido = string|#
(define (listaDocumento->string lista_documento desencriptador)
  (define (listaDocumento->stringEncapsulado lista_documento resultado)
    (if (= (length lista_documento) 0)
        ; V
        resultado
        
        ; F
        (listaDocumento->stringEncapsulado
         ; Argumento 1
         (quitarPrimerDocumento lista_documento)
         ; Argumento 2
         (string-append
          resultado
          "\n\n"
          (document->string (obtenerPrimerDocumento lista_documento) desencriptador)
          )
         
         )
        
        )
    )

  (listaDocumento->stringEncapsulado lista_documento "")
  )

(provide listaDocumento->string)


(provide crearListaDocumento listaDocumento? obtenerDocumento agregarDocumento)
(provide obtenerUltimoDocumento obtenerIndiceDocumento reemplazarDocumento buscarDocumento)
(provide obtenerPrimerDocumento)
(provide obtenerMaxIndiceDocumento)