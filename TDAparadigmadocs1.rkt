#lang racket
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAlistausuario.rkt")
(require "TDAdocumento.rkt")
(require "TDAlistadocumento.rkt")
(require "TDAacceso.rkt")
(require "TDAlistaaccesos.rkt")


; ----------------- REPRESENTACIÓN ------------------------
; (list string X date X function X function X listaUsuarios X listaDocumentos X user)




; ------------------ MODIFICADORES ------------------



; ------------------ OTROS
; Encriptador
#|Función que encripta un string invirtiendo el orden en el que estos se encuentran
dominio: string
recorrido: string|#
(define (encriptador texto)
  (list->string (reverse (string->list texto)))
  )
(provide encriptador)

; Encriptar el texto de un documento
#|dominio: function X document
recorrido: document|#
(define (encriptarDocumento encriptador documento)
  (document
   (obtenerFechaDocumento documento)
   (obtenerNombreDocumento documento)
   (encriptador (obtenerContenidoDocumento documento)) ; Aqui hacer el cambio -> encriptar
   (obtenerCreadorDocumento documento)
   (obtenerListaAccesos documento)
   (obtenerIdDocumento documento)
   (obtenerIdAntDocumento documento)
   )
  )
(provide encriptarDocumento)

; Desencriptador
#|dominio: string
recorrido: string|#
(define (desencriptador texto)
  (encriptador texto)
  )
(provide desencriptador)


; Inicialización del sistema (no cambiar)
(define fecha (date 7 12 2021)) ; Constante fecha -> se puede cambiar pero no borrar
(define administrador (user "admin" "usach"))
(define lista_usuarios (crearListaUsuario administrador))
(define null_document
  (document fecha "DOCUMENTO VACIO" "#######" administrador
            (accesess (access (obtenerNombre administrador) #\w))
            0 0
            )
  )
; Constantes: Necesarios para el constructor paradigmadocs
(define lista_documentos (crearListaDocumento null_document))
(define userActivo (user "admin" "usach"))













; ------------------ CONSTRUCTORES ----------------------------
; Función que construye el tda paradigmadocs
; Dominio = string X date X function X function
; Recorrido = Paradigmadocs
(define (paradigmadocs name date encryptFunction decryptFunction)
  (if  (fecha? date)
       (list name date encryptFunction decryptFunction lista_usuarios lista_documentos userActivo)
       null
       )
  )
(provide paradigmadocs)



; Constructor implicito
#|Función que construye el tda paradigmadocs de manera implicita pidiendo todos los argumentos
dominio: string X date X function X function X listaUsuarios X listaDocumentos X user
recorrido: paradigmadocs|#
(define
  (paradigmadocsImplicito
   name date encryptFunction decryptFunction
   lista_usuarios lista_documentos userActivo)
  
  (list name date encryptFunction decryptFunction
        lista_usuarios lista_documentos userActivo)
  
  )
(provide paradigmadocsImplicito)

; ------------------ PERTENENCIA ------------------
; Evalua si el parametro es un paradigmadocs
; Dominio: paradigmadocs
; recorrido: boolean
(define (paradigmadocs? base)
  (if (and (string? (list-ref base 0))
           (fecha? (list-ref base 1))
           (= (length base) 7))
      #t
      #f
      )
  )
(provide paradigmadocs?)

; ------------------ SELECTORES ------------------
#|Obtener el nombre del sistema
Dominio = paradigmadocs
Recorrido = string
|#
(define (obtenerNombreDocs paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 0)
      null
      )
  )
(provide obtenerNombreDocs)

#|Obtener la fecha del sistema
Dominio = paradigmadocs
Recorrido = date
|#
(define (obtenerFechaDocs paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 1)
      null
      )
  )
(provide obtenerFechaDocs)

#|Obtener la función encriptadora
Dominio = paradigmadocs
Recorrido = encryptFunction
|#
(define (obtenerEncryptDocs paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 2)
      null
      )
  )
(provide obtenerEncryptDocs)

#|Obtener la función desencriptadora
Dominio = paradigmadocs
Recorrido = decryptFunction
|#
(define (obtenerDecryptDocs paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 3)
      null
      )
  )
(provide obtenerDecryptDocs)



#|Obtener la lista de usuarios registrados totales
Dominio = paradigmadocs
Recorrido = tda lista de usuarios
|#
(define (obtenerListaUsersDoc paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 4)
      null
      )
  )
(provide obtenerListaUsersDoc)


#|Obtener la lista de documentos totales
Dominio = paradigmadocs
Recorrido = tda lista de documentos
|#
(define (obtenerListaDocumentsDoc paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 5)
      null
      )
  )
(provide obtenerListaDocumentsDoc)


#|Obtener la sesión activa
Dominio = paradigmadocs
Recorrido = user
|#
(define (obtenerSesionActivaDoc paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 6)
      null
      )
  )
(provide obtenerSesionActivaDoc)



#|                          MODIFICADORES                          |#

#|Agregar un usuario al sistema
Dominio = paradigmadocs
Recorrido = paradigmadocs|#
(define (agregarUsuarioDoc paradoc usuario)
  (if (and (paradigmadocs? paradoc) (user? usuario))
      (paradigmadocsImplicito
       (obtenerNombreDocs paradoc)
       (obtenerFechaDocs paradoc)
       (obtenerEncryptDocs paradoc)
       (obtenerDecryptDocs paradoc)
       (agregarUser (obtenerListaUsersDoc paradoc) usuario) ; Aqui hacer el cambio
       (obtenerListaDocumentsDoc paradoc)
       (obtenerSesionActivaDoc paradoc)
       )
      paradoc
      )
  
  )
(provide agregarUsuarioDoc)

#|Agregar un documento al sistema
Dominio = paradigmadocs
Recorrido = paradigmadocs
|#
(define (agregarDocumentoDoc paradoc documento)
  (if (and (paradigmadocs? paradoc) (document? documento))
      ; Caso verdadero
      (paradigmadocsImplicito
       (obtenerNombreDocs paradoc)
       (obtenerFechaDocs paradoc)
       (obtenerEncryptDocs paradoc)
       (obtenerDecryptDocs paradoc)
       (obtenerListaUsersDoc paradoc) 
       (agregarDocumento (obtenerListaDocumentsDoc paradoc) documento) ; Aqui hacer el cambio
       (obtenerSesionActivaDoc paradoc)
       )
      ; Caso falso
      paradoc
      )
  )
(provide agregarDocumentoDoc)

#|Agregar cambiar sesión activa
Dominio = paradigmadocs
Recorrido = paradigmadocs
|#
(define (cambiarSesionActivaDoc paradoc usuario)
  (if (and (paradigmadocs? paradoc) (user? usuario))
      ; Caso verdadero
      (paradigmadocsImplicito
       (obtenerNombreDocs paradoc)
       (obtenerFechaDocs paradoc)
       (obtenerEncryptDocs paradoc)
       (obtenerDecryptDocs paradoc)
       (obtenerListaUsersDoc paradoc) 
       (obtenerListaDocumentsDoc paradoc)
       usuario ; Aqui hacer el cambio
       )
      ; Caso falso
      paradoc
      )
  )
(provide cambiarSesionActivaDoc)


; Reemplaza un documento por otro
; Dominio = paradigmadocs X document X document
; Recorrido = paradigmadocs
(define (reemplazarDocumentoDoc paradoc documentoInicial documentoFinal)
  (if (and (paradigmadocs? paradoc) (document? documentoFinal))
      ; Caso verdadero
      (paradigmadocsImplicito
       (obtenerNombreDocs paradoc)
       (obtenerFechaDocs paradoc)
       (obtenerEncryptDocs paradoc)
       (obtenerDecryptDocs paradoc)
       (obtenerListaUsersDoc paradoc) 
       (reemplazarDocumento (obtenerListaDocumentsDoc paradoc) documentoInicial documentoFinal) ; Borra y agrega
       (obtenerSesionActivaDoc paradoc)
       )
      ; Caso falso
      null
      )
  )
(provide reemplazarDocumentoDoc)

; Función que mantiene todos los valores de paradigmadocs menos la lista de documentos
; Dominio = paradigmadocs X listaDocumento
; Recorrido = paradigmadocs
(define (actualizarListaDocumentosDoc paradoc listaDocumentos)
  (paradigmadocsImplicito
       (obtenerNombreDocs paradoc)
       (obtenerFechaDocs paradoc)
       (obtenerEncryptDocs paradoc)
       (obtenerDecryptDocs paradoc)
       (obtenerListaUsersDoc paradoc) 
       listaDocumentos ; Aqui se cambia por el parametro dado
       (obtenerSesionActivaDoc paradoc)
       )
  )
(provide actualizarListaDocumentosDoc)
