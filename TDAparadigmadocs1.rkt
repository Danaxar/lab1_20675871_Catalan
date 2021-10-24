#lang racket
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAlistausuario.rkt")
(require "TDAdocumento.rkt")
(require "TDAlistadocumento.rkt")

#|
Cosas que contiene el paradigmadocs
    Parametros de función (explicitos)
        Nombre del sistema
        Fecha
        Función encriptadora
        Función desencriptadora
    
    Funciones internas (implicitas) ; -> hace llamado dentro del mismo código
        Lista de documentos totales
        Lista de usuarios registrados

|#

; Otras funciones
; Encriptador
(define (encriptador texto)
  (list->string (reverse (string->list texto)))
  )

; Desencriptador
(define (desencriptador texto)
  (encriptador texto)
  )


; Tda paradigma doc
; Constructor
(define (paradigmadocs name date encryptFunction decryptFunction)
  (if  (fecha? date)
       (list name date encryptFunction decryptFunction lista_usuarios lista_documentos userActivo)
       null
       )
  )

; Constructor implicito
(define (paradigmadocsImplicito
         name date
         encryptFunction decryptFunction
         lista_usuarios lista_documentos userActivo)
  
  (list name date encryptFunction decryptFunction lista_usuarios lista_documentos userActivo)
  
  )

; Pertenencia
(define (paradigmadocs? base)
  (if (and (string? (list-ref base 0))
           (fecha? (list-ref base 1))
           (= (length base) 7))
      #t
      #f
      )
  )

#|                     Selectores                   |#
#|Obtener el nombre del sistema
Dominio = paradigmadocs
Recorrido = boolean
|#
(define (obtenerNombreDocs paradocs)
  (if (paradigmadocs? paradocs)
      (list-ref paradocs 0)
      null
      )
  )

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

#|                          MODIFICADORES                          |#

#|Agregar un usuario al sistema
Dominio = paradigmadocs
Recorrido = paradigmadocs
|#
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



; 1) FUNCIÓN REGISTER
(define (register paradoc date username password)
  (if (and (fecha? date) (string? username) (string? password))
      ; Caso true, cumplen los tipos de dato
      (if (not (existeUser? (obtenerListaUsersDoc paradoc) (user username password)))
          (agregarUsuarioDoc paradoc (user username password))
          (write "usuario ya existente")
          )
      paradoc ; Caso falso -> no cumplen los tipos de dato
      )
  
  )

; 2) FUNCIÓN LOGIN
(define (login paradoc username password function)
  (if (paradigmadocs? paradoc)
      ; Caso verdadero
      (if (and (string? username) (string? password))
          ; Caso verdadero
          (if (existeUser? (obtenerListaUsersDoc paradoc) (user username password))
              ; Caso verdadero
              function
              ; Caso falso
              (write "El usuario no se encuentra registrado")
              )
          ; Caso falso
          (write "Datos de usuario invalidos")
          )
      ; Caso falso
      (write "El sistema ingresado es incorrecto")
      )
  )

; 3) FUNCIÓN CREATE
; Función que le permite al usuario con una sesión iniciada crear un nuevo documento
; Dominio = paradoc X fecha X string X string
; Recorrido = paradigmasdocs
(define (create paradoc  fecha nombreDoc contenido)
  (if (and (fecha? fecha) (string? nombreDoc) (string? contenido))
      (agregarDocumentoDoc paradoc (document fecha nombreDoc contenido))
      paradoc
  )
)

; 4) FUNCIÓN SHARE
#|Función que permite compartir el documento con otros usuarios especificando
el tipo de acceso (lectura, escritura, comentarios)
Dominio = paradigmadocs X integer X accessList (o lista de usuarios)
Recorrido = 
|#








(define (share paradoc idDocumento ))















(define fecha (date 4 11 2021))
(define administrador (user "admin" "usach"))
(define administrador2 (user "admin2" "usach"))
(define lista_usuarios (crearListaUsuario administrador))
(define lista_usuarios2 (agregarUser lista_usuarios administrador2))

(define null_document (document fecha "" ""))
(define lista_documentos (crearListaDocumento null_document))
(define userActivo (user "admin" "usach"))

; Sistema inicial
(define sistema (paradigmadocs "paradoc" fecha encriptador desencriptador))


