#lang racket
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAlistausuario.rkt")
(require "TDAdocumento.rkt")
(require "TDAlistadocumento.rkt")
(require "TDAacceso.rkt")
(require "TDAlistaaccesos.rkt")

; Otras funciones
; Encriptador
(define (encriptador texto)
  (list->string (reverse (string->list texto)))
  )

; Encriptar el texto de un documento
(define (encriptarDocumento encriptador documento)
  (document
   (obtenerFechaDocumento documento)
   (obtenerNombreDocumento documento)
   (encriptador (obtenerContenidoDocumento documento)) ; Aqui hacer el cambio -> encriptar
   (obtenerCreadorDocumento documento)
   (obtenerListaAccesos documento)
   (obtenerIdDocumento documento)
   )
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
  
  (list
   name
   date
   encryptFunction
   decryptFunction
   lista_usuarios
   lista_documentos
   userActivo)
  
  )

; Pertenencia
; Evalua si el parametro es un paradigmadocs
; NO TERMINADO
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

; Agregar un permiso a un documento
; Dominio = paradigmadocs X acceso
(define (modificarListaPermiso paradoc acceso)
  (if (and (paradigmadocs? paradoc) (access? acceso))
      ; Caso verdadero
      (paradigmadocsImplicito
       (obtenerNombreDocs paradoc)
       (obtenerFechaDocs paradoc)
       (obtenerEncryptDocs paradoc)
       (obtenerDecryptDocs paradoc)
       (obtenerListaUsersDoc paradoc) 
       (append (obtenerListaDocumentsDoc paradoc)) ; No usar append
       #|Necesito obtener la lista de documentos
        Necesito cambiar un documento en especifico
        Necesito cambiar la lista de accesos de un documento
        Necesito agregar un acceso|#
       (obtenerSesionActivaDoc paradoc) ; Aqui hacer el cambio
       )
      ; Caso falso
      paradoc
      )
  )

; Reemplaza un documento por otro
; Dominio = paradigmadocs X document X document
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
(define login (lambda (paradoc username password function)
                ; Caso verdadero
                (if (and (string? username) (string? password) (paradigmadocs? paradoc)) ; Verificar entrada
                    ; Caso verdadero
                    (if (existeUser? (obtenerListaUsersDoc paradoc) (user username password)) ; Existencia usuario
                        (function
                         (cambiarSesionActivaDoc paradoc (user username password)) ; Retorna paradigmadocs
                         )
                        (write "El usuario no se encuentra registrado")
                        )
                    ; Caso falso
                    function
      
                    )
                )
  )

; 3) FUNCIÓN CREATE
; Función que le permite al usuario con una sesión iniciada crear un nuevo documento
; Dominio = paradoc X fecha X string X string
; Recorrido = paradigmasdocs
(define create
  (lambda (paradoc)
    (lambda (fecha nombreDoc contenido)
      ; Cuerpo de función
      (if (and (fecha? fecha) (string? nombreDoc) (string? contenido))
          (agregarDocumentoDoc ; V
           paradoc
           (document ; Creación de tda documento
            fecha 
            nombreDoc
            contenido
            (obtenerSesionActivaDoc paradoc) ; Usuario creador
            (accesess (access (obtenerNombre (obtenerSesionActivaDoc paradoc)) #\w))
            ; En esta linea hay que agregar el id incremental
            (+ (obtenerIdDocumento (obtenerUltimoDocumento (obtenerListaDocumentsDoc paradoc))) 1)
            )
           ) ; Lista de accesos -> creador
          paradoc ; F
          )
      )
    )
  )
; 4) FUNCIÓN SHARE
#|Función que permite compartir el documento con otros usuarios especificando
el tipo de acceso (lectura, escritura, comentarios)
Dominio = paradigmadocs X integer X accessList (o lista de usuarios)
Recorrido = paradigmadocs
|#
(define share
  (lambda (paradoc)
    (lambda (idDocumento acceso . accesses)
      ; Cuerpo de la función
      (if (null? accesses) ; Si solo hay un acceso
          ; t -> solo un acceso
          (reemplazarDocumentoDoc
           paradoc ; Sistema
           (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento) ; Documento inicial
           ; Documento final
           (document
            (obtenerFechaDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerNombreDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerContenidoDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerCreadorDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (agregarAcceso (obtenerListaAccesos (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento)) acceso)
            idDocumento
            )
           
           )
          
          ; f -> más de un acceso
          null
          )
      )
    )
  )

#|5) Función add
Función que permite que permite añadir texto al final del documento solo a
los que tengan acceso de escritura sobre el documento (propietario y extenos autorizados)
Dominio = paradigmadocs X int X date X String
Recorrido = paradigmadocs|#

(define add
  (lambda(paradoc)
    (lambda(idDoc fecha contenidoTexto)
      ; Cuerpo de la función
      ; Comprobar que se puede escribir en el documento
      (if
       ; Condición -> que tenga permiso de escritura sobre el documento
       (tienePermiso ; Aqui hay problemas
        (obtenerListaAccesos (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDoc))
        (obtenerNombre (obtenerSesionActivaDoc paradoc))
        #\w ; -> Permiso de escritura
        )

          
       ; Caso verdadero
       (paradigmadocsImplicito
        (obtenerNombreDocs paradoc)
        fecha
        (obtenerEncryptDocs paradoc)
        (obtenerDecryptDocs paradoc)
        (obtenerListaUsersDoc paradoc) 
        (reemplazarDocumentoDoc ; Cambiar la lista actualizando un documento en especifico
         paradoc
         (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDoc) ; Documento inicial -> buscado
         (encriptarDocumento (obtenerEncryptDocs paradoc) (agregarTexto (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDoc) contenidoTexto)))
        (obtenerSesionActivaDoc paradoc)
        )

       ; Caso falso
       paradoc

       )
      )
    )
  )

; (encriptarDocumento (obtenerEncryptDocs paradoc) (agregarTexto (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDoc) contenidoTexto))
; esta linea agrega texto y encripta

; Función para imprimir
; Para mostrar todo el sistema con toda la información
(define (ps base)
  (begin0
    (write (obtenerNombreDocs base)) (newline)
    (write "Fecha: ") (write (obtenerFechaDocs base)) (newline)
    (write "Usuarios registrados:")
    (write (obtenerListaUsersDoc base)) (newline) (newline)
    (write "Lista de documentos :") (write (obtenerListaDocumentsDoc base)) (newline) (newline)
    (write "Sesion activa: ") (write (obtenerSesionActivaDoc base)) (newline)
    ) 
  )

(define (printDocumento documento)
  (begin0
    (write "Nombre del archivo: ") (write (obtenerNombreDocumento documento)) (newline)
    (write "Autor: ") (write (obtenerCreadorDocumento documento)) (newline)
    (write "Permisos: ") (write (obtenerListaAccesos documento)) (newline)
    (write "Fecha: ") (write (obtenerFechaDocumento documento)) (newline)
    ; Contenido
    (write "Contenido: ") (newline)
    (write (obtenerContenidoDocumento documento)) (newline)
    )
  )

#|   MAIN     |#
(define fecha (date 4 11 2021))
(define administrador (user "admin" "usach"))
(define lista_usuarios (crearListaUsuario administrador))

(define null_document
  (document fecha " " " " administrador
            (accesess (access (obtenerNombre administrador) #\w))0
            )
  )




; Constantes: Necesarios para el constructor paradigmadocs
(define lista_documentos (crearListaDocumento null_document))
(define userActivo (user "admin" "usach"))

; Sistema inicial
(define sistema (paradigmadocs "paradoc" fecha encriptador desencriptador)) ; -> Inicialización del tda paradigmadocs
(define sistema2 (register sistema fecha "daniel" "123")) ; Registrandome en el sistema
(define sistema3 ((login sistema2 "daniel" "123" create) fecha "primerdoc" "hola XD")) ; Creando el primer documento
(define sistema4 ((login sistema3 "daniel" "123" share) 1 (access "fran" #\w))) ; Compartiendo el documento

; En desarrollo
(define sistema5
  ((login sistema4 "daniel" "123" add) ; Esto funciona
   1 fecha "ESTO ESTÁ AGREGADO"))
