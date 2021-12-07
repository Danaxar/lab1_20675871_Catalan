#| MAIN |#
#lang racket

; Importaciones TDA's
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
(require "TDAregistroid.rkt")
(require "TDAlistausuario.rkt")
(require "TDAdocumento.rkt")
(require "TDAlistadocumento.rkt")
(require "TDAacceso.rkt")
(require "TDAlistaaccesos.rkt")
(require "TDAparadigmadocs1.rkt")



#|FUNCIÓN CONSTRUCTOR PARADIGMADOCS (en archivo paradigmadocs1.rkt

(define (paradigmadocs name date encryptFunction decryptFunction)
  (if  (fecha? date)
       (list name date encryptFunction decryptFunction lista_usuarios lista_documentos userActivo)
       null
       )
  )
(provide paradigmadocs)


|#

; 1) FUNCIÓN REGISTER
#|
Función que registra a un usuario en el sistema
Dominio = paradigmadocs X date X string X string
Recorrido = paradigmadocs
|#
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
#|
Función que entrar al usuario al sistema permitiendole ejecutar las funciones del programa
Dominio = paradigmadocs X string X string X function
Recorrido = paradigmadocs
|#
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
      (if
       (and (fecha? fecha) (string? nombreDoc) (string? contenido))
       (agregarDocumentoDoc ; V
        
        ; Sistema
        paradoc
        
        ; Creación de tda documento
        (document 
         fecha 
         nombreDoc
         (encriptador contenido) ; Encriptar contenido
         (obtenerSesionActivaDoc paradoc) ; Usuario creador
         (accesess (access (obtenerNombre (obtenerSesionActivaDoc paradoc)) #\w))
         
         
         (+ ; Agregar el id incremental -> Sumar uno al del documento que tenga mayor indice
          (obtenerIdDocumento (obtenerUltimoDocumento (obtenerListaDocumentsDoc paradoc)))
          1)
         (* (+ (obtenerIdDocumento (obtenerUltimoDocumento (obtenerListaDocumentsDoc paradoc))) 1) -1)
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
           
           (document ; Documento final
            (obtenerFechaDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerNombreDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerContenidoDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerCreadorDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (agregarAcceso (obtenerListaAccesos (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento)) acceso)
            idDocumento
            (obtenerIdAntDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            )
           
           )
          
          ; f -> más de un acceso
          (reemplazarDocumentoDoc
           paradoc ; Sistema
           (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento) ; Documento inicial
           
           (document ; Documento final
            (obtenerFechaDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerNombreDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerContenidoDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (obtenerCreadorDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            (agregarAcceso2 (obtenerListaAccesos (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento)) acceso accesses)
            idDocumento
            (obtenerIdAntDocumento (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDocumento))
            )
           
           )
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
        
        ; Cambiar la lista agregando un documento nuevo (se guardan todas las versiones)
        ; Definición = (agregarDocumento lista documento)
        (agregarDocumento
         
         ; Lista
         (obtenerListaDocumentsDoc paradoc)
         
         ; Se agrega el documento ya encriptado
         (encriptarDocumento ; Encripto
          (obtenerEncryptDocs paradoc) 
          (agregarTexto ; Agrego el texto y generar nueva id
           (encriptarDocumento ; Desencripto
            (obtenerEncryptDocs paradoc)
            (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDoc) ; Obtengo el documento
            )
           contenidoTexto)
          )
         )
        (obtenerSesionActivaDoc paradoc)
        )

       ; Caso falso
       null ; -> Cambiar por paradoc

       )
      )
    )
  )

#|
6) Función restoreVersion
Función que permite restaurar una versión anterior solo al propietario del documento
Dominio = paradigmadocs X int X int
Recorrido = paradigmadocs|#

(define restoreVersion
  (lambda(paradoc)
    (lambda (idDoc version)
      ; Cuerpo de la función
      (if
       ; Condición
       (and
        (existeVersion (obtenerListaDocumentsDoc paradoc) idDoc)
        (propietario?
         (buscarDocumento (obtenerListaDocumentsDoc paradoc) idDoc) ; Documento
         (obtenerSesionActivaDoc paradoc) ; Usuario
         )
        )
       
       ; V -> actualizar lista de documentos
       (actualizarListaDocumentosDoc
        paradoc
        (restaurarVersionDocumento (obtenerListaDocumentsDoc paradoc) idDoc version) ; Aqui hay problemas -> no funciona
        )
       
       ; F
       paradoc
       )
      )
    )
  )

#| 7) Función revokeAllAccesses
Función que permite al usuario quitar todos los permisos otorgados a los demás ususarios
registrados en el sistema que no son dueños del documento
Dominio = paradigmadocs
Recorrido = paradigmadocs

no funciona.

|#
(define revokeAllAccesses
  (lambda (paradigmadocs)
    ; Cuerpo de la función
    (actualizarListaDocumentosDoc
     paradigmadocs
     ; lista de documentos (modificar)
     (quitarPermisosUsuarios
      (obtenerListaDocumentsDoc paradigmadocs)
      (obtenerSesionActivaDoc paradigmadocs))
     
     )
    )
  )


#|8) Función Search
No realizada
|#

#|9) Función paradigmas->string
Función que muestra por pantalla la información del sistema para que sea más
entendible para el usuario
dominio = paradigmadocs
recorrido = string
|#
(define paradigmadocs->string
  (lambda(paradigmadocs)
    ; Cuerpo de función
    (string-append
     ; Nombre del sistema
     (obtenerNombreDocs paradigmadocs) "\n"
     "Sesion Activa: " (obtenerNombre (obtenerSesionActivaDoc paradigmadocs)) "\n"
     "Fecha: " (fecha->string (obtenerFechaDocs paradigmadocs)) "\n"
     "Usuarios registrados:\n"
     (listaUsuarios->string (obtenerListaUsersDoc paradigmadocs))
     (listaDocumento->string
      (obtenerListaDocumentsDoc paradigmadocs)
      (obtenerDecryptDocs paradigmadocs))
     )
    ;
    )
  )


; CREACIÓN DE PARADIGMADOCS
(define FECHA (date 7 12 2021)) ; opcional, se puede ingresar manualmente también
; se definió con el fin de ahorrar tipeo en los ejemplos nada más.

; Inicialización del sistema (Cambiar nombre a gusto)
(define sistema (paradigmadocs "paradoc" FECHA encriptador desencriptador))


#| EJEMPLOS DE USO|#
; 1) FUNCIÓN REGISTER
(define sistema2 (register sistema FECHA "daniel" "123"))
(define sistema21 (register sistema FECHA "Francisco" "132"))
(define sistema22 (register sistema FECHA "Juan" "333"))

; 2) FUNCIÓN LOGIN
(define sistema29 (login sistema2 "daniel" "123" create))
(define sistema299 (login sistema2 "daniel" "123" share))
(define sistema2999 (login sistema2 "daniel" "123" add))

; 3) FUNCIÓN CREATE
(define sistema3 ((login sistema2 "daniel" "123" create) FECHA "primerdoc" "holaaaa"))
(define sistema31 ((login sistema2 "daniel" "123" create) FECHA "Segundo Documento" "Segundo Contenido"))
(define sistema32
  ((login sistema2 "daniel" "123" create)
   FECHA
   "Cien años de Soledad"
   (string-append
    "Muchos años después, frente al pelotón de fusilamiento, el coronel Aureliano Buendía "
    "habría de recordar aquella  tarde remota en que su padre lo llevó a conocer el hielo. "
    "Macondo era entonces una aldea de veinte casas de barro y cañabrava construidas a la orilla "
    "de un río de aguas diáfanas que se precipitaban por un lecho de piedras pulidas, blancas y "
    "enormes como huevos prehistóricos. El mundo era tan reciente, que muchas cosas carecían de "
    "nombre, y para mencionarlas había que señalarlas con el dedo." 
    )

   )
  )


; 4) FUNCIÓN SHARE
(define sistema4 ((login sistema3 "daniel" "123" share) 1 (access "fran" #\w) (access "admin" #\w)))
(define sistema41 ((login sistema3 "daniel" "123" share) 1 (access "Juan" #\w) (access "Francisco" #\r)))
(define sistema42 ((login sistema3 "daniel" "123" share) 1 (access "admin" #\r)))

; 5) FUNCIÓN ADD
(define sistema5((login sistema4 "daniel" "123" add) 1 FECHA "ESTO ESTÁ AGREGADO"))
(define sistema51 ((login sistema4 "daniel" "123" add) 1 FECHA "Texto agregado al final"))
(define sistema52 ((login sistema4 "daniel" "123" add) 1 FECHA "tEXTOaGREGADOaLFINAL"))

; 6) FUNCIÓN RESTOREVERSION
(define sistema6 ((login sistema5 "daniel" "123" restoreVersion) 2 1))
;(define sistema61 ((login sistema5 "daniel" "123" restoreVersion) 2 6))
;(define sistema61 ((login sistema5 "daniel" "123" restoreVersion) 2 2))
; Están comentados porque depende de la existencia de tales documentos
; De no existir no funcionaría

; 7) FUNCIÓN REVOKEALLACCESSES
(define sistema7 (login sistema6 "daniel" "123" revokeAllAccesses)) ; No funciona
; Reemplaza todos los documentos por el primero

; 8) FUNCIÓN PARADIGMADOCS->STRING
(define sistema8 (login sistema4 "daniel" "123" paradigmadocs->string))
(define sistema81 (login sistema32 "daniel" "123" paradigmadocs->string))
(define sistema82 (login sistema5 "daniel" "123" paradigmadocs->string))







