;Descripción de la función
;Dominio (lo que entra)
;Recorrido (lo que retorna)
;Tipo de recursión
;Documentación del cuerpo



; Principal
#lang racket

; Función para imprimir las funciones dentro del docs
(define (printFunciones) 
    (write "1) Crear un documento") (newline)
    (write "2) Eliminar un documento") (newline)
    (write "3) Compartir un documento") (newline)
    (write "4) Asignar permisos sobre un documento") (newline)
    (write "5) Editar un documento") (newline)
)

; Función para mostrar por pantalla los archivos creados en paradigmasdocs
(define (printArchivos lista_archivos))

; me falta poder obtener la entrada de usuario
; En caso de no poderse usar una instrucción para que el usuario haga su propio llamado a la función







; Registrarse
(define verificar)

;EMPLEAR RECURSIÓN NATURAL
(define (register paradigmadocs fecha usuario contrasena)
    (write "Verificando...") (newline)
    
    (if (= verificar()))
)


; Iniciar sesión
; Prerequisito: Función register
; Emplear funciones de orden superior
; Retornar una función
; Usar recursión natural o de cola
; Dominio: paradigmadocs X string X string X function
; Recorrido: Function
; Recorrido final: paradigmadocs
(define (login paradigmadocs usuario contrasena operation))


; Operaciones sobre archivos

; Crear archivo
; Prerequisito: Función login
; La función no arroja ningún cambio si no se utiliza como parametro de entrada de login
; La información debe ser encriptada
; Dominio: paradigmadocs X date X String X (nombreDoc) X String (contenido)
; Recorrido: paradigmadocs
(define (create paradigmadocs fecha usuario contenido))


; Compartir archivo
; Prerequisitos: Función create
; No realiza ningún cambio si no se utiliza como parámetro de entrada en la función login
; Dominio: paradigmadocs X int X access list
; Recorrido: paradigmadocs
(define (share paradigmadocs id_documento ...............))




; Añadir texto al final del documento
; Prerequisito: Función share
; No arroja ningún cambio si no se utiliza como parámetro de entrada en la función login
; Solo pueden escribir los que tengan permiso de escritura (writing)
; El resultado obtenido por esta función es encriptada por encrypFunction
; Dominio: paradigmadocs X int X date X String
; Recorrido: Paradigmadocs
(define (add paradigmadocs id_documento fecha contenidoTexto))





; Prerequisito
;
; Dominio
; Recorrido




