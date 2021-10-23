#lang racket
; TDA documento



; Cosas que debe contener un documento:
; Texto albergado            -> TDA texto
; Fecha de creación          -> tda fecha
; Modificación               -> palabras
; ID único incremental  (se va generando un correlativo a partir de del último documento o versión creada)
;                            -> numero entero
; Version -> numero entero
; Historial de Versiones     -> lista de tda documento
; Usuarios que tienen acceso -> lista de tda usuarios


; Implementación:
; Representación

; Constructores
; Función que construye un tda documento
; Dominio: string x fecha x entero x (list integer) x (list tda user)
(define (document texto fecha id historial lista_UsuariosAcceso)
    (if (and
            (string? texto)
            (fecha? fecha)
            (integer? id)
            ; Agregar algo que verifique que toda la lista de documentos sean de documentos
            #|
            (if (= (length historial) 0)
                null
                (map (document? historial))
            )
            |#
            (map user? lista_UsuariosAcceso)
        )

        ; Caso verdadero
        (list texto fecha id historial lista_UsuariosAcceso)
        ; Caso falso
        null
    )
)
; Pertenencia
; Función que verifica si el parametro de entrada es un documento
; Dominio: documento
; Recorrido: boolean
(define (document? documento)
    (if (and 
            (= (length documento) 5)
            (string? (car documento))
            (fecha? (cadr documento))
            (integer? (caddr documento))
            (map integer? (cadddr documento))
            (map user? (car (cdr (cdr (cdr(cdr documento))))))
        )
        #t
        #f
    )
)

; Selectores

; Modificadores
; Otras operaciones
