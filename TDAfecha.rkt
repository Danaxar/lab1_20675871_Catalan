; TDA fecha

;Descripción de la función
;Dominio (lo que entra)
;Recorrido (lo que retorna)
;Tipo de recursión
;Documentación del cuerpo

; Representación -> dd - mm - yyyy
; Dominio: int X int X int


; dia del mes
; mes
; año

; Constructores
; Función que construye el TDA fecha
; Dominio: int X int X int
; Recorrido: fecha
(define (date dia mes ano)
    (if 
        ; Condiciones que deben cumplir las entradas
        (and
            ; Dia
            (integer? dia)
            (>= dia 1)
            (<= dia 31)
            ; Mes
            (integer? mes)
            (>= mes 1)
            (<= mes 12)
            ; Año
            (integer? ano)
            (>= ano 1974)
        )
        
        ; Caso Verdadero -> construcción del tda
        (list dia mes ano)

        ; Caso falso -> no construcción del tda
        null
    )
)
; FUNCIONA BIEN

; Funciones de pertenencia
; Función que verifica si un numero corresponde a un dia
; Dominio: int
; Recorrido: boolean
(define (esDia d)
    (if (and (integer? d) (>= d 1) (<= d 31))
        ; Caso verdadero
        #t
        ; Caso falso
        #f
    )
)
; FUNCIONA

; Función que determina si un numero es mes
; Dominio: int
; Recorrido: Boolean
(define (esMes m)
    (if (and (integer? m) (>= m 1) (<= m 12))
        ; Caso verdadero
        #t
        ; Caso falso
        #f
    )
)
; FUNCIONA

; Función que determina si un numero es Año
; Dominio: int
; Recorrido: boolean
(define (esAno a)
    (if (and (integer? a) (>= a 1974))
        ; Caso verdadero
        #t
        ; Caso falso
        #f
    )
)
; FUNCIONA


; Función que determina si un numero es una fecha
; Dominio: list: int x int x int
; Recorrido: boolean
(define (esFecha f)
    (if (and (= (length f) 3)  (esDia (car f)) (esMes (car(cdr f)))  (esAno (car (cdr (cdr f))))  )
        ; caso verdadero
        #t
        ; caso falso
        #f
    )
)
; FUNCIONA



; SELECTORES
; Función que permite obtener un día de una fecha
; Dominio: list: int x int x int
; Recorrido: int
(define (obtenerDia f)
    (if (esFecha f)
        ; Caso verdadero
        (car f)
        ; Caso falso
        null
    )
)
; FUNCIONA

; Función que permite obtener un mes de una fecha
; Dominio: list: int x int x int
; Recorrido: int
(define (obtenerMes f)
    (if (esFecha f)
        ; Caso verdadero
        (cadr f)
        ; Caso falso
        null
    )
)
; FUNCIONA

; Función que permite obtener un Año de una fecha
; Dominio: list: int x int x int
; Recorrido: int
(define (obtenerAno f)
    (if (esFecha f)
        ; Caso verdadero
        (caddr f)
        ; Caso falso
        null
    )
)
; FUNCIONA




; MODIFICADORES

; Modificar el día
; Función que permite modificar un día de una fecha
; Dominio: list: int x int x int
; Recorrido: fecha
(define (modificarDia fecha nuevoDia)
    (if (esFecha fecha)
        ; Caso verdadero
        (list nuevoDia (obtenerMes fecha) (obtenerAno fecha))
        ; Caso falso
        null
    )
)
; FUNCIONA

; Modificar el mes
; Función que permite modificar un mes de una fecha
; Dominio: list: int x int x int
; Recorrido: fecha
(define (modificarMes fecha nuevoMes)
    (if (esFecha fecha)
        ; Caso verdadero
        (list (obtenerDia fecha) nuevoMes (obtenerAno fecha))
        ; Caso falso
        null
    )
)

; Modificar el Año
; Función que permite modificar un Año de una fecha
; Dominio: list: int x int x int
; Recorrido: fecha
(define (modificarAno fecha nuevoAno)
    (if (esFecha fecha)
        ; Caso verdadero
        (list (obtenerDia fecha) (obtenerMes fecha) nuevoAno)
        ; Caso falso
        null
    )
)

