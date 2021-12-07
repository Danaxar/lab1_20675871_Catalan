#lang racket
; TDA fecha


; ----------------- REPRESENTACIÓN ------------------------
; Representación -> dd - mm - yyyy: int X int X int

;Descripción de la función
;Dominio (lo que entra)
;Recorrido (lo que retorna)
;Tipo de recursión
;Documentación del cuerpo



; ------------------ CONSTRUCTOR ----------------------------
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
    (>= ano 0)
    )
        
   ; Caso Verdadero -> construcción del tda
   (list dia mes ano)

   ; Caso falso -> no construcción del tda
   null
   )
  )


; ------------------ PERTENENCIA ------------------
; Función que verifica si un numero corresponde a un dia
; Dominio: int
; Recorrido: boolean
(define (dia? d)
  (if (and (integer? d) (>= d 1) (<= d 31))
      ; Caso verdadero
      #t
      ; Caso falso
      #f
      )
  )


; Función que determina si un numero es mes
; Dominio: int
; Recorrido: Boolean
(define (mes? m)
  (if (and (integer? m) (>= m 1) (<= m 12))
      ; Caso verdadero
      #t
      ; Caso falso
      #f
      )
  )


; Función que determina si un numero es Año
; Dominio: int
; Recorrido: boolean
(define (ano? a)
  (if (and (integer? a) (>= a 0))
      ; Caso verdadero
      #t
      ; Caso falso
      #f
      )
  )



; Función que determina si un numero es una fecha
; Dominio: list: int x int x int
; Recorrido: boolean
(define (fecha? f)
  (if (and (= (length f) 3)  (dia? (car f)) (mes? (car(cdr f)))  (ano? (car (cdr (cdr f))))  )
      ; caso verdadero
      #t
      ; caso falso
      #f
      )
  )




; ------------------ SELECTORES ------------------
; Función que permite obtener un día de una fecha
; Dominio: list: int x int x int
; Recorrido: int
(define (obtenerDia f)
  (if (fecha? f)
      ; Caso verdadero
      (car f)
      ; Caso falso
      null
      )
  )


; Función que permite obtener un mes de una fecha
; Dominio: list: int x int x int
; Recorrido: int
(define (obtenerMes f)
  (if (fecha? f)
      ; Caso verdadero
      (cadr f)
      ; Caso falso
      null
      )
  )


; Función que permite obtener un Año de una fecha
; Dominio: list: int x int x int
; Recorrido: int
(define (obtenerAno f)
  (if (fecha? f)
      ; Caso verdadero
      (caddr f)
      ; Caso falso
      null
      )
  )





; ------------------ MODIFICADORES ------------------
; Modificar el día
; Función que permite modificar un día de una fecha
; Dominio: list: int x int x int
; Recorrido: fecha
(define (modificarDia fecha nuevoDia)
  (if (and (fecha? fecha) (dia? nuevoDia))
      ; Caso verdadero
      (list nuevoDia (obtenerMes fecha) (obtenerAno fecha))
      ; Caso falso
      fecha
      )
  )


; Modificar el mes
; Función que permite modificar un mes de una fecha
; Dominio: list: int x int x int
; Recorrido: fecha
(define (modificarMes fecha nuevoMes)
  (if (and (fecha? fecha) (mes? nuevoMes))
      ; Caso verdadero
      (list (obtenerDia fecha) nuevoMes (obtenerAno fecha))
      ; Caso falso
      fecha
      )
  )

; Modificar el Año
; Función que permite modificar un Año de una fecha
; Dominio: list: int x int x int
; Recorrido: fecha
(define (modificarAno fecha nuevoAno)
  (if (and (fecha? fecha) (ano? nuevoAno))
      ; Caso verdadero
      (list (obtenerDia fecha) (obtenerMes fecha) nuevoAno)
      ; Caso falso
      fecha
      )
  )

; ------------------ OTROS ------------------
; Exportar fecha a string
(define (fecha->string fecha)
  (string-append
   (number->string (obtenerDia fecha)) " "
   (number->string (obtenerMes fecha)) " "
   (number->string (obtenerAno fecha))
   )
  )
(provide fecha->string)
  


(provide date) ; Constructor
(provide dia? mes? ano? fecha?) ; Pertenencia
(provide obtenerDia obtenerMes obtenerAno) ; Selectores
(provide modificarDia modificarMes modificarAno) ; Modificadores
