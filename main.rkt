;Descripción de la función
;Dominio (lo que entra)
;Recorrido (lo que retorna)
;Tipo de recursión
;Documentación del cuerpo



#lang racket

"Bienvenido a paradigmasdocs"
"¿Qué desea hacer?"
"1) Iniciar sesión"
"2) Registrarse"

"Escriba <(inicio __su_respuesta__)> para comenzar con la ejecución"

; Función que recibe la primera respuesta una vez el programa es iniciado en la cual te pide:
; Iniciar sesión o registrarte
; Dominio: Entra un numero entero
; Recorrido: Sale un numero entero
(define inicio
	(lambda(primeraRespuesta)
		; Aqui se decide si inicia sesión o se registra
		(if (= primeraRespuesta 1)
			"Ha escogido iniciar sesión"
			; Caso verdadero (iniciar sesión)
				; Pedir usuario y contraseña
				; Verificar que exista en algún registro
			"Ha escogido registrarse"
			"Por favor ingrese su nombre de nuevo usuario"
			; Caso falso (Registrarse)
				; Pedir usuario
				; Pedir contraseña
				; Pedir que confirme la contraseña
				; Escribir usuario y contraseña en registro
		)
	)
)


"Funciones disponibles"
"1) Crear un documento"
"2) Eliminar un documento"
"3) Compartir un documento"
"4) Asignar permisos sobre un documento"
"5) Editar un documento"

(define accion
	(lambda x
		(if (= x 1)
			; Caso verdadero
				; Crear un documento
			; Caso falso -> otras opciones
			(if (= x 2)
				; Caso verdadero
					; Eliminar un documento
				; Caso falso
				(if (= x 3)
					; Caso verdadero
						; Compartir un documento
					; Caso falso
					(if (= x 4)
						; Caso verdadero
							; Asignar permisos sobre un documento
						; Caso falso
						(if (= x 5)
							; Caso verdadero
							"Que acción desea hacer?"
							"a) Escribir"
							"b) Cambiar estilos a los textos (ej: cursiva, negrita, etc.)"
							"c) Buscar y reemplazar"
							"d) Copiar texto"
							"e) Pegar texto"
							"f) Cortar texto"
							"g) Crear títulos, subtítulos"
							"h) Crear un índice"
							(lambda(y)
								(if (= y a)
									; v
									(if (= y b)
										() ; v
										(if (= y c)
											() ; v
											(if (= y d)
												() ; v
												(if (= y e)
													() ; v
													(if (= y f)
														()
														(if (= y g)
															()
															() ; Opción h
														)
													)
												)
											)
										)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)
