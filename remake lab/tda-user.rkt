#lang racket
(provide (all-defined-out))

;-----------------------------[ START OF: TDA user ]-----------------------------

; REPRESENTACION: name (string)

#| .............[ -> CONSTRUCTOR ].............

FUNCION CONSTRUCTORA DEL USUARIO

DESC: Función constructora del tda user. Contiene el nombre del usuario.

DOMINIO: - name (string)

RECORRIDO: user (string)

|#

(define user (lambda (name) name))
