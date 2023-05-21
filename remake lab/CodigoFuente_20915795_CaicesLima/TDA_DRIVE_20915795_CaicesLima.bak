#lang racket
(provide (all-defined-out))

;-----------------------------[ START OF: TDA drive ]-----------------------------

; REPRESENTACION: letter (char) X name (string) X cap (int) X content (list)

#| .............[ -> CONSTRUCTOR ].............

FUNCION CONSTRUCTORA DE LA UNIDAD DE DISCO

DESC: Función constructora del tda drive. Contiene la letra asignada a la unidad,
 el nombre de la unidad, la capacidad de la unidad y su contenido.

DOMINIO:
- letter (char)
- name (string)
- cap (int)
- content (list:folder/file)

RECORRIDO: drive (list)

|#

(define make-drive (lambda (letter ;0
                             name
                             cap
                             content)
                           (list
                             letter
                             name
                             cap
                             content)))

(define drive (lambda (letter name cap)
                        (make-drive  letter
                                     name
                                     cap
                                     null)))

#| .............[ -> SELECTORES ]............. |#

;_____________________________________

#| FUNCION SELECTORA DE LETRA DE UNIDAD

DESC: Función que obtiene la letra de la unidad de disco

DOMINIO: drive

RECORRIDO: letter(string)

|#
(define get-letter (lambda (drive-arg) (car drive-arg)))

;_____________________________________

#| FUNCION SELECTORA DE NOMBRE DE UNIDAD

DESC: Función que obtiene el nombre de la unidad de disco

DOMINIO: drive

RECORRIDO: drive-name(string)

|#
(define get-drive-name (lambda (drive-arg) (cadr drive-arg)))

;_____________________________________

#| FUNCION SELECTORA DE CAPACIDAD DE UNIDAD

DESC: Función que obtiene la capacidad de la unidad de disco

DOMINIO: drive

RECORRIDO: drive-cap(int)

|#
(define get-drive-cap (lambda (drive-arg) (caddr drive-arg)))

;_____________________________________

#| FUNCION SELECTORA DE CONTENIDO DE UNIDAD

DESC: Función que obtiene el contenido de la unidad de disco

DOMINIO: drive

RECORRIDO: drive-content(list)

|#
(define get-drive-content (lambda (drive-arg) (list-ref drive-arg 3)))
