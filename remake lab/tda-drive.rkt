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

(define get-letter (lambda (drive-arg)
                     (car drive-arg)))

(define get-drive-name (lambda (drive-arg)
                     (cadr drive-arg)))

(define get-drive-cap (lambda (drive-arg)
                     (caddr drive-arg)))

(define get-drive-content (lambda (drive-arg)
                     (list-ref drive-arg 3)))
