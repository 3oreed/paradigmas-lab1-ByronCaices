#lang racket

(provide (all-defined-out))

;-----------------------------[ START OF: TDA file ]-----------------------------

; REPRESENTACION: file-name (string) X extension (string) X text (string) X
; location (string) X password (string) X create-date (string) X mod-date (string) X
; security (list) X creator (string) X type (string)

#| .............[ -> CONSTRUCTOR ].............

FUNCION CONSTRUCTORA DEL ARCHIVO

DESC: Función constructora del tda file. Contiene el nombre del archivo, la extensión,
 el texto contenido en el archivo, la ubicación, la contraseña, la fecha de creación,
 la fecha de modificación, la seguridad, el creador y el tipo de archivo.

DOMINIO:
- file-name (string)
- extension (string)
- text (string)
- location (string)
- password (string)
- create-date (date)
- mod-date (date)
- security (list:char) ;Puede ser un booleano o una cadena de texto dependiendo de cómo se maneje la seguridad
- creator (user)
- type (string)

RECORRIDO: file (list)

|#

(define make-file (lambda (file-name ;0
                              extension ;1
                              text ;2
                              location ;3
                              password ;4
                              create-date ;5
                              mod-date ;6
                              security ;7
                              creator ;8
                              type);9
                        (list file-name ;0
                              extension ;1
                              text ;2
                              location ;3
                              password ;4
                              create-date ;5
                              mod-date ;6
                              security ;7
                              creator ;8
                              type))) ;9

(define file (lambda (name ext text . security)
               (make-file name
                          ext
                          text
                          "" ;sec
                          "" ;pass
                          null
                          null
                          security
                          ""
                          "")))

#| .............[ -> SELECTORES ]............. |#

;_____________________________________

#| FUNCION SELECTORA DE NOMBRE DE ARCHIVO

DESC: Función que obtiene el nombre del archivo

DOMINIO: file

RECORRIDO: file-name(string)

|#
(define (get-file-name file-arg) (list-ref file-arg 0))

;_____________________________________

#| FUNCION SELECTORA DE EXTENSIÓN

DESC: Función que obtiene la extensión del archivo

DOMINIO: file

RECORRIDO: extension(string)

|#
(define (get-extension file-arg) (list-ref file-arg 1))

;_____________________________________

#| FUNCION SELECTORA DE TEXTO

DESC: Función que obtiene el contenido del archivo

DOMINIO: file

RECORRIDO: text(string)

|#
(define (get-text file-arg) (list-ref file-arg 2))

;_____________________________________

#| FUNCION SELECTORA DE SEGURIDAD DE ARCHIVO

DESC: Función que obtiene la seguridad del archivo

DOMINIO: file

RECORRIDO: security(list)

|#
(define (get-file-security file-arg) (list-ref file-arg 7))

;_____________________________________

#| FUNCION SELECTORA DE CONTRASEÑA DE ARCHIVO

DESC: Función que obtiene la contraseña del archivo

DOMINIO: file

RECORRIDO: password(string)

|#
(define (get-file-password file-arg) (list-ref file-arg 4))

;_____________________________________

#| FUNCION SELECTORA DE FECHA DE CREACIÓN DE ARCHIVO

DESC: Función que obtiene la fecha de creación del archivo

DOMINIO: file

RECORRIDO: create-date(date)

|#
(define (get-create-date-file file-arg) (list-ref file-arg 5))

;_____________________________________

#| FUNCION SELECTORA DE FECHA DE MODIFICACIÓN DE ARCHIVO

DESC: Función que obtiene la fecha de modificación del archivo

DOMINIO: file

RECORRIDO: mod-date(date)

|#
(define (get-mod-date-file file-arg) (list-ref file-arg 6))

;_____________________________________

#| FUNCION SELECTORA DE UBICACIÓN DE ARCHIVO

DESC: Función que obtiene la ubicación del archivo

DOMINIO: file

RECORRIDO: location(string)

|#
(define (get-file-location file-arg) (list-ref file-arg 3))

;_____________________________________

#| FUNCION SELECTORA DE CREADOR DE ARCHIVO

DESC: Función que obtiene el creador del archivo

DOMINIO: file

RECORRIDO: creator(string)

|#
(define (get-file-creator file-arg) (list-ref file-arg 8))

;_____________________________________

#| FUNCION SELECTORA DE TIPO DE ARCHIVO

DESC: Función que obtiene el tipo del archivo

DOMINIO: file

RECORRIDO: type(string)

|#
(define (get-file-type file-arg) (list-ref file-arg 9))

