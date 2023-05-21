#lang racket

(provide (all-defined-out))

;-----------------------------[ START OF: TDA folder ]-----------------------------

; REPRESENTACION: folder-name (string) X create-date (string) X mod-date (string) X
; location (string) X creator (list) X size (int) X cant-items (int) X
; security (list) X password (string) X type (string)

#| .............[ -> CONSTRUCTOR ].............

FUNCION CONSTRUCTORA DE LA CARPETA

DESC: Función constructora del tda folder. Contiene el nombre de la carpeta,
la fecha de creación, la fecha de modificación, la ubicación, el creador, el
tamaño, la cantidad de items, el nivel de seguridad, la contraseña y el tipo de carpeta.

DOMINIO:
- folder-name (string)
- create-date (date)
- mod-date (date)
- location (string)
- creator (user)
- size (int)
- cant-items (int)
- security (list:char)
- password (string)
- type (string)

RECORRIDO: folder (list)

|#

(define make-folder (lambda (folder-name ;0
                             create-date ;1
                             mod-date ;2
                             location ;3
                             creator ;4
                             size ;5
                             cant-items ;6
                             security ;7
                             password ;8
                             type) ;9
                           (list folder-name
                             create-date
                             mod-date
                             location
                             creator
                             size
                             cant-items
                             security
                             password
                             type)))




(define folder (lambda (name)
                        (make-folder name ;0
                                     "" ;date 1
                                     "" ;date 2
                                     "" ;3
                                     "" ;4
                                     0 ;5
                                     0 ;6
                                     null ;7
                                     "" ;8
                                     "" )));9

(define set-location (lambda (folder-arg location)
                       (if (equal? (folder-type folder-arg) "Folder"*)
                           (make-folder (get-folder-name folder-arg)
                                        (get-create-date folder-arg)
                                        (get-create-date folder-arg)
                                        location
                                        (get-folder-creator folder-arg)
                                        (get-folder-size folder-arg)
                                        (get-items folder-arg)
                                        (get-folder-security folder-arg)
                                        (get-folder-pass folder-arg)
                                        (folder-type folder-arg))
                           ;si es File*
                           (make-folder (get-folder-name folder-arg)
                                        (get-create-date folder-arg)
                                        (get-create-date folder-arg)
                                        (string-append location (get-folder-name folder-arg))
                                        (get-folder-creator folder-arg)
                                        (get-folder-size folder-arg)
                                        (get-items folder-arg)
                                        (get-folder-security folder-arg)
                                        (get-folder-pass folder-arg)
                                        (folder-type folder-arg)))))
  
  
  

#| .............[ -> SELECTORES ]............. |#

;_____________________________________

#| FUNCION SELECTORA DE NOMBRE DE CARPETA

DESC: Función que obtiene el nombre de la carpeta

DOMINIO: folder

RECORRIDO: folder-name(string)

|#
(define (get-folder-name folder) (car folder))

;_____________________________________

#| FUNCION SELECTORA DE NOMBRE

DESC: Función que obtiene el nombre de la carpeta

DOMINIO: folder

RECORRIDO: name(string)

|#
(define (get-name folder) (car folder))

;_____________________________________

#| FUNCION SELECTORA DE FECHA DE CREACIÓN

DESC: Función que obtiene la fecha de creación de la carpeta

DOMINIO: folder

RECORRIDO: create-date(date)

|#
(define (get-create-date folder) (list-ref folder 1))

;_____________________________________

#| FUNCION SELECTORA DE FECHA DE MODIFICACIÓN

DESC: Función que obtiene la fecha de modificación de la carpeta

DOMINIO: folder

RECORRIDO: mod-date(date)

|#
(define (get-mod-date folder) (list-ref folder 2))

;_____________________________________

#| FUNCION SELECTORA DE UBICACIÓN DE CARPETA

DESC: Función que obtiene la ubicación de la carpeta

DOMINIO: folder

RECORRIDO: folder-location(string)

|#
(define (get-folder-location folder) (list-ref folder 3))

;_____________________________________

#| FUNCION SELECTORA DE CREADOR DE CARPETA

DESC: Función que obtiene el creador de la carpeta

DOMINIO: folder

RECORRIDO: folder-creator(string)

|#
(define (get-folder-creator folder) (list-ref folder 4))

;_____________________________________

#| FUNCION SELECTORA DE TAMAÑO DE CARPETA

DESC: Función que obtiene el tamaño de la carpeta

DOMINIO: folder

RECORRIDO: folder-size(int)

|#
(define (get-folder-size folder) (list-ref folder 5))

;_____________________________________

#| FUNCION SELECTORA DE ITEMS

DESC: Función que obtiene los items de la carpeta

DOMINIO: folder

RECORRIDO: items(list)

|#
(define (get-items folder) (list-ref folder 6))

;_____________________________________

#| FUNCION SELECTORA DE SEGURIDAD DE CARPETA

DESC: Función que obtiene la seguridad de la carpeta

DOMINIO: folder

RECORRIDO: folder-security(list)

|#
(define (get-folder-security folder) (list-ref folder 7))

;_____________________________________

#| FUNCION SELECTORA DE CONTRASEÑA DE CARPETA

DESC: Función que obtiene la contraseña de la carpeta

DOMINIO: folder

RECORRIDO: folder-pass(string)

|#
(define (get-folder-pass folder) (list-ref folder 8))

;_____________________________________

#| FUNCION SELECTORA DE TIPO DE ITEM

DESC: Función que obtiene el tipo item

DOMINIO: folder

RECORRIDO: folder-type(string)

|#
(define (folder-type folder) (list-ref folder 9))
