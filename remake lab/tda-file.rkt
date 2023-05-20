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
;(define hide?)
;(define read-only?)

(define file1 (file "goo4.docx" "docx" "hello world 4" #\h #\r))
(define file2 (file "goo4.docx" "docx" "hello world 4"))

(define hide? (lambda (file-arg)
                (if (not(member #\h (get-file-security file-arg)))
                    #f
                    #t)))

(define read-only? (lambda (file-arg)
                (if (not(member #\r (get-file-security file-arg)))
                    #f
                    #t)))

(define (get-file-name file-arg)
  (list-ref file-arg 0))

(define (get-extension file-arg)
  (list-ref file-arg 1))

(define (get-text file-arg)
  (list-ref file-arg 2))

(define (get-file-security file-arg)
  (list-ref file-arg 7))

(define (get-file-password file-arg)
  (list-ref file-arg 4))

(define (get-create-date-file file-arg)
  (list-ref file-arg 5))

(define (get-mod-date-file file-arg)
  (list-ref file-arg 6))

(define (get-file-location file-arg)
  (list-ref file-arg 3))

(define (get-file-creator file-arg)
  (list-ref file-arg 8))

(define (get-file-type file-arg)
  (list-ref file-arg 9))



