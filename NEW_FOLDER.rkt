#lang racket

(require racket/date)
(provide (all-defined-out))

(define (crnt-date-folder)
          (define fecha (current-date))
                          (string-append
                             (number->string (date-hour fecha))
                             ":"
                             (number->string (date-minute fecha))
                             " - "
                             (number->string (date-day fecha))
                             "/"
                             (number->string (date-month fecha))
                             "/"
                             (number->string (date-year fecha))))

(define make-folder (lambda (folder-name ;0
                             create-date
                             mod-date
                             location
                             creator
                             size
                             items
                             security
                             password
                             content-hijos)
                           (list folder-name
                             create-date
                             mod-date
                             location
                             creator
                             size
                             items
                             security
                             password
                             content-hijos)))

(define folder (lambda (name)
                        (make-folder name
                                     (crnt-date-folder)
                                     (crnt-date-folder)
                                     ""
                                     ""
                                     null
                                     null
                                     null
                                     ""
                                     null)))

(define make-folder (lambda (folder-name ;0
                             create-date
                             mod-date
                             location
                             creator
                             size
                             items
                             security
                             password
                             content-hijos)
                           (list folder-name
                             create-date
                             mod-date
                             location
                             creator
                             size
                             items
                             security
                             password
                             content-hijos)))

(define (folder-name folder)
  (car folder))

(define (get-create-date folder)
  (list-ref folder 1))

(define (get-mod-date folder)
  (list-ref folder 2))

(define (get-folder-location folder)
  (list-ref folder 3))

(define (get-folder-creator folder)
  (list-ref folder 4))

(define (get-folder-size folder)
  (list-ref folder 5))

(define (get-items folder)
  (list-ref folder 6))

(define (get-folder-security folder)
  (list-ref folder 7))

(define (get-folder-pass folder)
  (list-ref folder 8))

(define (folder-content-hijos folder)
  (list-ref folder 9))

(define (buscar-folder-hijo folder valor)
  (cond
    [(null? folder) #f]
    [(equal? (folder-name folder) valor) folder]
    [else (buscar-folder-hijo-aux (folder-content-hijos folder) valor)]))

(define (buscar-folder-hijo-aux content-hijos valor)
  (cond
    [(null? content-hijos) #f]
    [(buscar-folder-hijo (car content-hijos) valor) (car content-hijos)]
    [else (buscar-folder-hijo-aux (cdr content-hijos) valor)]))

(define (insertar-folder folder valor)
  (cond
    [(null? folder) (make-folder valor '() '() "" "" '() '() '() "" '())]
    [(buscar-folder-hijo folder valor) folder] ; No agregar hijo repetido
    [else (make-folder (folder-name folder)
                       (get-create-date folder)
                       (get-mod-date folder)
                       (get-folder-location folder)
                       (get-folder-creator folder)
                       (get-folder-size folder)
                       (get-items folder)
                       (get-folder-security folder)
                       (get-folder-pass folder)
                       (cons (make-folder valor '() '() "" "" '() '() '() "" '()) (folder-content-hijos folder)))]))

(define (insertar-folder-en-hijos padre valor-hijo valor-nuevo)
  (let ([folder-hijo (buscar-folder-hijo padre valor-hijo)])
    (if folder-hijo
        (make-folder (folder-name padre)
                     (get-create-date padre)
                     (get-mod-date padre)
                     (get-folder-location padre)
                     (get-folder-creator padre)
                     (get-folder-size padre)
                     (get-items padre)
                     (get-folder-security padre)
                     (get-folder-pass padre)
                     (map (lambda (hijo)
                            (if (equal? (folder-name hijo) valor-hijo)
                                (insertar-folder hijo valor-nuevo)
                                hijo))
                          (folder-content-hijos padre)))
        padre)))
(define folder-ejemplo (folder "Folder 0"))

(define arbol1 folder-ejemplo)

(define arbol2 (insertar-folder arbol1 "Folder 1")) ;inserta en "C:"
(define arbol3 (insertar-folder arbol2 "Folder 2")) ;inserta en "C:"
(define arbol4 (insertar-folder arbol3 "Folder 3"))

(define arbol5 (insertar-folder-en-hijos arbol4 "Folder 1" "Subfolder 1" "Subsubfolder 1"))
(define arbol6 (insertar-folder-en-hijos arbol5 "Folder 1" "Subfolder 2"))

(define arbol7 (insertar-folder-en-hijos arbol6 "Folder 2" "Subfolder 3"))

;(displayln arbol7)