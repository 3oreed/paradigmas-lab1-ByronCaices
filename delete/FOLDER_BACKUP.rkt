#lang racket

(require "NEW_DRIVE.rkt")
(require racket/date)
(provide (all-defined-out))

; FUNCION FECHA
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

;CONSTRUCTOR

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

;SELECTORES

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

;MODIFICADORES

(define (buscar-folder-hijo folder name)
  (cond
    [(null? folder) #f]
    [(equal? (folder-name folder) name) folder]
    [else (buscar-folder-hijo-aux (folder-content-hijos folder) name)]))

(define (buscar-folder-hijo-aux content-hijos name)
  (cond
    [(null? content-hijos) #f]
    [(buscar-folder-hijo (car content-hijos) name) (car content-hijos)]
    [else (buscar-folder-hijo-aux (cdr content-hijos) name)]))

(define (insertar-folder folder name)
  (cond
    [(null? folder) (make-folder name '() '() "" "" '() '() '() "" '())]
    [(buscar-folder-hijo folder name) folder] ; No agregar hijo repetido
    [else (make-folder (folder-name folder)
                       (get-create-date folder)
                       (get-mod-date folder)
                       (get-folder-location folder)
                       (get-folder-creator folder)
                       (get-folder-size folder)
                       (get-items folder)
                       (get-folder-security folder)
                       (get-folder-pass folder)
                       (cons (make-folder name (crnt-date-folder) (crnt-date-folder) "" "" '() '() '() "" '()) (folder-content-hijos folder)))]))


(define (insertar-folder-en-hijos padre . nombres-carpetas)
  (define (actualizar-hijos padre carpeta-buscada nueva-carpeta)
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
                        (if (equal? (folder-name hijo) carpeta-buscada)
                            nueva-carpeta
                            hijo))
                      (folder-content-hijos padre))))
  (if (null? nombres-carpetas)
      padre
      (let* ([carpeta-buscada (car nombres-carpetas)]
             [nodo-encontrado (buscar-folder-hijo padre carpeta-buscada)])
        (if nodo-encontrado
            (actualizar-hijos padre carpeta-buscada
                             (apply insertar-folder-en-hijos nodo-encontrado (cdr nombres-carpetas)))
            (let ([nueva-carpeta (make-folder carpeta-buscada
                                              '() ; create-date
                                              '() ; mod-date
                                              "" ; location
                                              "" ; creator
                                              '() ; size
                                              '() ; items
                                              '() ; security
                                              "" ; password
                                              '())]) ; content-hijos
              (if (null? (cdr nombres-carpetas))
                  (insertar-folder padre carpeta-buscada)
                  (insertar-folder-en-hijos
                   (actualizar-hijos padre carpeta-buscada nueva-carpeta) (cdr nombres-carpetas))))))))


#|
(define (insertar-folder-en-hijos padre name-hijo name-nuevo)
  (let ([folder-hijo (buscar-folder-hijo padre name-hijo)])
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
                            (if (equal? (folder-name hijo) name-hijo)
                                (insertar-folder hijo name-nuevo)
                                hijo))
                          (folder-content-hijos padre)))
        padre)))

|#
;EJEMPLOS

;(define f0 (folder "Folder 0"))

(define drive0 (drive #\C "drive1" 1000))

(define f0 drive0)

(define f1 (insertar-folder-en-hijos f0 "Folder 1"))


(define f2 (insertar-folder-en-hijos f1 "Folder 1"))
(define f3 (insertar-folder-en-hijos f2 "Folder 2"))
(define f4 (insertar-folder-en-hijos f3 "Folder 2"))
(define f5 (insertar-folder-en-hijos f4 "Folder 3"))

(define f6 (insertar-folder-en-hijos f5 "Folder 1" "Subfolder 1"))
(define f7 (insertar-folder-en-hijos f6 "Folder 1" "Subfolder 2"))
(define f8 (insertar-folder-en-hijos f7 "Folder 1" "Subfolder 2"))
(define f9 (insertar-folder-en-hijos f8 "Folder 1" "Subfolder 2" "Subsubfolder1"))

;(define drive1 (drive #\C "drive1" 1000))

;(define ff1 (insertar-folder drive1 "Folder 1!"))

#|

make-folder

Descripción: Crea un nuevo nodo de tipo folder con los atributos dados.
Dominio: Los argumentos son las propiedades del nodo-folder
 (folder-name, create-date, mod-date, location, creator, size, items,
 security, password, content-hijos).
Recorrido: Un nodo-folder representado como una lista.

folder-name

Descripción: Obtiene el nombre de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: El nombre del folder (string).

folder-content-hijos

Descripción: Obtiene la lista de hijos de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: Una lista de nodos-folder hijos.

buscar-folder-hijo

Descripción: Busca un nodo-folder hijo con el valor especificado en un árbol
 de nodos-folder.
Dominio: Un nodo-folder (raíz del árbol) y un valor (string) para buscar.
Recorrido: El nodo-folder encontrado o #f si no se encuentra.

buscar-folder-hijo-aux

Descripción: Función auxiliar que busca un nodo-folder hijo con el valor
especificado en una lista de nodos-folder hijos.
Dominio: Una lista de nodos-folder hijos y un valor (string) para buscar.
Recorrido: El nodo-folder encontrado o #f si no se encuentra.

insertar-folder

Descripción: Inserta un nuevo nodo-folder como hijo de otro nodo-folder si
 no existe un hijo con el mismo valor.
Dominio: Un nodo-folder y un valor (string) para el nuevo hijo.
Recorrido: El nodo-folder con el nuevo hijo agregado, o el nodo-folder original
 si ya existe un hijo con el mismo valor.

insertar-folder-en-hijos

Descripción: Inserta un nuevo nodo-folder como hijo de un nodo-folder específico
 que es hijo del nodo-folder padre dado.
Dominio: Un nodo-folder padre, un valor-hijo (string) que identifica al
 nodo-folder hijo donde se agregará el nuevo nodo, y un valor-nuevo (string)
para el nuevo hijo.
Recorrido: El nodo-folder padre con el nuevo hijo agregado al nodo-folder
 hijo especificado, o el nodo-folder padre original si no se encuentra el
 nodo-folder hijo especificado.

Además, aquí están las descripciones, dominios y recorridos para los getters que proporcionaste:

get-create-date

Descripción: Obtiene la fecha de creación de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: La fecha de creación del folder.

get-mod-date

Descripción: Obtiene la fecha de modificación de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: La fecha de modificación del folder.

get-folder-location

Descripción: Obtiene la ubicación de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: La ubicación del folder (string).

get-folder-creator

Descripción: Obtiene el creador de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: El creador del folder (string).

get-folder-size

Descripción: Obtiene el tamaño de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: El tamaño del folder.

get-items

Descripción: Obtiene la cantidad de elementos en un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: La cantidad de elementos en el folder.

get-folder-security

Descripción: Obtiene el nivel de seguridad de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: El nivel de seguridad del folder.

get-folder-pass

Descripción: Obtiene la contraseña de un nodo-folder.
Dominio: Un nodo-folder representado como una lista.
Recorrido: La contraseña del folder (string).
|#

