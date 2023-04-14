#lang racket

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


;EJEMPLOS

(define folder-ejemplo (folder "Folder 0"))

(define arbol1 folder-ejemplo)

(define arbol2 (insertar-folder arbol1 "Folder 1"))
(define arbol3 (insertar-folder arbol2 "Folder 2"))
(define arbol4 (insertar-folder arbol3 "Folder 2"))
(define arbol5 (insertar-folder arbol4 "Folder 3"))

(define arbol6 (insertar-folder-en-hijos arbol5 "Folder 1" "Subfolder 1"))
(define arbol7 (insertar-folder-en-hijos arbol6 "Folder 1" "Subfolder 2"))
(define arbol8 (insertar-folder-en-hijos arbol7 "Folder 1" "Subfolder 2"))

#|
Aquí hay una descripción, dominio y recorrido para cada función creada
en el código:

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

