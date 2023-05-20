#lang racket
(require "tda-drive.rkt")
(require "tda-user.rkt")
(require "tda-folder.rkt")
(require "tda-file.rkt")
(require racket/date)
(provide (all-defined-out))

;funcion que obtiene la fecha actual y retorna un string con la hora y fecha
(define (crnt-date)
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


;-----------------------------[ START OF: TDA system ]-----------------------------

; REPRESENTACION: system-name (string) X loged-user (string) X current-path (string) X
; users (list) X system-date (string) X drives (list) X trashcan (list) X paths (list)

#| .............[ -> CONSTRUCTOR ].............

FUNCION CONSTRUCTORA DEL SISTEMA

DESC: Función constructora del tda sistema. Contiene el nombre del sistema,
 el usuario conectado, el camino actual, la lista de usuarios, la fecha del sistema,
 los discos, la papelera y los caminos.

DOMINIO:
- system-name (string)
- loged-user (string)
- current-path (string)
- users (list:user)
- system-date (date)
- drives (list:drive) 
- trashcan (list:path) 
- paths (list:path) 

RECORRIDO: system (list)

|#

(define make-system (lambda
                        (system-name ;0
                         loged-user ;1
                         current-path ;2
                         users ;3
                         system-date ;4
                         drives;5
                         trashcan ;6
                         paths);7
                        (list system-name
                              loged-user
                              current-path ;string-downcase
                              users 
                              system-date
                              drives
                              trashcan
                              paths)))

(define system (lambda (name)
                 (make-system name
                              "" ;user
                              "" ;path
                              null ;users
                              (crnt-date) ;fecha
                              null;drives
                              null ;papelera
                              null) ;paths
                 ))

;_____________________________________


;                ...........................................




#|               .............[ -> SELECTORES ].............               |#

;_____________________________________

#| FUNCION SELECTORA DE NOMBRE DE SISTEMA

DESC: Función que obtiene el nombre del sistema

DOMINIO: system

RECORRIDO: system-name(string)

|#
(define (get-system-name system) (list-ref system 0))

;_____________________________________

#| FUNCION SELECTORA DE USUARIO CONECTADO

DESC: Función que obtiene el usuario conectado

DOMINIO: system

RECORRIDO: loged-user(string)

|#
(define (get-loged-user system) (list-ref system 1))

;_____________________________________

#| FUNCION SELECTORA DE RUTA ACTUAL

DESC: Función que obtiene la ruta actual del sistema

DOMINIO: system

RECORRIDO: current-path(string)

|#
(define (get-current-path system) (list-ref system 2))

;_____________________________________

#| FUNCION SELECTORA DE USUARIOS

DESC: Función que obtiene la lista de usuarios

DOMINIO: system

RECORRIDO: users(list)

|#
(define (get-users system) (list-ref system 3))

;_____________________________________

#| FUNCION SELECTORA DE FECHA DE SISTEMA

DESC: Función que obtiene la fecha del sistema

DOMINIO: system

RECORRIDO: system-date(date)

|#
(define (get-system-date system) (list-ref system 4))

;_____________________________________

#| FUNCION SELECTORA DE UNIDADES DE DISCO

DESC: Función que obtiene las unidades de disco del sistema

DOMINIO: system

RECORRIDO: drives(list)

|#
(define (get-drives system) (list-ref system 5))

;_____________________________________

#| FUNCION SELECTORA DE PAPELERA

DESC: Función que obtiene la papelera del sistema

DOMINIO: system

RECORRIDO: trashcan(list)

|#
(define (get-trashcan system) (list-ref system 6))

;_____________________________________

#| FUNCION SELECTORA DE RUTAS

DESC: Función que obtiene las rutas del sistema

DOMINIO: system

RECORRIDO: paths(list)

|#
(define (get-paths system) (list-ref system 7))

;_____________________________________

#| FUNCION SELECTORA DE UNIDAD DE DISCO ACTUAL

DESC: Función que obtiene la unidad de disco actual del sistema

DOMINIO: system

RECORRIDO: current-drive(drive)

|#
(define (get-current-drive system) (car (get-drives system)))
;_____________________________________

;                .............................................


;                 ---------------------------------------------
;                |                                             |
;                |      INICIO FUNCIONES ENUNCIADO LAB 1       |
;                |                                             |
;                 ---------------------------------------------



#| FUNCION RUN

DESC: Función que permite ejecutar un comando (función) sobre un sistema.

DOMINIO: System X Command  

RECORRIDO: System  
 
|#
(define run (lambda (system cmd) (cmd system)))
;_____________________________________


;funcion que dada una letra verifica si existe un drive en drives de un system
(define existing-letter? (lambda (letter drives)
                        (cond
                          [(null? drives) #f]
                          [(equal? letter (get-letter(car drives))) #t]
                          [else
                           (existing-letter? letter (cdr drives))])))



#| FUNCION ADD-DRIVE

DESC: Función que permite añadir una unidad a un sistema. La letra de la unidad es única.

DOMINIO: System X Letter (Char) X Name (String) X Cap (num)  

RECORRIDO: System

RECURSIVIDAD: Utiliza recursividad natural en existing-letter
 
|#                          
(define add-drive (lambda (system-arg)
                    (lambda (letter name cap)
                      (if (existing-letter? letter (get-drives system-arg))
                          system-arg
                          (make-system (get-system-name  system-arg) 
                                       (get-loged-user  system-arg) 
                                       (string-append (string letter) ":/")
                                       (get-users  system-arg)
                                       (get-system-date  system-arg) 
                                       (cons (drive letter name cap)
                                             (get-drives  system-arg))
                                       (get-trashcan  system-arg)
                                       (cons (string-append (string letter) ":/")
                                             (get-paths system-arg)))))))
;_____________________________________

;funcion que verifica si un user existe o no
(define existing-user? (lambda (user-name users)
                        (cond
                          [(null? users) #f]
                          [(equal? user-name (car users)) #t]
                          [else
                           (existing-user? user-name (cdr users))])))

#| FUNCION REGISTER

DESC: Función que permite registrar un nuevo usuario al sistema.
El nombre de usuario es único y no puede ser duplicado.

DOMINIO: System X userName(string)  

RECORRIDO: System

RECURSIVIDAD: Utiliza recursividad natural en existing-user
 
|#
(define register (lambda (system-arg)
                    (lambda (user-name)
                      (if (existing-user? user-name (get-users system-arg))
                          system-arg
                          (make-system (get-system-name  system-arg) 
                                       (get-loged-user  system-arg) 
                                       (get-current-path system-arg)
                                       (cons (user user-name)
                                        (get-users  system-arg))
                                       (get-system-date  system-arg) 
                                       (get-drives  system-arg)
                                       (get-trashcan  system-arg)
                                       (get-paths system-arg))))))
;_____________________________________


;Funcion que verifica si un string es vacio o nulo
(define my-string-null? (lambda (str)
                          (equal? str "")))

#| FUNCION LOGIN

DESC: Función que permite iniciar sesión con un usuario del sistema,
 solo si éste existe.

DOMINIO: System X userName(string)  

RECORRIDO: System

RECURSIVIDAD: Utiliza recursividad natural en existing-user
 
|#
(define login (lambda (system-arg)
                    (lambda (user-name)
                      (if (and (existing-user? user-name (get-users system-arg))
                               (my-string-null? (get-loged-user system-arg)))
                          
                          (make-system (get-system-name  system-arg) 
                                       (user user-name)
                                       (get-current-path system-arg)
                                       (get-users  system-arg)
                                       (get-system-date  system-arg) 
                                       (get-drives  system-arg)
                                       (get-trashcan  system-arg)
                                       (get-paths system-arg))
                          system-arg))))
;_____________________________________

#| FUNCION LOGOUT

DESC: Función que permite cerrar la sesión de un usuario en el sistema.

DOMINIO: System 

RECORRIDO: System' 
 
|#
(define logout (lambda (system-arg)
                 (if (null? (get-loged-user system-arg))
                     system-arg
                     (make-system (get-system-name  system-arg) 
                                  ""
                                  (get-current-path system-arg)
                                  (get-users  system-arg)
                                  (get-system-date  system-arg) 
                                  (get-drives  system-arg)
                                  (get-trashcan  system-arg)
                                  (get-paths system-arg)))))
;_____________________________________

;Funcion que mueve un elemento de una lista a su cabeza
;en este caso dada una letra e de un drive lo mueve a la cabeza
(define move-to-head (lambda (e lista)
                       (cons (first (filter
                                     (lambda (x) (char=? e (first x)))
                                     lista))
                             (remove (first (filter
                                             (lambda (x) (char=? e (first x)))
                                             lista))
                                     lista))))


#| FUNCION SWITCH-DRIVE

DESC: Funcion que fija la unidad en la que el usuario realizará acciones.
      actualiza el current path del system

DOMINIO: System X Letter (char)

RECORRIDO: System
 
|#
(define switch-drive (lambda (system-arg)
                       (lambda (letter)
                         (if (and (not(my-string-null? (get-loged-user system-arg)))
                                  (existing-letter? letter (get-drives system-arg)))
                             (make-system (get-system-name  system-arg) 
                                          (get-loged-user system-arg)
                                          (string-append (string letter) ":/")
                                          (get-users  system-arg)
                                          (get-system-date  system-arg) 
                                          (move-to-head letter (get-drives system-arg))
                                          (get-trashcan  system-arg)
                                          (get-paths system-arg))
                             system-arg))))
;_____________________________________

; constructor de un folder dado un system entrega atributos como
; usuario creador, hora de creacion, etc.
(define sys-make-folder (lambda (folder-name system-arg)
                          (make-folder folder-name
                                       (get-system-date system-arg)
                                       (get-system-date system-arg)
                                       (string-append (get-current-path system-arg)
                                                      (string-append folder-name "/"))
                                       (get-loged-user system-arg)
                                       0 ;folder-size
                                       0  ;cant-items
                                       null ; folder-security
                                       "" ;pass
                                       "Folder*"))) ;type
                                       

;funcion que verifica si existe un folder-new en la ruta actual del system
(define existing-folder? (lambda (folder-name system-arg)
                           (let ([new-path (string-append (get-current-path system-arg) folder-name "/")])
                             (if (member new-path (get-paths system-arg))
                                 #t
                                 #f))))

; dado el current-path de system y un folder-name,
; se toma el current-drive y se le agrega a su contenido
; si y solo si nuevo folder no existe
; retorna un drive
(define add-folder-to-drive (lambda (folder-name system-arg)
                              (let ([current-drive (get-current-drive system-arg)])
                                (if (existing-folder? folder-name system-arg)
                                    current-drive
                                    (make-drive (get-letter current-drive)
                                                (get-drive-name current-drive)
                                                (get-drive-cap current-drive)
                                                (cons (sys-make-folder folder-name system-arg)
                                                      (get-drive-content current-drive)))))))

                                    
#| FUNCION MD

DESC:  función que permite crear directorio dentro de una unidad a
 partir del nombre especificado. Internamente la función añade datos
 relativos a usuario creador, fecha de creación, fecha de última
 modificación, atributos de seguridad, etc.

DOMINIO: System X FolderName (string)

RECORRIDO: System
 
|#
(define md (lambda (system-arg)
             (lambda (folder-name)
               (if (existing-folder? folder-name system-arg)
                   system-arg
                   (make-system (get-system-name  system-arg) 
                                (get-loged-user system-arg)
                                (get-current-path system-arg)
                                (get-users  system-arg)
                                (get-system-date  system-arg) 
                                (cons (add-folder-to-drive folder-name system-arg)
                                      (cdr(get-drives system-arg)));;
                                (get-trashcan  system-arg)
                                (cons (string-append (get-current-path system-arg) folder-name "/")
                                      (get-paths system-arg)))))))

;_____________________________________

;funcion que determina el tipo de operacion que se ejecutará al cambiar de ruta en system
(define cd-type (lambda (arg)
                  (cond
                    [(equal? arg "/") "root"]
                    [(equal? arg "..") "back"]
                    [(equal? (substring arg 1 3) ":/") "path"]
                    [else "folder"])))

;funcion que actualiza el current path del sistema dado el tipo de cambio que se quiera realizar
(define change-path (lambda (arg system-arg)
                      (let ([current-path (get-current-path system-arg)])
                        (cond
                          [(equal? (cd-type arg) "root")
                           (substring current-path 0 3)]
                          [(equal? (cd-type arg) "back")
                           (string-append (string-join(reverse(cdr(reverse(string-split current-path "/"))))"/") "/")]
                          [(equal? (cd-type arg) "path")
                           arg]
                          [(existing-folder? arg system-arg)
                           (string-append current-path arg "/")]
                          [else
                           current-path]))))

#| FUNCION CD

DESC:  función que permite cambiar la ruta (path) donde se realizarán operaciones.
 cd permite cambiarse a un directorio especificado a partir de la ruta señalada en
 un String.

DOMINIO: System X Path or FolderName (string)

RECORRIDO: System
 
|#
(define cd (lambda (system-arg)
             (lambda (arg)

                 (if (and(existing-letter? (string-ref arg 0) (get-drives system-arg))
                         (equal? (cd-type (get-current-path system-arg)) "path"))
                     ;si es un type path entonces debo hacer un switch drive
                     (make-system (get-system-name  system-arg) 
                                  (get-loged-user system-arg)
                                  (change-path arg system-arg)
                                  (get-users  system-arg)
                                  (get-system-date  system-arg)
                                  ;switch-drive
                                  (move-to-head (string-ref arg 0) (get-drives system-arg))
                                  (get-trashcan  system-arg)
                                  (get-paths system-arg))

                     (make-system (get-system-name  system-arg) 
                                  (get-loged-user system-arg)
                                  (change-path arg system-arg)
                                  (get-users  system-arg)
                                  (get-system-date  system-arg) 
                                  (get-drives system-arg)
                                  (get-trashcan  system-arg)
                                  (get-paths system-arg))))))
;_____________________________________

; constructor de un file dado un system entrega atributos como
; usuario creador, hora de creacion, etc.
(define sys-make-file (lambda (system-arg file-name extension text . security)
                        (make-file file-name ;0
                                   extension
                                   text
                                   (string-append (get-current-path system-arg) file-name)
                                   ""
                                   (get-system-date system-arg)
                                   (get-system-date system-arg)
                                   security
                                   (get-loged-user system-arg)
                                   
                                   ;(get-current-path system-arg)
                                   "File*");9
                        ))

;funcion que verifica si existe un file en una determinada ruta
(define existing-file? (lambda (file-name system-arg)
                           (let ([new-path (string-append (get-current-path system-arg) file-name)])
                             (if (member new-path (get-paths system-arg))
                                 #t
                                 #f))))

;funcion que verifica si existe un path en las rutas del sistema
(define existing-path? (lambda (new-path system-arg)
                         (if (member new-path (get-paths system-arg))
                             #t
                             #f)))

; dado el current-path de system y un file-name,
; se toma el current-drive y se le agrega a su contenido
; si y solo si nuevo file no existe
; retorna un drive
(define add-file-to-drive (lambda (file-name extension text security system-arg)
                              (let ([current-drive (get-current-drive system-arg)])
                                (if (existing-file? file-name system-arg)
                                    current-drive
                                    (make-drive (get-letter current-drive)
                                                (get-drive-name current-drive)
                                                (get-drive-cap current-drive)
                                                (cons (sys-make-file system-arg file-name extension text security)
                                                      (get-drive-content current-drive)))))))

#| FUNCION ADD-FILE

DESC:  función que permite añadir un archivo en la ruta actual.

DOMINIO: System X FileName (string)

RECORRIDO: System
 
|#
(define add-file (lambda (system-arg)
                   (lambda (file-arg)
                     (let* ([file-name (get-file-name file-arg)]
                            [extension (get-extension file-arg)]
                            [text (get-text file-arg)]
                            [security (get-file-security file-arg)])
                     (if (existing-file? file-name system-arg)
                         system-arg
                         (make-system (get-system-name  system-arg) 
                                      (get-loged-user system-arg)
                                      (get-current-path system-arg)
                                      (get-users  system-arg)
                                      (get-system-date  system-arg) 
                                      (cons (add-file-to-drive file-name extension text security system-arg)
                                            (cdr(get-drives system-arg)));;
                                      (get-trashcan  system-arg)
                                      (cons (string-append (get-current-path system-arg) file-name)
                                            (get-paths system-arg))))))))

;_____________________________________

;En mi system añadir a la papelera o eliminar será:
;mover la ruta desde paths a trashcan, de esta manera
;el file queda inaccesible e inmutable


;dado el nombre de un file que se encuentra en ruta actual, elimina su ruta
(define del-file-from-paths (lambda (file-name system-arg)
                              (let* ([file-path (string-append (get-current-path system-arg) file-name)]
                                     [paths (get-paths system-arg)])
                                (if (existing-file? file-name system-arg)
                                    (remove file-path paths)
                                    paths))))

;dado el nombre de un file que se encuentra en ruta actual, busca el file y lo retorna
(define get-file-from-drive (lambda (file-name drive-content cola)
                              (cond
                                [(not(null? cola)) cola]
                                [(equal? (get-name (car drive-content)) file-name)
                                 (get-file-from-drive file-name
                                                      (cdr drive-content)
                                                      (cons (car drive-content)
                                                            cola))]
                                [else
                                 (get-file-from-drive file-name (cdr drive-content) cola)])))

                                 
#| FUNCION DEL

DESC:  función que permite eliminar un archivo de la ruta actual.

DOMINIO: System X FileName (string)

RECORRIDO: System

RECURSIVIDAD: Utiliza recursividad de cola

|#                               
; Muevela ruta del archivo a la papelera, de esta manera el archivo
; sigue siendo almacenado en el drive pero se vuelve inaccesible e ineditable
(define del (lambda (system-arg)
              (lambda (name)
                (make-system (get-system-name  system-arg) 
                             (get-loged-user system-arg)
                             (get-current-path system-arg)
                             (get-users  system-arg)
                             (get-system-date system-arg)
                             (get-drives system-arg)
                             (cons (get-file-location (car(get-file-from-drive name
                                                                           (get-drive-content(get-current-drive system-arg))
                                                                           '())))
                                   (get-trashcan  system-arg))
                             (del-file-from-paths name system-arg)))))
;_____________________________________

;dado el nombre de un folder que se encuentra en ruta actual, elimina su ruta
(define del-folderpath (lambda (folder-name system-arg)
                         (let* ([current-path (get-current-path system-arg)]
                                [path-to-del (string-append current-path folder-name "/")])
                           (filter (lambda (x)
                                     (not(string-contains? x path-to-del )))
                                   (get-paths system-arg)))))

;dado el nombre de un folder que se encuentra en ruta actual, busca el folder y lo retorna
(define get-folderpath (lambda (folder-name system-arg)
                         (let* ([current-path (get-current-path system-arg)]
                                [path-to-del (string-append current-path folder-name "/")])
                           (filter (lambda (x)
                                     (string-contains? x path-to-del ))
                                   (get-paths system-arg)))))

#| FUNCION RD

DESC:  función que permite eliminar un folder de la ruta actual, si está vacio

DOMINIO: System X FolderName (string)

RECORRIDO: System

|#   
(define rd (lambda (system-arg)
             (lambda (folder-name)
               (let ([folders-asociados (get-folderpath folder-name system-arg)])
               (make-system (get-system-name  system-arg) 
                            (get-loged-user system-arg)
                            (get-current-path system-arg)
                            (get-users  system-arg)
                            (get-system-date  system-arg) 
                            (get-drives system-arg)
                            (if (> (length folders-asociados) 1)
                                ;si tiene a alguien asociado:
                                (get-trashcan  system-arg)
                                (cons (car folders-asociados)
                                      (get-trashcan  system-arg)))
                             
                            (if (> (length folders-asociados) 1)
                                ;si tiene a alguien asociado:
                                (get-paths system-arg)
                                (del-folderpath folder-name system-arg)
                                ))))))
;_____________________________________

;COPY

;funcion que formatea un path al formato utilizado por el system                     
(define format-path (lambda (path)
                      (string-append
                       (string-upcase (substring path 0 3))
                       (substring path 3))))

;funcion que formatea un foldername o filename al formato utilizado por el system 
(define format-name (lambda (name)
                      (if (string-contains? name ".")
                          name
                          (string-append name "/"))))

;segun un path dado busca todos los items pertenecientes a ese path
(define search-item-by-path (lambda (new-path drive-content)
                         (filter
                          (lambda (x) (string-contains? (get-folder-location x) new-path))
                          drive-content)))

(define make-list2 (lambda (e n cola)
                    (if (> n 0)
                        (make-list2 e (- n 1)(cons e cola))
                        cola)))

;funcion que crea un lista con n veces el elemento e
(define make-list (lambda (e n)
                    (make-list2 e n '())))

(define str-len (lambda (str)
                  (length(string->list str))))


;copia todos los items de una folder y les cambia su location
;por la target location
(define copy-items (lambda (items-list new-path)
                     (map (lambda (item location)
                            (if (equal? (folder-type item) "Folder*")
                                (make-folder (get-folder-name item)
                                             (get-create-date item)
                                             (get-create-date item)
                                             (string-append location (format-name(get-folder-name item)))
                                             ;(string-append (substring location 0 3) (substring (get-folder-location item)3))
                                             (get-folder-creator item)
                                             (get-folder-size item)
                                             (get-items item)
                                             (get-folder-security item)
                                             (get-folder-pass item)
                                             (folder-type item))
                                ;si es File*
                                (make-folder (get-folder-name item)
                                             (get-create-date item)
                                             (get-create-date item)
                                             ;(string-append location (get-folder-name item))
                                             (if (> (length items-list)1)
                                                 (string-append location (cadr(reverse(string-split (get-folder-location item) "/"))) "/" (get-folder-name item));"**a"
                                                 (string-append location (format-name(get-folder-name item))))
                                             (get-folder-creator item)
                                             (get-folder-size item)
                                             (get-items item)
                                             (get-folder-security item)
                                             (get-folder-pass item)
                                             (folder-type item))))
                          items-list
                          (make-list new-path (length items-list)))))

;adquiere los nuevos paths que serán agregados al system tras copiar algo
(define path-from-copy (lambda (items-list)
                         (map get-folder-location items-list)))

(define add-items-to-drive (lambda (drive-arg items)
                             (make-drive (get-letter drive-arg)
                                         (get-drive-name drive-arg)
                                         (get-drive-cap drive-arg)
                                         (append items
                                                 (get-drive-content drive-arg)))))


(define copy (lambda (system-arg)
               (lambda (item-name target-path)
                 (let* ([aux-path (string-append (format-path target-path) (format-name item-name))]
                        [start-path (string-append (get-current-path system-arg) (format-name item-name))]
                        [new-drives (move-to-head (string-ref aux-path 0) (get-drives system-arg))]
                        [target-drive-content (get-drive-content(car(move-to-head (string-ref aux-path 0) (get-drives system-arg))))])
                   (if (existing-path? aux-path system-arg)
                       system-arg
                       (make-system (get-system-name  system-arg) 
                                    (get-loged-user system-arg)
                                    (get-current-path system-arg)
                                    (get-users  system-arg)
                                    (get-system-date  system-arg) 
                                    (move-to-head (string-ref start-path 0)(cons (add-items-to-drive (car new-drives)
                                                                                                   ;items
                                                                                                   (copy-items (search-item-by-path start-path (get-drive-content(get-current-drive system-arg)))
                                                                                                               (format-path target-path)))
                                                                               (cdr new-drives)))
                                    (get-trashcan  system-arg)
                                    (remove-duplicates(append (path-from-copy (copy-items (search-item-by-path start-path (get-drive-content(get-current-drive system-arg)))
                                                                                          (format-path target-path)))
                                                              (get-paths system-arg)))))))))


(define del-items-from-drive (lambda (drive-arg path)
                               (let* ([drive-content (get-drive-content drive-arg)])
                                 (make-drive (get-letter drive-arg)
                                             (get-drive-name drive-arg)
                                             (get-drive-cap drive-arg)
                                             (filter (lambda (x) (not(string-contains?
                                                                  (get-folder-location x)
                                                                  path)))
                                                       drive-content)))))

(define delete-moved-paths (lambda (paths-list path)
                             (filter (lambda (x)
                                       (not(string-contains? x path)))
                                     paths-list)))
                                                                  
                                                                  
                 
;item-name: folder3
;current-path: C:/
;origin-path: C:/folder3/
;target-path: D:/
;new-path: D:/folder3/
(define move (lambda (system-arg)
               (lambda (item-name target-path)
                 (let* ([new-sys ((copy system-arg)item-name target-path)]
                        [origin-path (string-append (get-current-path system-arg) (format-name item-name))]
                        [new-path (string-append (format-path target-path) (format-name item-name))])
                   ;new-sys
                   (make-system (get-system-name  new-sys) 
                                (get-loged-user new-sys)
                                (get-current-path new-sys)
                                (get-users  new-sys)
                                (get-system-date  new-sys) 
                                (cons (del-items-from-drive (get-current-drive new-sys)origin-path)
                                      (cdr(get-drives new-sys)))
                                (get-trashcan  new-sys)
                                (delete-moved-paths (get-paths new-sys) origin-path))))))
               


;retorna la drives-list actualizada
(define format-drive (lambda (letter new-name drives-list cola)
                       (cond
                         [(null? drives-list) (reverse cola)]
                         [(equal? letter (get-letter(car drives-list)))
                          (format-drive letter new-name (cdr drives-list)
                                        (cons
                                         (make-drive letter
                                                     new-name
                                                     (get-drive-cap (car drives-list))
                                                     '())
                                         cola))]
                         [else
                          (format-drive letter new-name (cdr drives-list)
                                        (cons (car drives-list)
                                              cola))])))
;funcion que elimina todos los paths asociados a la letra dada menos la raiz
;retorna paths sin la letra
(define format-paths (lambda (letter paths cola)
                       (cond
                         [(null? paths) (reverse cola)]
                         [(and (> (string-length (car paths)) 3)
                               (equal? letter (string-ref (car paths) 0)))
                          (format-paths letter (cdr paths) cola)]
                         [else
                          (format-paths letter (cdr paths)
                                        (cons (car paths)
                                              cola))])))

(define format (lambda (system-arg)
                 (lambda (letter new-name)
                   (make-system (get-system-name  system-arg) 
                                              (get-loged-user system-arg)
                                              (get-current-path system-arg)
                                              (get-users  system-arg)
                                              (get-system-date  system-arg) 
                                              (format-drive letter
                                                            new-name
                                                            (get-drives system-arg)
                                                            '())
                                              (get-trashcan  system-arg)
                                              ;(get-paths system-arg)
                                              (format-paths letter
                                                            (get-paths system-arg)
                                                            '())))))                  
#|
                      (make-folder (get-folder-name folder-arg)
                                   (get-create-date folder-arg)
                                   (crnt-date)
                                   new-path
                                   (get-folder-creator folder-arg)
                                   (get-folder-size folder-arg)
                                   (get-items folder-arg)
                                   (get-folder-security folder-arg)
                                   (get-folder-pass folder-arg)
                                   (folder-type folder-arg))))
                      
                      (make-file (get-file-name file-arg)
                                 (get-extension file-arg)
                                 (get-text file-arg)
                                 (get-file-creator file-arg)
                                 (get-file-password file-arg)
                                 (get-create-date-file file-arg)
                                 (get-mod-date-file file-arg);
                                 (get-file-security file-arg)
                                 (get-file-location file-arg)
                                 ()

|# 
                                 

;(search-folder-by-path "C:/folder1/" (get-drive-content(get-current-drive S15)))

(define STEST (make-system "newSystem"
                           "user2"
                           "C:/"
                           '("user2" "user1")
                           "16:24 - 11/5/2023"
                           '('(#\C "SO" 1000 ()) '(#\D "Util" 2000 ()))
                           '()
                           '("D:/" "C:/" )))                                        
                            

; EJEMPLOS

(define S0 (system "newSystem"))

;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))

;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))

;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user2"))

;cerrando sesión user1 e iniciando con user2
(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))

;cambios de unidad, incluyendo unidad inexistente K
(define S11 ((run S10 switch-drive) #\K))
(define S12 ((run S11 switch-drive) #\C))

;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))

(define S15 ((run S14 md) "folder2"))
(define S16 ((run S15 md) "folder3"))

;ingresa a carpeta folder2
(define S17 ((run S16 cd) "folder2"))

;crea subcarpeta folder21 dentro de folder2 (incluye caso S19 de carpeta con nombre duplicado)
(define S18 ((run S17 md) "folder21"))
(define S19 ((run S18 md) "folder21"))

;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente S221
(define S20 ((run S19 cd) "folder21"))
(define S21 ((run S20 cd) "folder22"))

;vuelve a carpeta anterior
(define S22 ((run S21 cd) ".."))

;vuelve a ingresar folder21
(define S23 ((run S22 cd) "folder21"))

;crea subcarpeta folder211 e ingresa
(define S24 ((run S23 md) "folder211"))
(define S25 ((run S24 cd) "folder211"))

;vuelve a la raíz de la unidad c:/
(define S26 ((run S24 cd) "/"))

;se cambia de unidad
(define S27 ((run S26 switch-drive) #\D))

;crea carpeta e ingresa a carpeta
(define S28 ((run S27 md) "folder5"))
(define S29 ((run S28 cd) "folder5"))

;se cambia de carpeta en base a la ruta especificada
(define S30 ((run S29 cd) "C:/folder1/"))

;formateando drive D:
(define S31 ((run S30 format) #\D "newD"))

;añadiendo archivos
(define S32 ((run S31 add-file) (file "foo1.txt" "txt" "hello world 1")))
(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2")))
(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3")))
(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r))) ;con atributos de seguridad oculto (h) y de solo lectura (r)

;eliminando archivos
;;(define S36 ((run S35 del) "*.txt"))
;;(define S37 ((run S35 del) "f*.docx"))
(define S38 ((run S35 del) "goo4.docx"))
(define S39 ((run S35 cd) ".."))
(define S40 ((run S39 del) "folder1"))

;borrando una carpeta
(define S41 ((run S39 rd) "folder1"))  ;no debería borrarla, pues tiene archivos
(define S42 ((run S41 cd) "folder1"))

;;(define S43 ((run S42 del) "*.*"))
(define S43.0 ((run S42 del) "goo4.docx")) ;;
(define S43.1 ((run S43.0 del) "foo3.docx")) ;;
(define S43.2 ((run S43.1 del) "foo2.txt")) ;;
(define S43 ((run S43.2 del) "foo1.txt")) ;;

(define S44 ((run S43 cd) "..")) ;((run S43 cd) ".."))
(define S45 ((run S44 rd) "folder1"))

(define ITEMS (search-item-by-path "C:/folder1/" (get-drive-content(get-current-drive S35))))

;copiando carpetas y archivos
(define S46 ((run S35 copy) "foo1.txt" "c:/folder3/"))
(define S47 ((run S46 cd) ".."))
(define S48 ((run S47 copy) "folder1" "d:/"))

;moviendo carpetas y archivos
(define S49 ((run S48 move) "folder3" "d:/"))
(define S50 ((run S49 cd) "folder1"))
(define S51 ((run S50 move) "foo3.docx" "d:/folder3/"))


