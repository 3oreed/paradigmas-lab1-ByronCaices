#lang racket
(require "tda-drive.rkt")
(require "tda-user.rkt")
(require "tda-folder.rkt")
(require "tda-file.rkt")
(require racket/date)
(provide (all-defined-out))

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


(define (get-system-name system) (list-ref system 0))
(define (get-loged-user system) (list-ref system 1))
(define (get-current-path system) (list-ref system 2))
(define (get-users system) (list-ref system 3))
(define (get-system-date system) (list-ref system 4))
(define (get-drives system) (list-ref system 5))
(define (get-trashcan system) (list-ref system 6))
(define (get-paths system) (list-ref system 7))
(define (get-current-drive system) (car (get-drives system)))


(define run (lambda (system cmd) (cmd system)))

(define lista-n-elementos (lambda (e n cola)
                            (if (> n 0)
                                (lista-n-elementos e (- n 1) (cons e cola))
                                cola)))

(define existing-letter? (lambda (letter drives)
                        (cond
                          [(null? drives) #f]
                          [(equal? letter (get-letter(car drives))) #t]
                          [else
                           (existing-letter? letter (cdr drives))])))


                            
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

;funcion que verifica si un user existe o no
(define existing-user? (lambda (user-name users)
                        (cond
                          [(null? users) #f]
                          [(equal? user-name (car users)) #t]
                          [else
                           (existing-user? user-name (cdr users))])))

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

;Funcion que verifica si un string es vacio o nulo
(define my-string-null? (lambda (str)
                          (equal? str "")))


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

;Funcion que mueve un elemento de una lista a su cabeza
(define move-to-head (lambda (e lista)
                       (cons (first (filter
                                     (lambda (x) (char=? e (first x)))
                                     lista))
                             (remove (first (filter
                                             (lambda (x) (char=? e (first x)))
                                             lista))
                                     lista))))



(define switch-drive (lambda (system-arg)
                       (lambda (letter)
                         (if (existing-letter? letter (get-drives system-arg))
                             (make-system (get-system-name  system-arg) 
                                          (get-loged-user system-arg)
                                          (string-append (string letter) ":/")
                                          (get-users  system-arg)
                                          (get-system-date  system-arg) 
                                          (move-to-head letter (get-drives system-arg))
                                          (get-trashcan  system-arg)
                                          (get-paths system-arg))
                             system-arg))))

; constructor de un folder dado un system
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
                                       
;funcion que dada una ruta de un system busca el drive que corresponde y lo retorna
;"C:/"
(define search-drive-by-path (lambda (system-arg)
                               (let ([letter (string-ref(car(string-split (get-current-path system-arg) "/"))0)])
                                 letter)))

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


(define cd-type (lambda (arg)
                  (cond
                    [(equal? arg "/") "root"]
                    [(equal? arg "..") "back"]
                    [(equal? (substring arg 1 3) ":/") "path"]
                    [else "folder"])))

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


(define existing-file? (lambda (file-name system-arg)
                           (let ([new-path (string-append (get-current-path system-arg) file-name)])
                             (if (member new-path (get-paths system-arg))
                                 #t
                                 #f))))

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


;En mi system añadir a la papelera o eliminar será:
;mover la ruta desde paths a trashcan y agregar atributo hide
;si el path termina en / es folder sino es file


;retorna lista de paths actualizada
(define del-file-from-paths (lambda (file-name system-arg)
                              (let* ([file-path (string-append (get-current-path system-arg) file-name)]
                                     [paths (get-paths system-arg)])
                                (if (existing-file? file-name system-arg)
                                    (remove file-path paths)
                                    paths))))



;retorna el drive-content actualizada
(define del-file-from-drive (lambda (file-name drive-content cola)
                              (cond
                                [(null? drive-content) (reverse cola)]
                                [(equal? (get-name (car drive-content)) file-name)
                                 (del-file-from-drive file-name (cdr drive-content) cola)]
                                [else
                                 (del-file-from-drive file-name
                                                      (cdr drive-content)
                                                      (cons (car drive-content)
                                                            cola))])))

(define update-drive-content (lambda (file-name drive-arg)
                               (make-drive (get-letter drive-arg)
                                           (get-drive-name drive-arg)
                                           (get-drive-cap drive-arg)
                                           (del-file-from-drive file-name (get-drive-content drive-arg) '()))))

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

;rd
(define del-folderpath (lambda (folder-name system-arg)
                         (let* ([current-path (get-current-path system-arg)]
                                [path-to-del (string-append current-path folder-name "/")])
                           (filter (lambda (x)
                                     (not(string-contains? x path-to-del )))
                                   (get-paths system-arg)))))

(define get-folderpath (lambda (folder-name system-arg)
                         (let* ([current-path (get-current-path system-arg)]
                                [path-to-del (string-append current-path folder-name "/")])
                           (filter (lambda (x)
                                     (string-contains? x path-to-del ))
                                   (get-paths system-arg)))))

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

(define search-item-by-path (lambda (new-path drive-content)
                         (filter
                          (lambda (x) (string-contains? (get-folder-location x) new-path))
                          drive-content)))


                      
                                   

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
(define S43.3 ((run S43.2 del) "foo1.txt")) ;;

(define S44 ((run S43.3 cd) "..")) ;((run S43 cd) ".."))
(define S45 ((run S44 rd) "folder1"))

;copiando carpetas y archivos
;(define S46 ((run S35 copy) "foo1.txt" "c:/folder3/"))
;(define S47 ((run S46 cd) ".."))
;(define S48 ((run S47 copy) "folder1" "d:/"))



