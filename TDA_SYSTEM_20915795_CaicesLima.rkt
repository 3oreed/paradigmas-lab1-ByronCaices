#lang racket

(require "TDA_DRIVE_20915795_CaicesLima.rkt")
(require "TDA_USER_20915795_CaicesLima.rkt")
(require "TDA_FOLDER_20915795_CaicesLima.rkt")
(require "TDA_FILE_20915795_CaicesLima.rkt")
(require racket/date)
(provide (all-defined-out))

; copy debe verificar que el archivo se encuentre en la ruta actual del
; system, si no, no puede ejecutar la funcion

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
                         current-drive ;3
                         users ;4
                         drives ;5
                         system-date ;6
                         trashcan);7
                        (list system-name
                              loged-user
                              (string-upcase current-path) ;string-downcase
                              current-drive
                              users
                              drives
                              system-date
                              trashcan)))

(define system (lambda (name)
                 (make-system name
                              "" ;user
                              "" ;path
                              null ;drive
                              null ;users
                              null ;drives
                              (crnt-date) ;fecha
                              null) ;papelera
                 ))




; SELECTORES

(define get-system-name (lambda (system)
                          (car system)))

(define get-loged-user (lambda (system)
                          (cadr system)))

(define get-path (lambda (system)
                          (caddr system)))

(define get-current-drive (lambda (system)
                            (cadddr system)))

(define get-users (lambda (system)
                          (list-ref system 4)))

(define get-drives (lambda (system)
                          (list-ref system 5)))

(define get-system-date (lambda (system)
                          (list-ref system 6)))

(define get-trashcan (lambda (system)
                          (list-ref system 7)))

; MODIFICADORES

(define set-system-name (lambda (system-arg name)
                          (make-system name
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))))

(define set-loged-user (lambda (system-arg user)
                          (make-system (get-system-name system-arg)
                                       user
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))))

(define set-path (lambda (system-arg path)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       path
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))))

(define set-current-drive (lambda (system-arg drive)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       drive
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))))

(define set-users (lambda (system-arg users)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       users
                                       (get-drives system-arg)
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))))



(define set-drives (lambda (system-arg drives)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       drives
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))))

(define my-string-null? (lambda (str)
                          (equal? str "")))


(define loged-user? (lambda (system-arg)
                      (if (my-string-null? (get-loged-user system-arg))
                          #f
                          #t)))

(define existing-user? (lambda (system-arg user)
                          (if (not(member user (get-users system-arg)))
                              #f
                              #t)))

(define existing-drive? (lambda (system-arg letter)
                          (if (not(member letter (map get-letter (get-drives system-arg))))
                              #f
                              #t)))



; Si el drive no existe retorna F
; Si el drive existe retorna T

; Funcion que busca un drive en una lista de drives y lo retorna
;(define search-drive)
;(define registered? (lambda (system-arg))

#|
(define rebuild-system (lambda (system-arg
                                system-name
                                loged-user
                                current-path
                                current-drive
                                users
                                drives
                                system-date)
                         (set-(set-drives(set-users(set-current-drive(set-path(set-loged-user(set-system-name system system-name)loged-user)current-path)current-drive)users)drives)system-date)
                            
                         
|#


; REQUERIMIENTOS FUNCIONALES LAB 1

(define run (lambda (system cmd) (cmd system)))


(define add-drive (lambda (system-arg)
                    (lambda (letter drive-name cap)
                      (if (not(existing-drive? system-arg letter))
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       
                                       (string-append (string letter) ":/")
                                        
                                       ;(get-current-drive system-arg)
                                       (cons(drive letter drive-name cap)'())
                                      
                                           
                                       (get-users system-arg)
                                       (cons
                                        (drive letter drive-name cap)
                                        (get-drives system-arg))
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))
                          system-arg))))
                          

(define register (lambda (system-arg)
                   (lambda (user-name)
                     (if (not (member user-name (get-users system-arg)))
                         (make-system (get-system-name system-arg)
                                      (get-loged-user system-arg)
                                      (get-path system-arg)                            
                                      (get-current-drive system-arg)
                                      (cons (user user-name)
                                            (get-users system-arg))
                                      (get-drives system-arg)
                                      (get-system-date system-arg)
                                      (get-trashcan system-arg))
                          system-arg))))
                         



(define login (lambda (system-arg)
                (lambda (user-name)
                  (if (and
                       (not(loged-user? system-arg))
                       (existing-user? system-arg user-name))
                         
                         (make-system (get-system-name system-arg)
                                      user-name
                                      (get-path system-arg)                            
                                      (get-current-drive system-arg)
                                      (get-users system-arg)
                                      (get-drives system-arg)
                                      (get-system-date system-arg)
                                      (get-trashcan system-arg))
                         system-arg))))

(define logout (lambda (system-arg)
                 (make-system (get-system-name system-arg)
                                      ""
                                      (if (not(null?(get-current-drive system-arg)))
                                                     (string-append
                                                         (string (get-letter (car(get-current-drive system-arg))))
                                                         ":/")
                                                     "")
                                      (get-current-drive system-arg)
                                      ;(car (get-drives system-arg))
                                      (get-users system-arg)
                                      (get-drives system-arg)
                                      (get-system-date system-arg)
                                      (get-trashcan system-arg))))
                  




; Funcion que dada una letra busca un drive
; en una lista de drives y lo retorna

(define lista-drives '((#\D "Util" 2000 ()) (#\C "SO" 1000 ())))

                          

(define search-drive (lambda (letter drives-list founded-drive)
                       (if (and
                            (not(null? drives-list))
                            (equal? (get-letter (car drives-list)) letter))
                               (cons (car drives-list) founded-drive)
                               (if(not(null? drives-list))
                                  (search-drive letter (cdr drives-list) founded-drive)
                                  founded-drive))))
                                  
(define remove-drive (lambda (system-arg letter drives-list)
                       (remove
                        (search-drive letter
                                      (get-drives system-arg)
                                      '())
                        (get-drives system-arg))))
 
(define switch-drive2 (lambda (system-arg)
                    (lambda (letter)
                      (if (and
                           (existing-drive? system-arg letter)
                           (loged-user? system-arg))
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       
                                       (if(null?(search-drive letter (get-drives system-arg) '()))
                                           (get-path system-arg)
                                           (string-append (string letter) ":/"))
                                        
                                       (if(null?(search-drive letter (get-drives system-arg) '()))
                                          (get-current-drive system-arg)
                                          (search-drive letter (get-drives system-arg) '()))
                                       ;'(1 2 3)    
                                       (get-users system-arg)
                                       ;quitar el que agrego a current-drive
                                       ;y agregar el que viene de current-drive
                                       (get-drives system-arg)
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))
                          system-arg))))



; Funcion que en una lista LISTA de listas Busca una LISTA current
; y la reemplaza por una lista older

;Funcion que recibe un current-drive y los system-drives

(define compara-letter (lambda (drive1 drive2)
                         (equal? (get-letter drive1)
                                 (get-letter drive2))))



(define my-replace (lambda (current older cola)
                     (cond
                       [(and(not(null? older))
                            (compara-letter (car current) (car older)))
                               (my-replace current (cdr older)
                                           (cons (car current) cola))]
                       [(and(not(null? older))
                            (not(compara-letter (car current) (car older))))
                             (my-replace current (cdr older)
                                         (cons (car older) cola))]
                       [else cola ])))


(define switch-drive (lambda (system-arg)
                    (lambda (letter)
                      (if (and
                           (existing-drive? system-arg letter)
                           (loged-user? system-arg))
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       
                                       (if(null?(search-drive letter (get-drives system-arg) '()))
                                           (get-path system-arg)
                                           (string-append (string letter) ":/"))
                                        
                                       (if(null?(search-drive letter (get-drives system-arg) '()))
                                          (get-current-drive system-arg)
                                          (search-drive letter (get-drives system-arg) '()))
                                       ;'(1 2 3)    
                                       (get-users system-arg)
                                       ;quitar el que agrego a current-drive
                                       ;y agregar el que viene de current-drive
                                       (my-replace (get-current-drive system-arg)
                                                   (get-drives system-arg)
                                                   '())
                                       (get-system-date system-arg)
                                       (get-trashcan system-arg))
                          system-arg))))

; MD y Funciones adicionales
; MODIFICAR FOLDER DESDE SYSTEM

(define (sys-buscar-folder-hijo folder name)
  (cond
    [(null? folder) #f]
    [(equal? (folder-name folder) name) folder]
    [else (sys-buscar-folder-hijo-aux (folder-content-hijos folder) name)]))

(define (sys-buscar-folder-hijo-aux content-hijos name)
  (cond
    [(null? content-hijos) #f]
    [(sys-buscar-folder-hijo (car content-hijos) name) (car content-hijos)]
    [else (sys-buscar-folder-hijo-aux (cdr content-hijos) name)]))




;###
(define (sys-insertar-folder system-arg folder name)
  (cond
    [(null? folder) (make-folder name '() '() (if (pair? (get-folder-location folder)) (string-append (get-folder-location folder) "/" name) "h") (get-loged-user system-arg) '() '() '() "" '())]
    [(sys-buscar-folder-hijo folder name) folder] ; No agregar hijo repetido
    [else (make-folder (folder-name folder)
                       (get-create-date folder)
                       (get-mod-date folder)
                       (get-folder-location folder)
                       (get-folder-creator folder)
                       (get-folder-size folder)
                       (get-items folder)
                       (get-folder-security folder)
                       (get-folder-pass folder)
                       (cons (make-folder name
                                          (crnt-date-folder)
                                          (crnt-date-folder)
                                          (if (pair? (get-folder-location folder)) (string-append (get-folder-location folder) "/" name) "")
                                          (get-loged-user system-arg)
                                          '()
                                          '()
                                          '()
                                          ""
                                          '())
                             (folder-content-hijos folder)))]))



;#####


;######
(define (sys-insertar-folder-en-hijos system-arg padre . nombres-folders)
  (define (actualizar-hijos padre folder-buscado nueva-folder)
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
                        (if (equal? (folder-name hijo) folder-buscado)
                            nueva-folder
                            hijo))
                      (folder-content-hijos padre))))
  (if (null? nombres-folders)
      padre
      (let* ([folder-buscado (car nombres-folders)]
             [folder-encontrado (sys-buscar-folder-hijo padre folder-buscado)])
        (if folder-encontrado
            (actualizar-hijos padre folder-buscado
                             (apply sys-insertar-folder-en-hijos system-arg folder-encontrado (cdr nombres-folders)))
            (let ([nueva-folder (make-folder folder-buscado
                                              (crnt-date) ; create-date
                                              (crnt-date) ; mod-date
                                              (if (pair? (get-folder-location padre))
                                                  (string-append (get-folder-location padre) "/" folder-buscado)
                                                  (string-append "/" folder-buscado)) ; location
                                              (get-loged-user system-arg) ; creator
                                              '() ; size
                                              '() ; items
                                              '() ; security
                                              "" ; password
                                              '())]) ; content-hijos
              (sys-insertar-folder system-arg
                                   (actualizar-hijos padre folder-buscado nueva-folder)
                                   folder-buscado))))))




(define make-dir (lambda (system-arg folder-name)
                   (make-system (get-system-name system-arg)
                                (get-loged-user system-arg)
                                (get-path system-arg)                            
                                (list (sys-insertar-folder-en-hijos
                                       system-arg
                                       (car (get-current-drive system-arg))
                                       folder-name))
                                (get-users system-arg)
                                (get-drives system-arg)
                                (get-system-date system-arg)
                                (get-trashcan system-arg))))




(define md (lambda (system-arg)
             (lambda (folder-name)
               (make-dir system-arg folder-name))))

(define (path-to-list path)
  (cdr(cons
    ""
    ;(string-ref path 0)
        (string-split (substring path 2) "/"))))

(define path? (lambda (path)
                (string-contains? path "/")))

(define root? (lambda (path)
                (if(and(path? path)
                       (= (string-length path) 3))
                   #t
                   #f)))

#|
(define change-dir (lambda (system-arg argument)
             (cond
                 [(and (not(root? (get-path system-arg))) (equal? argument ".."))
                  (string-join(reverse(cdr(reverse(string-split (get-path system-arg) "/"))))"/")]
                 [(equal? argument "/")
                  (substring (get-path system-arg) 0 3)]
                 [(path? (get-path system-arg))
                   ()
|#
               
;(string-join(reverse(cdr(reverse(string-split "C:/Folder1/Folder11" "/"))))"/")
            
; EJEMPLOS

(define S0 (system "System Tester"))
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))

(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))

(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user2"))

(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))

;(define S100 ((run S10 md) "folder1"))

(define S11 ((run S10 switch-drive) #\K))
(define S12 ((run S11 switch-drive) #\C))

(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))
(define S15 ((run S14 md) "folder2"))
(define S16 ((run S15 md) "folder3"))

;(define current-test (get-current-drive S16))

;(define drives-test (get-drives S16))


;(define S17 ((run S16 switch-drive) #\D))


;(define drive0 (drive #\C "drive1" 1000))

;(define f0 drive0)

;(define f1 (sys-insertar-folder-en-hijos S12 f0 "Folder 1"))
;(define f2 (sys-insertar-folder-en-hijos S12 f1 "Folder 1" "Subfolder1"))


;(define f2 (sys-insertar-folder-en-hijos f1 S12 "Folder 1"))
;(define f3 (sys-insertar-folder-en-hijos f2 "Folder 2"))
;(define f4 (sys-insertar-folder-en-hijos f3 "Folder 2"))
;(define f5 (sys-insertar-folder-en-hijos f4 "Folder 3"))

;(define f6 (sys-insertar-folder-en-hijos f5 "Folder 1" "Subfolder 1"))
;(define f7 (sys-insertar-folder-en-hijos f6 "Folder 1" "Subfolder 2"))
;(define f8 (sys-insertar-folder-en-hijos f7 "Folder 1" "Subfolder 2"))
;(define f9 (sys-insertar-folder-en-hijos f8 "Folder 1" "Subfolder 2" "Subsubfolder1"))












