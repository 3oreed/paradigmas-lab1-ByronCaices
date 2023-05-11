#lang racket
(require "tda-drive.rkt")
(require "tda-user.rkt")
(require "tda-folder.rkt")
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

;Funcion que mueve un elemento de una lista 
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
; se toma el current-drive y se le agrega a su contenido el
; si y solo si nuevo folder si este no existe
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




