#lang racket
(require "tda-drive.rkt")
(require "tda-user.rkt")
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
                         trashcan);6
                        (list system-name
                              loged-user
                              current-path ;string-downcase
                              users 
                              system-date
                              drives
                              trashcan)))

(define system (lambda (name)
                 (make-system name
                              "" ;user
                              "" ;path
                              null ;users
                              (crnt-date) ;fecha
                              null;drives
                              null) ;papelera
                 ))


(define (get-system-name system) (list-ref system 0))
(define (get-loged-user system) (list-ref system 1))
(define (get-current-path system) (list-ref system 2))
(define (get-users system) (list-ref system 3))
(define (get-system-date system) (list-ref system 4))
(define (get-drives system) (list-ref system 5))
(define (get-trashcan system) (list-ref system 6))


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
                                       (get-trashcan  system-arg))))))

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
                                       (get-trashcan  system-arg))))))

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
                                       (get-trashcan  system-arg))
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
                                  (get-trashcan  system-arg)))))

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
                                          (get-current-path system-arg)
                                          (get-users  system-arg)
                                          (get-system-date  system-arg) 
                                          (move-to-head letter (get-drives system-arg))
                                          (get-trashcan  system-arg))
                             system-arg))))

                            

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


