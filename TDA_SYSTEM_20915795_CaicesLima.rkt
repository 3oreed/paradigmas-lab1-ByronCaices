#lang racket

(require "TDA_DRIVE_20915795_CaicesLima.rkt")
(require racket/date)

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
                        (system-name
                         loged-user
                         current-path
                         current-drive
                         users
                         drives
                         system-date)
                        (list system-name
                              loged-user
                              current-path ;string-downcase
                              current-drive
                              users
                              drives
                              system-date)))

(define system (lambda (name)
                 (make-system name
                              null ;user
                              "" ;path
                              null ;drive
                              null ;users
                              null ;drives
                              (crnt-date)) ;fecha
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
                          (caddr(reverse system))))

(define get-drives (lambda (system)
                          (cadr(reverse system))))

(define get-system-date (lambda (system)
                          (car(reverse system))))

; MODIFICADORES

(define set-system-name (lambda (system-arg name)
                          (make-system name
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))

(define set-loged-user (lambda (system-arg user)
                          (make-system (get-system-name system-arg)
                                       user
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))

(define set-path (lambda (system-arg path)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       path
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))

(define set-current-drive (lambda (system-arg drive)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       drive
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))

(define set-users (lambda (system-arg users)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       users
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))



(define set-drives (lambda (system-arg drives)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-current-drive system-arg)
                                       (get-users system-arg)
                                       drives
                                       (get-system-date system-arg))))


(define loged-user? (lambda (system-arg)
                      (if (null? (get-loged-user system-arg))
                          #f
                          #t)))

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
                      (if (not(member letter (map car (get-drives system-arg))))
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       
                                       (string-append (string letter) ":/")
                                        
                                       (get-current-drive system-arg)
                                           
                                       (get-users system-arg)
                                       (cons
                                        (drive letter drive-name cap)
                                        (get-drives system-arg))
                                       (get-system-date system-arg))
                          system-arg))))
                          
#|
(define register (lambda (system-arg)
                   (lambda (user-name)
                     ()
|#

; EJEMPLOS

(define S0 (system "System Tester"))
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))



