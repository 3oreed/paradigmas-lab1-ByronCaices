#lang racket

(require "TDA_DRIVE_20915795_CaicesLima.rkt")
(require racket/date)



(define make-system (lambda
                        (system-name
                         loged-user
                         current-path
                         users
                         drives
                         system-date)
                        (list system-name
                              loged-user
                              current-path ;string-downcase
                              users
                              drives
                              system-date)))

(define system (lambda (name)
                 (make-system name
                              null ;user
                              null ;path
                              null ;users
                              null ;drives
                              (crnt-date)) ;fecha
                 ))

(define (crnt-date2)
          (define fecha (current-date))
                          (string-append
                             (number->string (date-day fecha))
                             "/"
                             (number->string (date-month fecha))
                             "/"
                             (number->string (date-year fecha))))


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


(define get-system-name (lambda (system)
                          (car system)))

(define get-loged-user (lambda (system)
                          (cadr system)))

(define get-path (lambda (system)
                          (caddr system)))

(define get-users (lambda (system)
                          (cadddr system)))

(define get-drives (lambda (system)
                          (cadr(reverse system))))

(define get-system-date (lambda (system)
                          (car(reverse system))))

(define set-system-name (lambda (system-arg name)
                          (make-system name
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))

(define set-loged-user (lambda (system-arg user)
                          (make-system (get-system-name system-arg)
                                       user
                                       (get-path system-arg)
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))

(define set-path (lambda (system-arg path)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       path
                                       (get-users system-arg)
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))

(define set-users (lambda (system-arg users)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       users
                                       (get-drives system-arg)
                                       (get-system-date system-arg))))



(define set-drives (lambda (system-arg drives)
                          (make-system (get-system-name system-arg)
                                       (get-loged-user system-arg)
                                       (get-path system-arg)
                                       (get-users system-arg)
                                       drives
                                       (get-system-date system-arg))))

(define loged-user? (lambda (system-arg)
                      (if (null? (get-loged-user system-arg))
                          #f
                          #t)))


(define s0 (system "System Tester"))

(define run (lambda (system cmd) (cmd system)))

(define drive1 (drive #\C "SO" 1000))
