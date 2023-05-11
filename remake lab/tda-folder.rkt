#lang racket

(provide (all-defined-out))

(define make-folder (lambda (folder-name ;0
                             create-date ;1
                             mod-date ;2
                             location ;3
                             creator ;4
                             size ;5
                             cant-items ;6
                             security ;7
                             password ;8
                             type) ;9
                           (list folder-name
                             create-date
                             mod-date
                             location
                             creator
                             size
                             cant-items
                             security
                             password
                             type)))




(define folder (lambda (name)
                        (make-folder name ;0
                                     "" ;date 1
                                     "" ;date 2
                                     "" ;3
                                     "" ;4
                                     0 ;5
                                     0 ;6
                                     null ;7
                                     "" ;8
                                     "" )));9

;SELECTORES

(define (get-folder-name folder)
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

(define (folder-type folder)
  (list-ref folder 9))
