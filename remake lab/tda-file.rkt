#lang racket

(provide (all-defined-out))

(define make-file (lambda (file-name ;0
                           extension
                           text
                           creator
                           password
                           create-date
                           mod-date
                           security
                           location
                           type);9
                        (list file-name ;0
                              extension ;1
                              text ;2
                              creator ;3
                              password ;4
                              create-date ;5
                              mod-date ;6
                              security ;7
                              location ;8
                              type))) ;9

(define file (lambda (name ext text . security)
               (make-file name
                          ext
                          text
                          "" ;sec
                          "" ;pass
                          null
                          null
                          security
                          ""
                          "")))
;(define hide?)
;(define read-only?)

(define file1 (file "goo4.docx" "docx" "hello world 4" #\h #\r))
(define file2 (file "goo4.docx" "docx" "hello world 4"))

(define hide? (lambda (file-arg)
                (if (not(member #\h (get-file-security file-arg)))
                    #f
                    #t)))

(define read-only? (lambda (file-arg)
                (if (not(member #\r (get-file-security file-arg)))
                    #f
                    #t)))

(define (get-file-name file-arg)
  (list-ref file-arg 0))

(define (get-extension file-arg)
  (list-ref file-arg 1))

(define (get-text file-arg)
  (list-ref file-arg 2))

(define (get-file-security file-arg)
  (list-ref file-arg 7))

(define (get-file-password file-arg)
  (list-ref file-arg 4))

(define (get-create-date-file file-arg)
  (list-ref file-arg 5))

(define (get-mod-date-file file-arg)
  (list-ref file-arg 6))

(define (get-file-location file-arg)
  (list-ref file-arg 8))

(define (get-file-creator file-arg)
  (list-ref file-arg 3))

(define (get-file-type file-arg)
  (list-ref file-arg 9))

