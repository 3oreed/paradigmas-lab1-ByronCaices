#lang racket
(provide (all-defined-out))

;CONSTRUCTOR

(define make-drive (lambda (letter ;0
                             name
                             cap
                             content)
                           (list
                             letter
                             name
                             cap
                             null
                             null
                             null
                             null
                             null
                             null
                             content)))

(define drive (lambda (letter name cap)
                        (make-drive  letter
                                     name
                                     cap
                                     null)))

(define set-drive-content (lambda (drive-arg content)
                            (make-drive (get-letter drive-arg)
                                        (get-drive-name drive-arg)
                                        (get-drive-cap drive-arg)
                                        content)))

(define get-letter (lambda (drive-arg)
                     (car drive-arg)))

(define get-drive-name (lambda (drive-arg)
                     (cadr drive-arg)))

(define get-drive-cap (lambda (drive-arg)
                     (caddr drive-arg)))

(define get-drive-content (lambda (drive-arg)
                     (list-ref drive-arg 9)))

(define drive1 (drive #\C "drive1" 1000))