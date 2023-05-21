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
                             content)))

(define drive (lambda (letter name cap)
                        (make-drive  letter
                                     name
                                     cap
                                     null)))

(define get-letter (lambda (drive-arg)
                     (car drive-arg)))

(define get-drive-name (lambda (drive-arg)
                     (cadr drive-arg)))

(define get-drive-cap (lambda (drive-arg)
                     (caddr drive-arg)))

(define get-drive-content (lambda (drive-arg)
                     (list-ref drive-arg 3)))
