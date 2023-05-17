#lang racket
(provide (all-defined-out))

(define user (lambda (name) name))


(define filtro (lambda (x)
                 (not(string-contains? x "hola"))))

(define names (list "pehola" "alo" "lola" "hola" "papaholapapa"))