#lang racket


(provide (all-defined-out))

#|
(define lista-strings '("manzana" "banana" "pera" "uva"))
(define lista-ordenada (sort lista-strings string<?))
(displayln lista-ordenada)

|#
                      

(define make-drive (lambda (letter name cap contenido)
                     (list letter name cap contenido)))

(define drive (lambda (letter name cap)
                (make-drive letter
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
                     (cadddr drive-arg)))

(define set-drive-letter (lambda (drive-arg letter)
                           (make-drive letter
                                       (get-drive-name drive-arg)
                                       (get-drive-cap drive-arg)
                                       (get-drive-content drive-arg))))

(define set-drive-name (lambda (drive-arg name)
                           (make-drive (get-letter drive-arg)
                                       name
                                       (get-drive-cap drive-arg)
                                       (get-drive-content drive-arg))))

(define set-drive-cap (lambda (drive-arg)
                           (make-drive (get-letter drive-arg)
                                       (get-drive-name)
                                       (get-drive-cap drive-arg)
                                       (get-drive-content drive-arg))))

(define set-drive-content (lambda (drive-arg content-arg)
                           (make-drive (get-letter drive-arg)
                                       (get-drive-name)
                                       (get-drive-cap drive-arg)
                                       content-arg)))


(define (path-to-list2 path)
  (cons
    ""
    ;(string-ref path 0)
        (string-split (substring path 2) "/")))



#|
(existing-folder? (lambda (drive-arg folder-name)
                          (if (not(member folder-name (map get-folder-name (get-drive-content drive-arg))))
                              #f
                              #t)))

(define search-folder2 (lambda (drive-content folder-name path-list)
                        (if(not(null?? path-list)))
                           (if (equal? (car path-list) (get-folder-name (car drive-content)))
                               ;si son iguales entonces encontre la carpeta
                               ;y debo retornarla
                               (car drive-content)
                               (search-folder (cdr drive-content) folder-name target-path)
                           
|#



(define drive1 (drive #\C "drive1" 1000))






