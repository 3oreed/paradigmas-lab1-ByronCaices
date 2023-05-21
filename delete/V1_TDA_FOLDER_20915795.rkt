#lang racket

(require racket/date)
(provide (all-defined-out))

(define (crnt-date-folder)
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

(define make-folder (lambda (folder-name ;0
                             create-date
                             mod-date
                             location
                             creator
                             size
                             items
                             security
                             password
                             content);9
                           (list folder-name
                             create-date
                             mod-date
                             location
                             creator
                             size
                             items
                             security
                             password
                             content)))

(define folder (lambda (name)
                        (make-folder name
                                     (crnt-date-folder)
                                     (crnt-date-folder)
                                     ""
                                     ""
                                     null
                                     null
                                     null
                                     ""
                                     null)))

(define add-subfolder (lambda (folder1 folder2)
                        (make-folder (get-folder-name folder1)
                                     (get-create-date folder1)
                                     (crnt-date-folder); fecha mod
                                     (get-folder-location folder1)
                                     (get-folder-creator folder1)
                                     (get-folder-size folder1)
                                     (get-items folder1)
                                     (get-folder-security folder1)
                                     (get-folder-pass folder1)
                                     (cons
                                      folder2
                                      (get-folder-content folder1)))))

                                     

(define (get-folder-name folder)
  (list-ref folder 0))

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

(define (get-folder-content folder)
  (list-ref folder 9))


(define F1 (folder "folder1"))
(define F1A (folder "folder1A"))
(define F1AA (folder "folder1AA"))
(define run1 (add-subfolder F1 F1A))
(define F2 (folder "folder2"))
(define F3 (folder "folder3"))

(define DC1 (list run1 F2 F3))


