(define-module (Flax utils)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 match)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-13)
               #:use-module (srfi srfi-19)
               #:use-module (srfi srfi-26)

               #:export (absolute-path
                         relative-path
                         decompose-file-name
                         compose-file-name))

;; return the absolute path of the file
(define absolute-path 
  (lambda (file-name)
    "Return the absolute path of the file"
    (if (absolute-file-name? file-name)
        file-name
        (string-append (getcwd) "/" file-name))))

;; decompose a string
;; example
;; "
(define decompose-file-name
  (lambda (filename)
    "Split filename into components seperated by '/'."
    (match filename
      ("" '())  ;; if string is empty
      ("/" '("")) ;; if string only has one '/'
      (_ (delete "" (string-split filename #\/))))))

;; components is a list of strings
;; like ("hello" "nice" "good") => /hello/nice/good
(define compose-file-name
  (lambda (components)
    "put all strings of the filename together"
    (string-join components "/" 'prefix)))
