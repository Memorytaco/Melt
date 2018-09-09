(define-module (Flax utils)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 match)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-19)
               #:use-module (srfi srfi-26)
               #:export (absolute-path
                         relative-path))

;; return the absolute path of the file
(define absolute-path 
  "Return the absolute path of the file"
  (lambda (file-name)
    (if (absolute-file-name? file-name)
        file-name
        (string-append (getcwd) "/" filename))))
