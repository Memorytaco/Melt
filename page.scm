(define-module (Flax page)
    #:use-module (ice-9 match)
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-26)
    #:use-module (Flax utils)
    
    #:export (make-page
              is-page?
              get-page-file-name
              get-page-contents
              get-page-writer
              write-page))

(define-record-type <page>
    (make-page file-name contents writer)
    is-page?
    (file-name get-page-file-name)
    (contents  get-page-contents)
    (writer    get-page-writer))

(define write-page
    (lambda (page output-directory) 
        (match page
            (($ <page> file-name contents writer)
             (let ((output (string-append output-directory "/" file-name)))
                (mkdir-p (dirname output))
                (call-with-output-file output (cut writer contents <>)))))))