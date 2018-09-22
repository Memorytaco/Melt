;; the page data type

(define-module (Flax page)
  #:use-module (Flax post)
  #:use-module (Flax html)
  #:use-module (Flax theme)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (Flax utils)
  
  #:export (make-page
            is-page?
            get-page-file-name
            get-page-contents
            get-page-writer
            write-page
	    create-writer
	    page))
;;
;; define the record <page>
;; ~file-name~ is a string
;; ~contents~ is a html code
(define-record-type <page>
    (make-page file-name contents writer)
    is-page?
    (file-name get-page-file-name)
    (contents  get-page-contents)
    (writer    get-page-writer))

;; write the rendered page to one directory
;; the directory can be a path
(define (write-page page output-directory)
  (match page
    (($ <page> file-name contents writer)
     (let ((output (string-append output-directory "/" file-name)))
       (mkdir-p (dirname output))
       (writer contents output)))))

;; create the default writer for page
(define (create-writer)
  (lambda (contents output)
    (let ((port (open-output-file output)))
      (sxml->html contents port)
      (close-output-port port))))

;; build the page obj and write it to disk
(define* (page post build-directory #:key  (theme-assoc-list '()) (key '()) (writer (create-writer)))
  (let ((file-name (get-post-file-name post))
	(sxml-content (get-post-sxml post))
	(metadata (get-post-metadata post)))
    (let ((page* (make-page file-name ((get-theme-layout (assq-ref theme-assoc-list 'index)) (assq-ref theme-assoc-list 'post) post) writer)))
      (write-page page* build-directory))))
