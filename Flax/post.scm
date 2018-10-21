(define-module (Flax post)
  #:use-module (Flax utils)
  #:use-module (Flax reader)

  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)


  #:export (make-post
            is-post?
            get-post-file-name
            get-post-metadata
            get-post-sxml
	    set-post-sxml
            
	    read-post
	    post-ref
	    register-metadata-parser!
	    read-metadata-headers
	    parse-metadata))


;; ~metadata~ is an alist
;; ~sxml~ is a sxml tree
(define-record-type <post>
  (make-post file-name metadata sxml)
  is-post?
  (file-name get-post-file-name)
  (metadata get-post-metadata)
  (sxml get-post-sxml set-post-sxml))

;; read one post and return post object
(define* (read-post file-name #:key (reader-list default-reader-list))
  (if (pair? reader-list)
      (if (is-reader-available? (car reader-list) file-name)
	  (let-values (((meta-data content) (proc-of-reader (car reader-list) file-name)))
	      (make-post (basename file-name) meta-data content))
	    (read-post file-name #:reader-list (cdr reader-list)))
      (begin
	(format (current-error-port) "Unmatched file : ~a ~%" file-name)
	'())))

(define (post-ref post key)
  (assq-ref (get-post-metadata post) key))


;; read meta data
(define %metadata-parsers
  (make-hash-table))

(define (register-metadata-parser! name parser)
  (hash-set! %metadata-parsers name parser))

(define (metadata-parser key)
  (or (hash-ref %metadata-parsers key) identity))

(define (parse-metadata key value)
  ((metadata-parser key) value))

(define (read-metadata-headers port)
  (let loop ((metadata '()))
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)
	(error "end of file while readig metadata: " (port-filename port)))
       ((string=? line "---")
	metadata)
       (else
	(match (map string-trim-both (string-split-at line #\:))
	  (((= string->symbol key) value)
	   (loop (alist-cons key (parse-metadata key value) metadata)))
	  (_ error "invalid metadata format: " line)))))))

(register-metadata-parser!
 'type
 (lambda (str)
   (string->symbol str)))

(register-metadata-parser!
 'depth
 (lambda (str)
   (string->number str)))
