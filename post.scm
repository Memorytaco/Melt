(define-module (Flax post)
  #:use-module (Flax utils)
  #:use-module (Flax reader)

  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)

  #:export (make-post
            is-post?
            get-post-file-name
            get-post-metadata
            get-post-sxml
	    set-post-sxml
            
	    post-ref))


;; ~metadata~ is an alist
;; ~sxml~ is a sxml tree
(define-record-type <post>
  (make-post file-name metadata sxml)
  is-post?
  (file-name get-post-file-name)
  (metadata get-post-metadata)
  (sxml get-post-sxml set-post-sxml))

;; read one post and return  post object
(define (read-post file-name)
  (let-values ((path-to-file-name (get-absolute-path file-name))
	       ((meta-data content) ((get-reader-proc sxml-reader) (load path-to-file-name)))) 
    (make-post file-name meta-data content)))
