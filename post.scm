(define-module (Flax post)
    #:use-module (ice-9 match)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-19)
    #:use-module (Flax utils)
    
    #:export (make-post
              is-post?
              get-post-file-name
              get-post-metadata
              get-post-sxml
              
              post-ref))


;; ~metadata~ is a alist
;; ~sxml~ is a sxml tree
(define-record-type <post>
    (make-post file-name metadata sxml)
    is-post?
    (file-name get-post-file-name)
    (metadata get-post-metadata)
    (sxml get-post-sxml))

;; return the metadata of the ~post~ by ~key~
(define (post-ref post key)
    (assq-ref (get-post-metadata post) key))

()


