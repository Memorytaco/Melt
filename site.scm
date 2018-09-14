(define-module (Flax site)
    #:use-module (Flax reader)
    #:use-module (Flax page)
    #:use-module (Flax post)
    #:use-module (Flax asset)
    
    #:export (create-site
              is-site?
              get-site-title
              get-site-domain
              get-site-postdirectory
              get-site-build-directory
              get-site-metadata
              get-site-readers
              get-site-builders))

;; ~title~ is a string
;; ~domain~ is a string
;; ~posts-directory~ where posts found
;; ~build-directory~ generated pages stored in
;; ~default-metadata~ an alist of arbitrary default metadata for posts
;; ~readers~ a list of reader objects
;; ~builders~ a list of procedures for building pages
(define-record-type <site>
    (make-site title domain posts-directory build-directory
               default-metadata readers builders)
    is-site?
    (title get-site-title)
    (domain get-site-domain)
    (posts-directory get-site-postdirectory)
    (build-directory get-site-build-directory)
    (default-metadata get-site-metadata)
    (readers get-site-readers)
    (builders get-site-builders))


;; create the site object
(define* (create-site #:key (title "Welcome to Flax!")
                            (domain "expamle.com")
                            (posts-directory "posts")
                            (build-directory "site")
                            (default-metadata '())
                            (readers '(sxml-reader html-reader))
                            (builders '())
    (make-site title domain posts-directory build-directory
               default-metadata readers builders)))

;; TODO : unfinished
;; 
(define (build-site site)
    (let ((posts (if (file-exists? (get-site-postdirectory site))
                     ())))))
