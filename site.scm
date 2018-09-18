;; NOTE : unfinished file

(define-module (Flax site)
    #:use-module (Flax reader)
    #:use-module (Flax page)
    #:use-module (Flax post)
    #:use-module (Flax asset)

    #:use-module (srfi srfi-9)
    
    #:export (create-site
              is-site?
              get-site-title
              get-site-postdirectory
              get-site-build-directory
              get-site-readers
              get-site-builders))

;; ~title~ is a string
;; ~domain~ is a string
;; ~posts-directory~ where posts found
;; ~build-directory~ generated pages stored in
;; ~readers~ a list of reader objects
;; ~builders~ a list of procedures for building pages
(define-record-type <site>
  (make-site title posts-directory build-directory
             readers builders)
  is-site?
  (title get-site-title)
  (posts-directory get-site-postdirectory)
  (build-directory get-site-build-directory)
  (readers get-site-readers)
  (builders get-site-builders))


;; create the site object
(define* (site #:key
	       (title "Welcome to Flax!")
               (posts-directory "posts")
               (build-directory "site")
               (readers '(sxml-reader html-reader))
               (builders '()))
  (make-site title
	     posts-directory
	     build-directory
	     readers
	     builders))
;; need to complete
(define (build-site obj)
  (format #t "Temporary obj is ~a" obj)
  (format #t "Building Now!!~%"))
