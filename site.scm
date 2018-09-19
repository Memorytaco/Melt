;; NOTE : unfinished file

(define-module (Flax site)
    #:use-module (Flax reader)
    #:use-module (Flax page)
    #:use-module (Flax post)
    #:use-module (Flax asset)
    #:use-module (Flax utils)

    #:use-module (srfi srfi-9)
    
    #:export (create-site
              is-site?
              get-site-title
              get-site-postdirectory
              get-site-build-directory
              get-site-readers
              get-site-builders
	      build-site
	      site))

;; ~title~ is a string
;; ~domain~ is a string
;; ~posts-directory~ where posts found
;; ~build-directory~ generated pages stored in
;; ~readers~ a list of reader objects
;; ~builders~ a list of procedures for building pages
(define-record-type <site>
  (make-site title posts-directory build-directory readers)
  is-site?
  (title get-site-title)
  (posts-directory get-site-postdirectory)
  (build-directory get-site-build-directory)
  (readers get-site-readers))


;; create the site object
(define* (site #:key
	       (title "Welcome to Flax!")
               (posts-directory "posts")
               (build-directory "site")
               (readers '(sxml-reader html-reader)))
  (make-site title
	     posts-directory
	     build-directory
	     readers))
;; need to complete
(define (build-site obj)
  (let ((posts-directory (get-absolute-path (get-site-postdirectory obj)))
	(build-directory (get-absolute-path (get-site-build-directory obj))))
    (format #t "there is something to do!!~%"))
  (format #t "Building Now!!~%"))
