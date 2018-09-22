
(define-module (Flax site)
    #:use-module (Flax reader)
    #:use-module (Flax page)
    #:use-module (Flax post)
    #:use-module (Flax asset)
    #:use-module (Flax theme)
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
  (make-site title author asset theme posts-directory build-directory readers)
  is-site?
  ;; the site title which is a string
  (title get-site-title)
  ;; author is a string which is your name or something else
  (author get-site-author)
  (asset get-site-asset set-site-asset)  ;; the asset obj
  (theme get-site-theme set-site-theme)  ;; theme must be a list of themes
  ;; string which is where the posts are
  (posts-directory get-site-postdirectory)
  ;; the object directory where the builded page is sended to
  (build-directory get-site-build-directory)
  ;; transfer the post file to the proper sxmltree which is needed by page
  (readers get-site-readers))


;; create the site object and return it 
(define* (site #:key
	       (title "Welcome to Flax!")
	       (author "Memorytoco")
	       (asset (make-asset "assets" "site"))
	       ;; the theme field contains an assoc list
	       (theme `((index . ,default-index-theme)
			(post . ,default-post-theme)))
               (posts-directory "posts")
               (build-directory "site")
               (readers '(sxml-reader html-reader)))
  (make-site title
	     author
	     asset
	     theme
	     posts-directory
	     build-directory
	     readers))

;; write the content to the disk
(define* (write-content posts-directory build-directory #:key theme-assoc-list)
  (let* ((file-tree-list (get-file-tree-list posts-directory)))
    (set! file-tree-list (cdr file-tree-list))
    (while (not (eq? '() file-tree-list))
      (if (list? (car file-tree-list))
	  ;; the element is directory
	  (let ((build-directory* (string-append build-directory "/" (caar file-tree-list)))
		(posts-directory* (string-append posts-directory "/" (caar file-tree-list))))
	    (mkdir-p build-directory)
	    (write-content posts-directory
			   build-directory
			   #:theme-assoc-list theme-assoc-list))
	  ;; the element is a file
	  (begin
	    ;; mark -------------------------------
	    (page (read-post (string-append posts-directory "/" (car file-tree-list))) build-directory
		  #:theme-assoc-list theme-assoc-list)))
      ;; update the list 
      (set! file-tree-list (cdr file-tree-list)))))

;; This procedure will not be deleted!! Just develop it!
;; get the site object and build the site
(define (build-site obj)
  (let ((posts-directory (get-site-postdirectory obj))
	(build-directory (get-site-build-directory obj))
	(assets-obj (get-site-asset obj))
	(theme-assoc-list (get-site-theme obj)))
    (cp-asset assets-obj)
    (write-content posts-directory build-directory #:theme-assoc-list theme-assoc-list)) ;; temporary execution mark!!!!!
  (format #t "Building successful!!~%"))
