(define-module (Flax site)
  #:use-module (Flax reader)
  #:use-module (Flax page)
  #:use-module (Flax post)
  #:use-module (Flax asset)
  #:use-module (Flax process)
  #:use-module (Flax utils)
  
  #:use-module (srfi srfi-9)
  
  #:export (make-site
            is-site?
            get-site-postdirectory
            get-site-build-directory
            get-site-readers
            get-site-process-layer
	    set-site-process-layer
	    
	    build-site
	    site))

;; ~title~ is a string
;; ~domain~ is a string
;; ~posts-directory~ where posts found
;; ~build-directory~ generated pages stored in
;; ~readers~ a list of reader objects
;; ~builders~ a list of procedures for building pages
(define-record-type <site>
  (make-site asset process-layer posts-directory build-directory readers)
  is-site?
  (asset get-site-asset set-site-asset)  ;; the asset obj
  (process-layer get-site-process-layer set-site-process-layer)  ;; process-layer must be a list of processes
  ;; string or string list which is where the posts are
  (posts-directory get-site-postdirectory)
  ;; the object directory where the builded page is sended to
  (build-directory get-site-build-directory)
  ;; transfer the post file to the post
  (readers get-site-readers))


;; create the site object and return it 
(define* (site #:key
	       (posts-directory "Metapost")
	       (build-directory "BlogSite")
	       (asset (make-asset "assets" "BlogSite"))
	       ;; the process field contains an assoc list
	       (process-layer default-process-layer)
               (readers default-reader-list))
  (make-site asset
	     process-layer
	     posts-directory
	     build-directory
	     readers))

;; write the content to the disk
(define* (write-content posts-file-tree prefix-directory
			#:key (flag #f) (environment (getcwd)) (reader-list default-reader-list) (process-layer default-process-layer))
  (if (string? posts-file-tree)
      ;; initilaize the file tree, if flag is #t, onle executed once in this procedure!!
      (if flag
	  (let* ((file-tree (get-file-tree-list posts-file-tree))
		 (environment* (string-append environment "/" (car file-tree))))
	    (write-content (cdr file-tree) prefix-directory
			   #:environment environment*
			   #:reader-list reader-list
			   #:process-layer process-layer))
	  ;;if flag is #f, it is a single element from the file tree list
	  (if (is-directory? (string-append environment "/" posts-file-tree))
	      ;; if the file is a directory, create it 
	      (mkdir-p (string-append prefix-directory "/" posts-file-tree))
	      ;; if not, write the page
	      (let ((post (read-post (string-append environment "/" posts-file-tree)
				     #:reader-list reader-list)))
		(if (not (eq? post '()))
		    (page post prefix-directory process-layer)
		    (format (current-error-port) "Warning! Unrecognized file: ~a ~%" posts-file-tree)))))
      ;; if it is a file list
      (if (pair? posts-file-tree)
	  (let ((prefix-directory* (if flag
				       (string-append prefix-directory "/" (car posts-file-tree))
				       prefix-directory))
		(environment* (if flag
				  (string-append environment "/" (car posts-file-tree))
				  environment))
		(file-tree (if flag
			       (cdr posts-file-tree)
			       posts-file-tree)))
	    (write-content (car file-tree) prefix-directory*
			   #:flag (if (pair? (car file-tree))
				      #t
				      #f)
			   #:environment environment*
			   #:reader-list reader-list
			   #:process-layer process-layer)
	    (write-content (cdr file-tree) prefix-directory*
			   #:environment environment*
			   #:reader-list reader-list
			   #:process-layer process-layer)))))

;; This procedure will not be deleted!! Just develop it!
;; get the site object and build the site
(define (build-site obj)
  (let ((posts-directory (get-site-postdirectory obj))
	(build-directory (get-site-build-directory obj))
	(assets-obj (get-site-asset obj))
	(process-layer (get-site-process-layer obj))
	(reader-list (get-site-readers obj)))
    ;; cp the asset src file
    (if (pair? assets-obj)
	(while (not (eq? '() assets-obj))
	  (begin
	    (if (file-exists? (get-asset-target (car assets-obj)))
		(begin
		  (format (current-error-port) "Warning!! There already exists one \"~a\"!~%" (get-asset-target (car assets-obj)))
		  (format (current-error-port) "This is a warning, If you know what happened, just ignored.~%")))
	    (cp-asset (car assets-obj)))
	  (set! assets-obj (cdr assets-obj))
	  (format #t "Install source file successfully!~%"))
	(begin
	  (if (file-exists? (get-asset-target assets-obj))
	      (begin
		(format (current-error-port) "Warning!! There already exists one \"~a\"!~%" (get-asset-target assets-obj))
		(format (current-error-port) "This is a warning, If you know what happened, just ignored~%")))
	  (cp-asset assets-obj)
	  (format #t "Install source file successfully!~%")))
    ;; read the post and build the page and write them to the disk 
    (write-content posts-directory build-directory
		   #:flag #t
		   #:reader-list reader-list
		   #:process-layer process-layer))
  (format #t "Building successful!!~%"))
