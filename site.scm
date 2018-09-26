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
  ;; transfer the post file to the proper sxmltree which is needed by page
  (readers get-site-readers))


;; create the site object and return it 
(define* (site #:key
	       (posts-directory "Metapost")
	       (build-directory "BlogSite")
	       (asset (make-asset "assets" "BlogSite"))
	       ;; the process field contains an assoc list
	       (process-layer default-process-layer)
               (readers '(sxml-reader html-reader)))
  (make-site asset
	     process-layer
	     posts-directory
	     build-directory
	     readers))

;; write the content to the disk
(define* (write-content posts-directory prefix-directory #:optional process-layer)
  (let* ((file-tree-list (get-file-tree-list posts-directory)))
    (if (pair? file-tree-list)
	(begin
	  (set! file-tree-list (cdr file-tree-list))
	  (while (not (eq? '() file-tree-list))
	    (if (list? (car file-tree-list))
		;; the element is directory which contains at least one file , the file may be a directory too!
		(let ((prefix-directory* (string-append prefix-directory "/" (caar file-tree-list)))
		      (posts-directory* (string-append posts-directory "/" (caar file-tree-list))))
		  (mkdir-p prefix-directory)
		  (write-content posts-directory*
				 prefix-directory*
				 process-layer))
		;; the element is a file
		(if (is-directory? (string-append posts-directory "/" (car file-tree-list)))
		    ;; no matter whether the directory contains file, create it
		    (mkdir-p (string-append prefix-directory "/" (car file-tree-list)))
		    (begin
		      (if (not (file-exists? prefix-directory))
			  (mkdir-p prefix-directory))
		      (page (read-post (string-append posts-directory "/" (car file-tree-list)))
			    prefix-directory
			    process-layer))))
	    ;; update the list, and process the rest list elements
	    (set! file-tree-list (cdr file-tree-list))))
	(if (is-directory? file-tree-list)
	    ;; no matter whether the directory contains file, create it
	    (mkdir-p (string-append prefix-directory "/" file-tree-list))
	    (begin
	      (if (file-exists? prefix-directory)
		  (if (not (is-directory? prefix-directory))
		      (format (current-error-port) "When building pages. find there exists conflict files!! ~%Whose name is \"~a\".~%~%" prefix-directory))
		  (mkdir-p prefix-directory))
	      (page (read-post file-tree-list)
		    prefix-directory
		    process-layer))))))

;; This procedure will not be deleted!! Just develop it!
;; get the site object and build the site
(define (build-site obj)
  (let ((posts-directory (get-site-postdirectory obj))
	(build-directory (get-site-build-directory obj))
	(assets-obj (get-site-asset obj))
	(process-layer (get-site-process-layer obj)))
    ;; cp the asset src file
    (if (pair? assets-obj)
	(while (not (eq? '() assets-obj))
	  (if (file-exists? (get-asset-target (car assets-obj)))
	      (format (current-error-port) "Warning!! There already exists one \"~a\"!~%" (get-asset-target (car assets-obj)))
	      (begin
		(cp-asset (car assets-obj))
		(format #t "Install source file successfully!~%")))
	  (set! assets-obj (cdr assets-obj)))
	(if (file-exists? (get-asset-target assets-obj))
	      (format (current-error-port) "Warning!! There already exists one \"~a\"!~%" (get-asset-target assets-obj))
	      (begin
		(cp-asset assets-obj)
		(format #t "Install source file successfully!~%"))))
    ;; read the post and build the page and write them to the disk 
    (write-content posts-directory build-directory process-layer))
  (format #t "Building successful!!~%"))
