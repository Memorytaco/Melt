(library (melt site)
  (export create-site
		  load-source)
  (import (scheme)
          (melt structure)
		  (melt invoke)
          (melt utils)
		  (melt data)
		  (melt asset)
		  (melt page))
  
  (import type-site)

  ;; TODO: complete the create-site procedure
  (define create-site
    (lambda args
	  (cond
	   [(null? args)
		(make-site (create-data '(index)
								(list (lambda (directory chain)
										((create-writer (string-append directory "/index.html"))
										 (compose (cdr (page-list-query 'index
																		(chain-data-query 'page chain)))
												  (chain-data-query 'renderer chain))))))
				   (create-data)
				   (create-data '(domain)
								'("localhost")))]
	   [else
		(let ((layout (car args))
			  (comt (car (cdr args)))
			  (attr (car (cdr (cdr args)))))
		  (make-site layout comt attr))])))
  
  ;; get the layout lambda
  (define (load-source name)
	(define (read-fasl-lambda path)
	  (define binary-port (open-file-input-port path))
	  (define obj (eval (fasl-read binary-port)))
	  (close-input-port binary-port)
	  obj)
	(define fasl-name (string-append ".melt/fasl/" name))
	(define res-name (string-append ".melt/resource/" name ".scm"))
	(if (file-exists? res-name)
		(begin
		  (if (file-exists? fasl-name)
			  (delete-file fasl-name))
		  (fasl-file res-name fasl-name)
		  (read-fasl-lambda fasl-name))
		(if (file-exists? fasl-name)
			(read-fasl-lambda fasl-name)
			(error name "file not exists")))
	)
  )
