(library (melt site)
  (export create-site)
  (import (scheme)
          (melt structure)
		  (melt invoke)
          (melt utils)
		  (melt data)
		  (melt asset)
		  (melt page))
  
  (import type-site)

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
		  (make-site layout comt attr
					 ;;(apply create-data layout)
					 ;;(apply create-data comt)
					 ;;(apply create-data attr)
					 ))])))
  
  )
