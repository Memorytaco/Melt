(library (melt page)
  (export create-page
		  compose
		  create-writer
		  page-list-query)
  (import (scheme)
          (melt srfi match)
          (melt parser sxml)
          (melt utils)
		  (melt lib sxml)
		  (melt lib file)
          (melt asset)
		  (melt renderer)
          (melt structure))
  
  (import type-page)

  (define (create-page meta cont comt)
	(make-page meta cont comt))

  (define (compose page renderer-list)
	(let ((generate-sxml (page-cont page)))
	  (generate-sxml page renderer-list)))

  (define (page-list-query key page-list)
	(if (null? page-list)
		#f
		(if (eq? key (page-meta (car page-list)))
			(car page-list)
			(page-list-query key (cdr page-list)))))
  
  ;; convert the sxml to html and write it to a file
  (define (create-writer output-file-name)
    (lambda (sxml)
      (let ((port (open-output-file output-file-name 'replace)))
        (sxml->html sxml port)
        (close-output-port port))))
  
  )
