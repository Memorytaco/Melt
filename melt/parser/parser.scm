(library (melt parser parser)
  (export parse
		  create-parser)
  (import (scheme)
          (melt structure)
          (melt utils))
  
  (import type-parser)

  (define (parse file parser)
	(let ((filt (make-filter (symbol->string (parser-type parser)))))
	  (display "not ready yet\n")))

  ;; make file extension matcher
  ;; return the function which returns #t if the file's
  ;; extension is ext
  (define make-filter
    (lambda (ext)
      (lambda (path)
        (string=? ext (path-extension path)))))

  ;; please use this function instead of make-parser
  (define create-parser
	(case-lambda
	  [(type proc refp)
	   (if (symbol? type)
		   (make-parser type proc refp))]
	  [(type proc)
	   (create-parser type proc (lambda () (display "Do nothing\n")))]))
  
  
  )
