(library (melt parser)
  (export parse
		  create-parser)
  (import (scheme)
          (melt structure)
		  (melt lib console)
          (melt utils))
  
  (import type-parser)

  ;; parser procedure for using parser to parse soucre file
  ;; and use refine procedure to update source file
  (define (parse file-path parser)
	(let ((filt (make-filter (symbol->string (parser-type parser)))))
	  (if (filt file-path)
		  (let ((proc (parser-proc parser))
				(refine (parser-refp parser)))
			(define returned-value (proc file-path))
			(if refine (refine file-path))
			returned-value)
		  #f)))

  ;; make file extension matcher
  ;; return the function which returns #t if the file's
  ;; extension is ext
  (define make-filter
    (lambda (ext)
	  (if (string? ext)
		  (lambda (path)
			(string=? ext (path-extension path)))
		  (gem-display (gem "[38;5;160m" "error: ")
					   (gem "[38;5;112m" "in (melt parser): make-filter")
					   "ext must be string!\n"))))

  ;; please use this function instead of make-parser
  (define create-parser
	(case-lambda
	  [(type proc refp)
	   (if (symbol? type)
		   (make-parser type proc refp)
		   (begin
			 (gem-display (gem "[38;5;160m" "error: ")
						  (gem "[38;5;112m" "in (melt parser): create-parser")
						  "type must be symbol!\n")
			 (exit 1)))]
	  [(type proc)
	   (make-parser type proc #f)]))
  
  
  )
