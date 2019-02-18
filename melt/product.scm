#!chezscheme
(library (melt product)
  (export sxml-parser
		  commonmark-parser)
  (import (scheme))
  
  (module sxml-parser
		  [sxml-parser]

		  (import (melt asset))
		  (import (melt lib file))
		  (import (melt utils))
		  (import (melt parser parser))
		  
		  ;; define default sxml parser
		  (define sxml-parser
            (create-parser 'sxml
                           (lambda (file-name)
							 (let ((contents ((lambda (file-name)
												(define fasl (string-append (cd) "/cache" (directory-separator-string) (basename file-name)))
												(define port (if (file-exists? fasl)
																 (open-file-input-port fasl)
																 (begin
																   (mkdir-r (string-append (cd) "/cache"))
																   (fasl-file file-name fasl)
																   (open-file-input-port fasl))))
												(define content (eval (fasl-read port)))
												(close-input-port port)
												content) file-name)))
                               (values (alist-delete 'content contents) ;; return the metadata list
                                       (cdr (assq 'content contents)))))))
		  
		  )


  (module commonmark-parser
          [commonmark-parser
		   register-metadata-parser!
		   read-metadata-headers
		   parse-metadata]

		  (import (melt parser parser))
		  (import (melt utils))
		  
		  ;; read meta data
		  (define %metadata-parsers
			(make-eq-hashtable))

		  (define (register-metadata-parser! name parser)
			(hashtable-set! %metadata-parsers name parser))

		  (define (get-metadata-parser key)
			(hashtable-ref %metadata-parsers key identity))

		  (define (parse-metadata key value)
			((get-metadata-parser key) value))
		  
		  (define (read-metadata-headers port)
			(define (string-trim-both string)
			  (string-trim string 'both))

			(display "in read metadata headers\n")
			(let ((metadata '()))
			  (do ((line (get-line port) (get-line port))
				   (end? #f))
				  ((or end? (eof-object? line))
				   (if (not end?)
					   (error (port-name port)
							  "end of file while readig metadata: " )))
				(cond
				 [(string=? line "---")
				  (set! end? #t)]
				 [else
				  (let-values (((key value) (values (car (map string-trim-both (string-split-dual line #\:)))
													(car (cdr (map string-trim-both (string-split-dual line #\:)))))))
					(set! metadata (alist-cons (string->symbol key) (parse-metadata (string->symbol key) value) metadata)))]))
			  metadata))



		  ;; for the temple commonmark
		  (define (commonmark->sxml port)
			(display "in commonmark->sxml\n\n")
			(do ((line (get-line port) (get-line port))
				 (strings '()))
				((eof-object? line) (display (apply string-append strings))
				 `((content . ,(apply string-append strings))))
			  (set! strings (append strings (list (string-append line "\n"))))))

		  ;; not available now!!
		  (define commonmark-parser
		    (create-parser 'md
						   (lambda (file)
		                     (call-with-input-file file
							   (lambda (port)
								 (values
								  (commonmark->sxml port)
								  (read-metadata-headers port)))))))

		  ;; two new parser
		  (register-metadata-parser!
		   'type
		   (lambda (str)
			 (string->symbol str)))

		  (register-metadata-parser!
		   'depth
		   (lambda (str)
			 (string->number str)))

		  )
  )
