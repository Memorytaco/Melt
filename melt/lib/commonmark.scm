#!chezscheme
(library (melt lib commonmark)
  (export commonmark->sxml
		  split-paragraph
		  transform-token
		  tokenize-module)
  (import (scheme)
		  (melt utils))

  ;;-----------------------------------------------define record ###########
  ;; define the media type
  (define-record-type token
	(nongenerative melt-token)
	(fields
	 ;; type is a symbol
	 (immutable type token-type)
	 ;; attr is an list and each element is a list
	 ;; which has two elements
	 (immutable attr token-attr)
	 ;; the content is also a list
	 (immutable cont token-cont)))

  ;;-----------------------------------------------define key words ########
  ;; define special characters
  (define &&key-words (alist->hash-table
					   '((#\* . star)
						 (#\_ . under-score)
						 (#\- . minus)
						 (#\+ . plus)
						 (#\! . she)
						 (#\# . sharp)
						 (#\newline . newline)
						 (#\` . back-quote))))

  ;; transform one token to an sxml tag
  (define (transform-token token)
	  (define (attr-expand t-list)
		(if (null? t-list)
			'()
			(cons '@ t-list)))
	  (if (token-attr token)
		  (if (token-cont token)
			  `(,(token-type token) ,(attr-expand (token-attr token)) ,(token-cont token))
			  `(,(token-type token) ,(attr-expand (token-attr token))))
		  (if (token-cont token)
			  `(,(token-type token) ,(token-cont token))
			  (list (token-type token)))))
  
  ;; judge whether a line is a blank line
  ;; the line doesn't contain a newlne character
  (define (blank-line? line)
	(call/cc (lambda (cc)
			   (do ((char-list (string->list line) (cdr char-list)))
				   ((null? char-list) #t)
				 (let ((char (car char-list)))
				   (if (not
						(or (equal? #\tab char)
							(equal? #\space char)))
					   (cc #f)))))))
  
  ;; read one file, generate a list of paragraphs
  (define (split-paragraph path)
	(call-with-port
	 (open-file-input-port path (file-options) (buffer-mode block) (native-transcoder))
	 (lambda (port)
	   (define paragraphs '())
	   (do ((paragraph '() (append paragraph line))			
			(line (get-line port) (get-line port)))
		   ((eof-object? line)
			(set! paragraphs (append paragraphs (list paragraph)))
			paragraphs)
		 (if (blank-line? line)
			 (begin
			   (set! paragraphs (append paragraphs (list paragraph)))
			   (set! paragraph '())
			   (set! line '()))
			 (set! line (list line)))))))

  (module tokenize-module
		  (tokenize
		   )
		  
		  (define tokenize-p
			(let ((tokens '())
				  (temp-char-list '()))
			  (values (lambda (line-or-port)
						(call-with-port (if (port? line-or-port)
											line-or-port
											(open-string-input-port line-or-port))
										(lambda (port)
										  (do ((char (read-char port) (read-char port)))
											  ((eof-object? char) #t)
											(cond
											 [(equal? char #\#)
											  (append! temp-char-list (list char))])))))
					  (lambda ()
						(values tokens)))))

		  (define tokenize
			(lambda (paragraphs)
			  (do ((paragraphs paragraphs (cdr paragraphs))
				   (tokens (list)))
				  ((null? paragraphs) tokens)
				(set! tokens
					  (append tokens (list (make-token 'p #f (apply string-append (car paragraphs)))))))))

		  )
  
  
  
  (import tokenize-module)
  (define commonmark->sxml
	(lambda (path)
	  (define lines (split-paragraph path))
	  (define tokens (tokenize lines))
	  (display tokens)
	  (map transform-token tokens)))

  )
