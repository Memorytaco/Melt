(library
  (melt parser)
  (export parse
          parse-with-parsers
          create-parser)
  (import (scheme)
          (melt structure)
          (melt lib console)
          (melt utils))

  (import type-parser)

  ;; parser procedure for using parser to parse soucre file
  ;; and use refine procedure to update source file
  (define (parse path parser)
    (let ((filt (make-filter (symbol->string (parser-type parser)))))
      (if (filt path)
          (let ((proc (parser-proc parser))
                (refine (parser-refp parser)))
            (define returned-value (proc path))
            (if refine (refine path))
            returned-value)
          #f)))

  ;; almost same with parse but accept a parser list
  ;; rely on parse
  (define (parse-with-parsers path parser-list)
    (call/cc
      (lambda (cc)
        (do ((parser-ls parser-list (cdr parser-ls)))
          ((null? parser-ls) #f)
          (let ((re-value (parse path (car parser-ls))))
            (if re-value (cc re-value)))))))

  ;; make file extension matcher
  ;; return the function which returns #t if the file's
  ;; extension is ext
  (define make-filter
    (lambda (ext)
      (if (string? ext)
          (lambda (path)
            (string=? ext (path-extension path)))
          (gemd:error
            (string-append (gem:text "[38;5;112m" "in (melt parser): make-filter")
                           "ext must be string!")))))

  ;; please use this function instead of make-parser
  (define create-parser
    (case-lambda
      [(type proc refp)
       (if (symbol? type)
           (make-parser type proc refp)
           (begin
             (gemd:error (string-append (gem:text "[38;5;112m" "in (melt parser): create-parser")
                                        "type must be symbol!"))
             (exit 1)))]
      [(type proc)
       (make-parser type proc #f)]))


  )
