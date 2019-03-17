(library (melt glob parser)
  (export sxml-parser
		  commonmark-parser)
  (import (scheme))

  (module sxml-parser
		  [sxml-parser]
		  
		  (import (melt asset))
		  (import (melt lib file))
		  (import (melt utils))
		  (import (melt parser))
		  
		  ;; define default sxml parser
		  (define sxml-parser
            (create-parser 'sxml
                           (lambda (file-name)
							 (let ((contents ((lambda (file-name)
												(define fasl (string-append (cd) ".melt/cache" (directory-separator-string) (basename file-name)))
												(define port (if (file-exists? fasl)
																 (open-file-input-port fasl)
																 (begin
																   (mkdir-r (string-append (cd) ".melt/cache"))
																   (fasl-file file-name fasl)
																   (open-file-input-port fasl))))
												(define content (eval (fasl-read port)))
												(close-input-port port)
												content) file-name)))
                               (list (alist-delete 'content contents)
                                     (cdr (assq 'content contents))))))))

  (module commonmark-parser
		  [common-mark]

		  (define common-mark "hello"))

  )
