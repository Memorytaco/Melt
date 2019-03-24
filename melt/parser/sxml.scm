(library (melt parser sxml)
  (export sxml-parser)
  (import (scheme)
		  (melt parser)
		  (melt lib sxml)
		  (melt utils)
		  (melt lib file))


  ;; the name is source path
  ;; dir is the target directory
  (define (store-fasl dir name target)
	(if (fasl-file name target)
		#t
		#f))

  ;; return the dir/name string
  (define (get-fasl-path dir name)
	(string-append dir (directory-separator-string) (basename name)))

  ;; return the readed data, make sure the source doesn't contain any real code
  (define (get-fasl path)
	(eval (fasl-read path)))

  
  (define sxml-parser
    (create-parser 'sxml
                   (lambda (file-name)
					 (let ((cache (get-fasl-path ".melt/cache" file-name)))
					   (if (not (file-exists? cache))
						   (store-fasl ".melt/cache" file-name cache))
					   (let ((contents (get-fasl cache)))
						 (list (alist-delete 'content contents)
							   (cdr (assq 'content contents))))))))

  
  )
