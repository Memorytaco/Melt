(library (melt post)
  (export parse-post
          post-ref)
  (import (scheme)
          (melt structure)
          (melt utils)
          (melt parser parser))
  
  (import type-post)
  (import type-parser)
  
  
  ;; read one post and return post object
  (define (parse-post file-name parser)
    (let-values (((attr-data content) ((parser-proc parser) file-name)))
	  (make-post `((src-path . ,file-name)) attr-data content)))
  
  (define (post-ref key post)
	(cdr (assq key (post-attr post))))
  )
