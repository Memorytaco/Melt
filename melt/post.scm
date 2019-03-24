(library (melt post)
  (export post-meta-query
		  post-attr-query
		  create-post)
  (import (scheme)
          (melt structure)
          (melt utils)
          (melt data))
  
  (import type-post)
  (import type-data)

  ;; accept two assoc list
  (define create-post
	(case-lambda
	  [(meta attr cont)
     (make-post meta attr cont)]))
  
  ;; query the data in post and return the key value pair if exists
  (define (post-meta-query key post)
	(assq key (data-cont (post-meta post))))
  
  (define (post-attr-query key post)
	(assq key (data-cont (post-attr post))))
  
  )
