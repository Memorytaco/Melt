(library
  (melt post)
  (export post-meta-query
          post-attr-query
          create-post)
  (import (scheme)
          (melt structure)
          (melt utils)
          (melt data))

  (import type-post)

  ;; accept two assoc list one sxml tree
  (define create-post
    (case-lambda
      [(meta attr cont)
       (make-post (create-data meta) (create-data attr) cont)]))

  ;; query the data in post and return the value if exists
  (define (post-meta-query key post)
    (data-value-query key (post-meta post)))

  (define (post-attr-query key post)
    (data-value-query key (post-attr post)))

  )
