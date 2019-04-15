(library
  (melt uutil)
  (export parse-posts
          get-md-attr
          get-md-title
          get-md-post-title-list
          scone!)
  (import (scheme)
          (melt post)
          (melt lib file)
          (melt lib console)
          (melt parser markdown)
          (melt data)
          (melt utils)
          (melt config)
          (melt parser))

  (define-syntax scone!
    (syntax-rules ()
      [(_ ls item)
       (set! ls (append ls (if (not (null? item)) (list item) '())))]))

  ;; parse a directory and return a list of posts
  (define (parse-posts path parser-list)
    (let ((dir-ls (directory-list path))
          (post-ls '()))
      (do ((obj-ls (map string-append
                        (make-list (length dir-ls) (string-append path (directory-separator-string)))
                        dir-ls)
                   (cdr obj-ls)))
        ((null? obj-ls) (flatten post-ls))
        (if (file-directory? (car obj-ls))
            (scone! post-ls (parse-posts (car obj-ls) parser-list))
            (let ((raw-sxml (parse-with-parsers (car obj-ls) parser-list)))
              (if raw-sxml
                  (scone! post-ls (create-post
                                    `((path . ,(car obj-ls))
                                      (name . ,(path-last (path-root (car obj-ls)))))
                                    (car raw-sxml)
                                    (car (cdr raw-sxml))))))))))

  ;; return the assoc list of a markdown post
  (define (get-md-attr file-name)
    (call-with-input-file file-name
                          (lambda (port)
                            (parse-markdown-attr port))))

  ;; return the markdown post title, title is a string
  (define (get-md-title file-name)
    (cdr (assq 'title (get-md-attr file-name))))

  ;; accept a directory and return pairs like (file-name . post-title)
  (define (get-md-post-title-list pdirectory)
    (let ((post-title-list '())
          (items (directory-list pdirectory)))
      (do ((files (map string-append
                       (make-list (length items) (string-append pdirectory "/"))
                       items)
                  (cdr files)))
        ((null? files) post-title-list)
        (if (file-regular? (car files))
            (scone! post-title-list `(,(path-root (path-last (car files))) . ,(get-md-title (car files))))))))



  )
