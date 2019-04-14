(library
  (melt parser markdown)
  (export markdown
          parse-markdown-attr)
  (import (scheme)
          (melt parser)
          (melt lib markdown)
          (melt lib sxml)
          (melt lib string))

  (define (parse-markdown-attr port)
    (define (get-line-attrs port)
      (if (pattern-match '(#\~ #\~ #\~ #\newline) port)
          (begin (char-forward port 4) '())
          (let ((str-ls (string-split (get-line port) #\:)))
            ;; this is for parsing date
            (if (eq? (length str-ls) 4)
                (cons (string->symbol (string-trim (car str-ls) 'both))
                      (string-trim (string-append (list-ref str-ls 1) ":"
                                                  (list-ref str-ls 2) ":"
                                                  (list-ref str-ls 3))
                                   'both))
                (cons (string->symbol (string-trim (car str-ls) 'both))
                      (string-trim (apply string-append (cdr str-ls)) 'both))))))

    (if (pattern-match '(#\~ #\~ #\~ #\newline) port)
        (begin (char-forward port 4)
               (do ((cur-attr (get-line-attrs port) (get-line-attrs port))
                    (sxml '()))
                 ((null? cur-attr) sxml)
                 (set! sxml (scone sxml cur-attr))))
        (begin (read-char port)
               (parse-markdown-attr port))))

  (define markdown
    (create-parser 'md
                   (lambda (file-name)
                     (call-with-input-file
                       file-name
                       (lambda (input-port)
                         (let* ((attrs (parse-markdown-attr input-port))
                                (contents (markdown->sxml input-port)))
                           (list attrs contents)))))))
  )
