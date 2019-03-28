(library (melt parser markdown)
         (export markdown)
         (import (scheme)
                 (melt parser)
                 (melt lib markdown)
                 (melt lib sxml)
                 (melt utils))

         (define (parse-markdown-attr port)
           (define (get-attrs sxml port)
             (if (pattern-match '(#\~ #\~ #\~ #\newline) port)
                 (begin (char-forward port 4) sxml)
                 (get-attrs (scone sxml (cons (string->symbol
                                                (parse-key (list) port))
                                              (parse-value (list) port)))
                            port)))
           ;; following procedure returns a string
           (define (parse-key chars port)
             (if (pattern-match '(#\:) port)
                 (begin (char-forward port 1) (string-trim (apply string chars)
                                                               'both))
                 (parse-key (scone chars (read-char port)) port)))
           (define (parse-value chars port)
             (if (pattern-match '(#\newline) port)
                 (begin (char-forward port 1) (string-trim (apply string chars)
                                                               'both))
                 (parse-value (scone chars (read-char port)) port)))

           (if (pattern-match '(#\~ #\~ #\~ #\newline) port)
               (begin (char-forward port 4) (get-attrs (list) port))
               (read-char port)))

         (define markdown
           (create-parser 'md
                          (lambda (file-name)
                            (call-with-input-file file-name
                                                  (lambda (input-port)
                                                    (let* ((attrs (parse-markdown-attr input-port))
                                                           (contents (markdown->sxml input-port)))
                                                      (list attrs contents)))))))
         )
