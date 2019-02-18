#!chezscheme
;; convert sxml to html
(library (melt parser sxml)
         (export sxml->html
                 sxml->html-string)
         (import (scheme)
                 (melt utils)
                 (melt srfi match))


         (define %void-elements
           '[area
              base
              br
              col
              command
              embed
              hr
              img
              input
              keygen
              link
              meta
              param
              source
              track
              wbr])

         ;; pair? if arg is atom ,return #t,else if list return #t
         (define (void-element? tag)
           "Return #t if TAG is a void element."
           (pair? (memq tag %void-elements)))

         ;; something like '<' -> &le;
         ;; This need to be update from time to time
         ;; --- mark ---
         (define %escape-chars
           (alist->hash-table
             '((#\" . "quot")
               (#\& . "amp")
               (#\< . "lt")
               (#\> . "gt"))))

         (define (string->escaped-html S port)
           ;Write the HTML escaped form of S to PORT.
           (define (escape c)
             (let ((escaped (hash-ref %escape-chars c)))
               (if escaped
                   (format port "&~a;" escaped)
                   (display c port))))
           (string-for-each escape S))

         (define (object->escaped-html obj port)
           ; Write the HTML escaped form of OBJ to PORT.
           (string->escaped-html
             (call-with-string-output-port
               (lambda (x)
                 (display obj x)))
             port))

         (define (attribute-value->html value port)
           ; Write the HTML escaped form of VALUE to PORT.
           (if (string? value)
               (string->escaped-html value port)
               (object->escaped-html value port)))

         (define (attribute->html attr value port)
           "Write ATTR and VALUE to PORT."
           (format port "~a=\"" attr)
           (attribute-value->html value port)
           (display #\" port))

         (define (element->html tag attrs body port)
           ; Write the HTML TAG to PORT, where TAG has the attributes in the
           ; list ATTRS and the child nodes in BODY.
           (format port "<~a" tag)
           (for-each (match-lambda
                       ((attr value)
                        (display #\space port)
                        (attribute->html attr value port)))
                     attrs)
           (if (and (null? body) (void-element? tag))
               (display " />\n" port)
               (begin
                 (display #\> port)
                 (for-each (lambda (x)
                             (sxml->html x port)) body)
                 (format port "</~a>\n" tag))))

           (define (doctype->html doctype port)
             (format port "<!DOCTYPE ~a>\n" doctype))

           (define (sxml->html tree port)
             ;Write the serialized HTML form of TREE to PORT.
             (match tree
                    (() 'unspecified)
                    (('doctype type)
                     (doctype->html type port))
                    (((? symbol? tag) ('@ attrs ...) body ...)
                     (element->html tag attrs body port))
                    (((? symbol? tag) body ...)
                     (element->html tag '() body port))
                    ((nodes ...)
                     (for-each (lambda (x)
                                 (sxml->html x port)) nodes))
                    ((? string? text)
                     (string->escaped-html text port))
                    ;; Render arbitrary Scheme objects, too.
                    (obj (object->escaped-html obj port))))

           (define (sxml->html-string sxml)
             ; Render SXML as an HTML string.
             (call-with-string-output-port
               (lambda (port)
                 (sxml->html sxml port))))

           )
