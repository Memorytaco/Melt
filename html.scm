;; convert sxml to html
;; NOTE : done
(define-module (Flax html)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:export (sxml->html
            sxml->html-string))

(define %void-elements
  '(area
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
    wbr))
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
  "Write the HTML escaped form of S to PORT."
  (define (escape c)
    (let ((escaped (hash-ref %escape-chars c)))
      (if escaped
          (format port "&~a;" escaped)
          (display c port))))
  (string-for-each escape S))

(define (object->escaped-html obj port)
  "Write the HTML escaped form of OBJ to PORT."
  (string->escaped-html
   (call-with-output-string (cut display obj <>))
   port))

(define (attribute-value->html value port)
  "Write the HTML escaped form of VALUE to PORT."
  (if (string? value)
      (string->escaped-html value port)
      (object->escaped-html value port)))

(define (attribute->html attr value port)
  "Write ATTR and VALUE to PORT."
  (format port "~a=\"" attr)
  (attribute-value->html value port)
  (display #\" port))

(define (element->html tag attrs body port)
  "Write the HTML TAG to PORT, where TAG has the attributes in the
list ATTRS and the child nodes in BODY."
  (format port "<~a" tag)
  (for-each (match-lambda
             ((attr value)
              (display #\space port)
              (attribute->html attr value port)))
            attrs)
  (if (and (null? body) (void-element? tag))
      (display " />" port)
      (begin
        (display #\> port)
        (for-each (cut sxml->html <> port) body)
        (format port "</~a>" tag))))

(define (doctype->html doctype port)
  (format port "<!DOCTYPE ~a>" doctype))

(define* (sxml->html tree #:optional (port (current-output-port)))
  "Write the serialized HTML form of TREE to PORT."
  (match tree
    (() *unspecified*)
    (('doctype type)
     (doctype->html type port))
    (((? symbol? tag) ('@ attrs ...) body ...)
     (element->html tag attrs body port))
    (((? symbol? tag) body ...)
     (element->html tag '() body port))
    ((nodes ...)
     (for-each (cut sxml->html <> port) nodes))
    ((? string? text)
     (string->escaped-html text port))
    ;; Render arbitrary Scheme objects, too.
    (obj (object->escaped-html obj port))))

(define (sxml->html-string sxml)
  "Render SXML as an HTML string."
  (call-with-output-string
   (lambda (port)
     (sxml->html sxml port))))
