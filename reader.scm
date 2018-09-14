;; TODO: unfinished files

(define-module (Flax reader)
  #:use-module (Flax utils)
  #:use-module (Flax post)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (sxml simple)

  #:export (make-reader
            is-reader?
            get-reader-matcher
            set-reader-matcher-as
            get-reader-proc
            is-reader-available?
            read-post
            read-posts
            
            make-file-extension-matcher
            sxml-reader
            html-reader))


;; ~matcher~ is a function to judge whether the file is supported
;; by this reader
;; ~proc~ is the processor function which process the supported file
(define-record-type <reader>
  (make-reader matcher proc)
  is-reader?
  (matcher get-reader-matcher set-reader-matcher-as)
  (proc get-reader-proc))

;; return the function which returns #t if the file is
;; end with ".~ext~"
(define (make-file-extension-matcher ext)
"return the compiled regexp"
  (let ((regexp (make-regexp (string-append "\\." ext "$"))))
    (lambda (file-name)
      (regexp-match? (regexp-exec regexp file-name))))


;; return #t if the ~file-name~ is supported by ~reader~
(define (is-reader-available? reader file-name)
  ((get-reader-matcher reader) file-name))


;; read-post
(define* (reader file-name #:optional (default-metadata '()))
  (let-values (((metadata sxml) ((get-reader-proc reader) file-name)))
    (make-post file-name cc (append metadata default-metadata) sxml)))

;;;;;;;;;;;        ;;;;;;;;;;;
;;;; define simple reader  ;;;
;;;;;;;;;;;        ;;;;;;;;;;;
;; sxml-reader
(define sxml-reader
  (make-reader (make-file-extension-matcher "sxml")
               ((lambda (file-name) 
                  (let ((contents (load (absolute-file-name file-name))))
                    (values (alist-delete 'content contents eq?)
                    (assq-ref contents 'content)))))))

;; html reader
(define (read-html-post port)
  (values (read-metadata-headers port)
          (match (xml->sxml port)
            (('*TOP* sxml) sxml))))

(define html-reader
  (make-reader (make-file-extension-matcher "html")
               (cut call-with-input-file <> read-html-post)))
