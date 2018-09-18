;; NOTE : done

(define-module (Flax reader)
  #:use-module (srfi srfi-1) ;;alist-delete procedure
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (sxml simple)

  #:use-module (Flax utils)

  #:export (make-reader
            is-reader?
            get-reader-matcher
            set-reader-matcher-as
            get-reader-proc
            is-reader-available?

            make-file-extension-matcher
            sxml-reader))


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
      (regexp-match? (regexp-exec regexp file-name)))))


;; return #t if the ~file-name~ is supported by ~reader~
(define (is-reader-available? reader file-name)
  ((get-reader-matcher reader) file-name))

;;;;;;;;;;;        ;;;;;;;;;;;
;;;; define simple reader  ;;;
;;;;;;;;;;;        ;;;;;;;;;;;
;; sxml-reader
;; take the file as the scheme code and then read it
(define sxml-reader
  (make-reader (make-file-extension-matcher "sxml")
               (lambda (file-name)
                  (let ((contents (load (get-absolute-path file-name))))
                    (values (alist-delete 'content contents eq?) ;; return the metadata list
                            (assq-ref contents 'content))))))    ;; return the list
