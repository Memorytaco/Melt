;; TODO: unfinished files

(define-module (Flax reader)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (sxml simple)
  #:use-module (Flax utils)
  #:use-module (Flax post)

  #:export (make-reader))

(define-record-type <reader>
  (make-reader matcher proc)
  is-reader?
  (matcher get-reader-matcher)
  (proc get-reader-proc))


(define (is-reader-available? reader file-name)
  "return #t if ~file-name~ is supported by ~reader~"
  ((get-reader-matcher reader) file-name))

(define* (read-post reader file-name #:optional
                                      (default-metadata'()))
    (let-values))
