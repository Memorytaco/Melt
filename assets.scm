(define-module (Flax asset)
               #:use-module (srfi srfi-1)
               #:use-module (ice-9 match)
               #:use-module (Flax utils)
               #:export (create-asset
                         is-asset?
                         asset-source
                         asset-target
                         install-asset
                         directory-assets))


(define-record-type <asset>
  (create-asset source target)
  is-asset?
  (source get-asset-source)
  (target get-asset-target))

(define cp-asset
  (lambda (asset prefix)
    (match asset
      (($ <asset> source target)
       (let ((target* (string-append prefix "/" target)))
         (mkdir-p (dirname target*))
         (copy-file source target*))))))

(define directory-assets
  (lambda (directory keep? destination)
    (define enter? (const #t))
    ;; Convert every file name to a list of components
    ;; manipulated then converted back into a string
    (define leaf
      (let ((base-lenth (length (decompose-file-name directory)))
            (dest* (decompose-file-name destination)
