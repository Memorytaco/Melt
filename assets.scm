;; This is used to cp all resource to destination
(define-module (Flax asset)
               #:use-module (srfi srfi-1)
               #:use-module (ice-9 match)
               #:use-module (Flax utils)
               #:export (make-asset
                         is-asset?
                         get-asset-source
                         get-asset-target
                         cp-asset
                         directory-assets))


(define-record-type <asset>
  (make-asset source target)
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
            (dest* (decompose-file-name destination)))
          (lambda (file-name stat memo)
            (if (keep? file-name)
                (let* ((file-name* (file-name-components file-name))
                       (target (join-file-name-components (append dest* (drop file-name* base-lenth)))))
                  (cons (make-asset file-name target) memo))
                memo))))
    (define (noop file-name stat memo) memo)
    (define (err file-name stat errno memo)
      (error "asset processing failed with errno: " file-name errno))

    (file-system-fold enter? leaf noop noop noop err '() directory)))
