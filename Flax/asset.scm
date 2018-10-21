(define-module (Flax asset)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9) ;;record type
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (Flax utils)
  #:export (make-asset
            is-asset?
            get-asset-source
            get-asset-target
            
	    cp-asset
	    cp-file-tree))

(define-record-type <asset>
  (make-asset source target)
  is-asset?
  (source get-asset-source)
  (target get-asset-target))

(define (cp-asset obj)
  (let ((src-assets (get-asset-source obj))
	(target-directory (get-asset-target obj)))
    (cp-file-tree src-assets target-directory)))

;; return a list of ~nth~ same element
(define (repeat-element-of-list nth element)
  (if (= 0 nth)
      '()
      (cons element (repeat-element-of-list (- nth 1) element))))

;; src is the resource directory
;; prefix is absolute path where the source will go in
;; the organization is like dest-prefix/src
(define (file-tree-move src-tree src-path prefix)
  (if (not (pair? src-tree))
      (let ((src-file-name (string-append src-path "/" src-tree))
            (dest-file-path (string-append prefix "/" src-tree)))
        (if (is-directory? src-file-name)
            (mkdir-p dest-file-path)
            (copy-file src-file-name dest-file-path)))
      (begin (mkdir-p (string-append prefix "/" (car src-tree)))
             (map file-tree-move (cdr src-tree)
                  (repeat-element-of-list (length (cdr src-tree))
                                          (string-append src-path "/" (car src-tree)))
                  (repeat-element-of-list (length (cdr src-tree))
                                          (string-append prefix "/" (car src-tree)))))))
;;
(define (cp-file-tree src prefix)
  (mkdir-p (get-absolute-path prefix))
  (file-tree-move (get-file-tree-list src)
                  (dirname (get-absolute-path src))
                  (get-absolute-path prefix)))
