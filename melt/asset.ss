(library (melt asset)
  (export asset-cp
          asset-list-cp)
  (import (scheme)
          (melt lib file)
          (melt structure))
  
  (import type-asset)
  
  ;; for single object
  ;; copy the src file to the target-directory
  (define (asset-cp asset-obj)
    (let ((src-directory (asset-source asset-obj))
          (target-directory (asset-target asset-obj)))
      (cp-rf src-directory
             (string-append target-directory
                            (directory-separator-string)
                            src-directory))))
  
  (define (asset-list-cp asset-list)
    (map asset-cp asset-list))
  
  )
