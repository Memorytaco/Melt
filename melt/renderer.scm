(library
  (melt renderer)
  (export create-renderer
          update-renderer-data!)
  (import (scheme)
          (melt structure)
          (melt invoke)
          (melt data))

  (import type-renderer)

  (define create-renderer
    (case-lambda
      [(type proc)
       (make-renderer type proc (create-data))]
      [(type proc data)
       (make-renderer type proc data)]))

  (define update-renderer-data!
    (lambda (renderer keys data)
      (if (renderer? renderer)
          (update-data! (renderer-data renderer)
                        data)
          (error 'renderer "in update-renderer-data! : renderer must be a renderer type"))))

  )
