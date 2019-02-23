(library (melt data)
  (export create-data
		  update-data!)
  (import (scheme)
		  (melt structure)
		  (melt utils))

  (import type-data)

  ;; keys are a list symbols
  ;; values are corresponded to the keys
  (define create-data
    (lambda args
	  (cond
	   [(null? args)
		(make-data '()
				   '(() . ()))]
	   [else
		((lambda (keys values)
		   (make-data keys (map cons keys values)))
		 (car args)
		 (car (cdr args)))])))

  ;; it receive three types of args: data-type, alist, keys list.
  ;; first two is used to update the data
  ;; the third is used to delete the data
  (define update-data!
	(lambda (data arg)
	  (cond
	   [(data? arg)
		(do ((keys (data-keys arg) (cdr keys))
			 (alist (data-cont arg)))
			((null? keys) (data-cont data))
		  (if (memv (car keys) (data-keys data))
			  (begin
				(update-data! data `(,(car keys)))
				(update-data! data `(,(assq (car keys)
											alist))))
			  (begin
				(data-keys-set! data (cons (car keys)
										   (data-keys data)))
				(data-cont-set! data (cons (assq (car keys) alist)
										   (data-cont data))))))]
	   [(alist? arg)
		(if (not (null? arg))
			(update-data! data (create-data (map car arg)
											(map cdr arg))))]
	   [(list? arg)
		(do ((keys arg (cdr keys)))
			((null? keys) (data-cont data))
		  (data-cont-set! data (remove (assq (car keys)
											 (data-cont data))
									   (data-cont data)))
		  (data-keys-set! data (remove (car keys) (data-keys data))))]
	   [else (display "Didn't do anything!\n")])))
    
  )
