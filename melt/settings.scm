(library (melt settings)
  (export settings-update!
		  settings-query)
  (import (scheme)
		  (melt glob)
		  (melt data))

  ;; accept an alist or single list as argument
  (define (settings-update! a-list)
	(update-data! %%settings a-list))

  (define (settings-query key)
	(data-value-query key %%settings))
  )
