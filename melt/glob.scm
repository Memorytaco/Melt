#!chezscheme
(library (melt glob)
  (export %%chain
		  %%settings)
  (import (scheme)
		  (melt structure)
		  (melt invoke)
		  (melt data)
		  (melt asset))

  (import type-chain)
  (import type-hook)
  (import type-site)

  (define %%chain (init-chain #t
							  (lambda () (display "Building ...\n"))
							  (create-data)))

  (define %%settings (create-data))
  )
