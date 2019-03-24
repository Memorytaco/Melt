(library (melt command serve)
         (export serve)
         (import (scheme)
                 (melt structure))

         (import type-command)

		 (define (serve-help)
       (display "the command is not ready yet\n"))

     (define serve-cli
       (lambda args
         (cond
           [(null? args)
            (display "the command is not ready yet\n")]
           [else
             (display "the command is not ready yet\n")])))

     (define serve
       (make-command 'serve
                     "melt server to preview your site"
                     serve-cli))

         )
