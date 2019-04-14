(library
  (melt command serve)
  (export serve-cli)
  (import (scheme)
          (melt lib console)
          (melt command))

  (define (serve-help)
    (gemd:info "the command is not ready yet"))

  (define serve-command
    (lambda args
      (cond
        [(null? args)
         (gemd:info "the command is not ready yet")]
        [else
          (gemd:info "the command is not ready yet")])))

  (define serve-cli
    (create-command 'serve
                    "melt server to preview your site."
                    serve-command))

  )
