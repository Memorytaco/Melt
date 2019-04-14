(library
  (melt command invoke)
  (export invoke-cli)
  (import (scheme)
          (melt invoke)
          (melt lib console)
          (melt lib file)
          (melt command)
          (melt utils))

  ;; display the build command usage
  (define (invoke-cli-help)
    (gemd:info
      (string-append
        (gem:text "[37;1m" "melt")
        (gem:text "[38;5;67m" " invoke")
        (gem:text "[38;5;253m" " <main-execute-file>\n")
        "       If the"
        (gem:text "[38;5;253m" " <main-execute-file> ")
        "is not provided, use"
        (gem:text "[38;5;190m" " melt.scm ")
        "instead.")))

  (define (invoke-cli global-invocation)
    (create-command 'invoke
                    "melt command to execute one list actions, mostly build the site."
                    (lambda args
                      (cond
                        [(eq? 1 (length args))
                         (let ((main-ex-file (car args)))
                           (if (file-exists? main-ex-file)
                               (begin
                                 (load main-ex-file)
                                 (invoke global-invocation))
                               (begin
                                 (gemd:error "Coundn't find config file !!")
                                 (gemd:error (string-append "expect " (basename main-ex-file) " but the file doesn't exist!")))))]
                        [else
                          (gemd:warn "please give me the melt.scm!")
                          (invoke-cli-help)]))))

  )
