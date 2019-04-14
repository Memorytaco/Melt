(library
  (melt command init)
  (export init-cli)
  (import (scheme)
          (melt command)
          (melt cell)
          (melt lib console))

  (define (init-help)
    (gemd:help (string-append
                 (gem:text "[37;1m" "melt")
                 (gem:text "[38;5;67m" " init ")
                 (gem:text "[38;5;253m" "name")))
    (gemd:help (string-append
                 "if the"
                 (gem:text "[38;5;253m" " name ")
                 "is not provided, it will generate .melt in working directory")))

  ;; create some files
  (define (initprocedure)
    (mkdir ".melt")
    (mkdir "public")
    (mkdir "post")
    (system "touch .melt/config.scm")
    (system "touch .melt/melt.scm")
    (mkdir ".melt/fasl")
    (mkdir ".melt/resource"))

  ;; interface to be invoked in ui
  (define command-init
    (lambda args
      (cond
        [(null? args)
         (init-help)]
        [(eq? (length args) 1)
         (let ((target (make-cell (car args) (lambda (value) (if (file-exists? value)
                                                                 (begin (gemd:error "file exists!") #f)
                                                                 value)))))
           (if (target)
               (begin (target (target) (lambda (value) value)) (mkdir (target)) (cd (target)) (initprocedure))))]
        [else (gemd:info "only one argument required.")])))

  (define init-cli
    (create-command 'init
                    "init command to generate basic settings."
                    command-init)))
