#!chezscheme
(library
  (melt ui)
  (export melt)
  (import (scheme)
          (melt lib console)
          (melt version)
          (melt command)
          (melt glob)
          (melt cell)
          (melt srfi match))

  (define (show-version)
    (gemd:info (string-append
                 (gem:text "[37;1m" "melt")
                 (gem:text "[38;5;15m" " version ")
                 (gem:text "[38;5;165m" "0.0.5-2")) ))

  ;; the basic information
  (define (introduction)
    (gem:display (gem:text "[37m" "This is melt! Meta Excellent Local Note System.\n")
                 (gem:text "[37m" "Please use \"-h\" or \"--help\" to get further help.\nFor more information please follow")
                 (gem:text "[36m" " github io page.\n"))
    (gemd:info "github blog https://haxpeta.github.io/Lago"))

  ;; basic help information
  (define (help)
    (gem:display (gem:text "[37;1m" "melt ")
                 (gem:text "[38;5;102m" "[options] [command] [command options] \n"))
    (gem:display (gem:text "[38;5;80m" "available options are :")
                 (gem:text "[38;5;111m" " -h | -v | -vs | -l\n")))

  ;; to structure commands
  (define prepare
    (make-cell
      #t
      (lambda (value)
        (let ((melt-load (make-cell ".melt/config.scm"
                                    (lambda (x)
                                      (if (file-regular? x)
                                          (begin (load x) #t)
                                          (begin
                                            (gemd:error (gem:text "[38;5;222m" "melt configure file doesn't exist!"))
                                            #f))))))
          (if value
              (if (melt-load)
                  (begin
                    (gemd:info "Available commands :")
                    (inter:show-commands)
                    (user:show-commands))
                  (begin
                    (gemd:info "Available commands :")
                    (inter:show-commands)))
              (melt-load))))))

  (define melt-scan (make-cell ".melt" (lambda (value) (if (file-exists? value)
                                                           #t
                                                           (begin (gemd:error (gem:text "[38;5;196m" "You are not in the root of working directory!")) #f)))))

  ;; user interface
  (define (melt self . extra-args)
    (match extra-args
           [(or ("-h") ("--help"))
            (help)
            (exit 0)]
           [(or ("-v") ("--version"))
            (show-version)
            (exit 0)]
           [(or ("-vs") ("--version-history"))
            (show-version-history)
            (exit 0)]
           [(or ("-l") ("--list"))
            (begin (prepare #t) (prepare))
            (exit 0)]
           ['()
            (introduction)
            (exit 0)]
           [else (gemd:info "searching command ...")])

    (melt-scan)
    (prepare #f)
    (prepare)

    (let ((inter-command (inter:command-query (string->symbol (car extra-args))))
          (user-command (user:command-query (string->symbol (car extra-args)))))
      (cond
        [inter-command
          (apply inter-command (cdr extra-args))]
        [user-command
          (apply user-command (cdr extra-args))]
        [else
          (gemd:error (gem:text "[38;5;99m" "Command not available!"))
          (begin (prepare #t) (prepare))])))

  )
