(library
  (melt command)
  (export create-command
          command-add!
          command-remove!
          command-proc
          command-desc
          command-name
          command-query
          show-commands)
  (import (scheme)
          (melt data)
          (melt lib console))

  ;; command itself is a data
  ;; symbol, which specifics string in command line
  ;; string, one line description
  ;; procedure for the command, accept arguments
  (define (create-command name desc proc)
    (create-data '(name desc proc)
                 `(,name ,desc ,proc)))

  (define (command-name command)
    (data-value-query 'name command))
  (define (command-desc command)
    (data-value-query 'desc command))
  (define (command-proc command)
    (data-value-query 'proc command))

  ;; add one command to command data
  (define (command-add! command data)
    (update-data! (create-data (list (command-name command))
                               (list command))
                  data))

  ;; remove one command in command data
  (define (command-remove! command data)
    (update-data! (list (command-name command)) data))

  ;; query command in a data
  (define (command-query name data)
    (data-value-query name data))

  ;; display command names and its description
  (define (show-commands command-data)
    (define (show-command command)
      (format #t "  ~8a>>>   ~a~%" (symbol->string (command-name command)) (command-desc command)))

      ; (gem:format "  ~10@a-->~10a~%"
      ;             `(("[37;1m" . ,(symbol->string (command-name command)))
      ;               ("[38;5;166m" . ,(command-desc command)))))

    (do ((command-list (data-values command-data) (cdr command-list)))
      ((null? command-list) #t)
      (show-command (car command-list))))

  )
