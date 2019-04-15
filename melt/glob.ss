#!chezscheme
(library
  (melt glob)
  (export %%invocation
          %%user-commands
          %%builtin-commands
          %%config
          user:command-add!
          user:show-commands
          user:command-query
          inter:command-add!
          inter:command-query
          inter:show-commands

          user:config-options-query
          user:config-update-option!)
  (import (scheme)
          (melt invoke)
          (melt data)
          (melt config)
          (melt command))

  ;; /////config //////
  (define %%config (create-data))

  (define user:config-options-query
    (melt:config-options-query %%config))

  (define (user:config-update-option! key value)
    (config-update-option! key value %%config))

  (define %%invocation (create-invocation))

  ;; define global data
  (define %%builtin-commands (create-data))
  (define %%user-commands (create-data))

  (define (user:command-add! command)
    (command-add! command %%user-commands))

  (define (user:command-query name)
    (let ((command (data-value-query name %%user-commands)))
      (if command
          (command-proc command)
          #f)))

  (define (user:show-commands)
    (show-commands %%user-commands))

  ;; ---------------define inter command
  (define (inter:command-add! command)
    (command-add! command %%builtin-commands))

  (define (inter:command-query name)
    (let ((command (data-value-query name %%builtin-commands)))
      (if command
          (command-proc command)
          #f)))

  (define (inter:show-commands)
    (show-commands %%builtin-commands))

  ;; ======= inter command import
  (import (melt command init))
  (import (melt command invoke))
  (import (melt command serve))
  (import (melt command post))

  (inter:command-add! init-cli)

  (inter:command-add! (invoke-cli %%invocation))

  (inter:command-add! serve-cli)

  (inter:command-add! (post %%config))

  )
