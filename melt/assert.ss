#!chezscheme
(library
  (melt assert)
  (export mmessage
          massert)
  (import (scheme))

  ;; there are three level errors
  ;; 'error 'warn 'fatal

  ;; 'error: the input is not correct, but the program will continue
  ;; 'warn: the input may induce some error
  ;; 'fatal: the program will breakdown and quit

  ;; error format:
  ;;        <error-type>: <error-message> --> <arg-position> :<error-input> || <expect-input>
  ;; warn format:
  ;;        <warn-type>: <warn-message>
  ;; fatal format:
  ;;        <fatal-type>: <fatal-message> <fatal-stack>


  )
