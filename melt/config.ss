(library
  (melt config)
  (export config-update-option!
          melt:config-options-query)
  (import (scheme)
          (melt data))

  ;; add one option with default value
  (define (config-update-option! key value data)
    (update-data! (create-data (list key) (list value)) data))

  ;; if no arg provided, return the whole options list
  ;; if provide a key(option), return the value if exists and #f otherwise
  (define (melt:config-options-query data)
    (lambda para
      (cond
        [(null? para)
         (data-keys data)]
        [(symbol? (car para))
         (data-value-query (car para) data)])))

  )
