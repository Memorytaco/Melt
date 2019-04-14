(library
  (melt config)
  (export config-update-option!
          config-add-option!
          config-value-query
          config-options-query)
  (import (scheme)
          (melt data))

  ;; update one option or add the option with default value
  (define (config-update-option! key value data)
    (let ((para (data-guard-query key data)))
      (if para
          (para value)
          (config-add-option! key value data))))

  ;; add one option with default value
  (define (config-add-option! key value data)
    (update-data! (create-data '(key) (list value)) data))

  ;; return the value of #f if it doesn't exist
  (define (config-value-query key data)
    (data-value-query key data))

  ;; if no arg provided, return the whole options list
  ;; if provide a key(option), return #t if exists and #f otherwise
  (define (config-options-query data)
    (lambda para
      (cond
        [(null? para)
         (data-keys data)]
        [(symbol? (car para))
         (data-value-query (car para) data)])))

  )
