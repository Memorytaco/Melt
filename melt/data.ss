(library
  (melt data)
  (export create-data
          update-data!
          data-keys
          data-values
          data-guards
          data->alist
          data-guard-query
          data-value-query)
  (import (scheme)
          (melt cell)
          (melt utils))

  ;; !!!!!! need to take use of gurd of cell

  ;; accept nothing or list of keys and list of values
  ;; keys are a list symbols
  ;; values are corresponded to the keys
  ;; or an assoc list
  (define create-data
    (lambda args
      (cond
        [(null? args)
         (make-cell '())]
        [(and (eq? 1 (length args)) (alist? (car args)))
         ((lambda (keys values)
            (make-cell (map cons keys (map make-cell values))))
          (map car (car args))
          (map cdr (car args)))]
        [(and (eq? 2 (length args)) (list? (car args)))
         ((lambda (keys values)
            (make-cell (map cons keys (map make-cell values))))
          (car args)
          (cadr args))]
        [else
          (error 'create-data "please provide keys and args or nothing")])))

  ;; return the data key list
  (define (data-keys data)
    (map car (data)))

  ;; return the data value list
  (define (data-values data)
    (map (lambda (proc) (proc)) (map cdr (data))))

  ;; return a list of accessors
  (define (data-guards data)
    (map cdr (data)))

  ;; delete one item in data, return left item
  (define (%delete-data key data)
    (remove (assq key (data)) (data)))

  ;; transform data to an association list
  (define (data->alist data)
    (map cons (data-keys data) (data-values data)))

  ;; same as %delete-data, but return unspecific
  (define (%delete-data! key data)
    (data (remove (assq key (data)) (data))))

  ;; query one value via a key
  (define (data-value-query key data)
    (if (memv key (data-keys data))
        (force (cdr (assq key (data))))
        #f))

  ;; query one accessor of one item
  (define (data-guard-query key data)
    (if (memv key (data-keys data))
        (cdr (assq key (data)))
        #f))

  ;; add one item in data
  ;; return the alist if successfully
  ;; return #f if exist one
  (define (%add-data! key value data)
    (if (memv key (data-keys data))
        #f
        (begin
          (data (cons (cons key (make-cell value)) (data)))
          (data->alist data))))

  ;; it receives three type args
  ;; 1. the data type, used to update values or add new values
  ;; 2. the assoc list, same as 1
  ;; 3. a list of symbols, all the symbols may be the keys and it
  ;;    will delete items in the data
  ;; first two is used to update! the data
  ;; the third is used to delete! the data
  (define (update-data! k-values data)
    (if (and (procedure? data) (alist? (data)))
        (cond
          [(alist? k-values)
           (do ((keys (map car k-values) (cdr keys)))
             ((null? keys) data)
             (if (memv (car keys) (data-keys data))
                 ((data-guard-query (car keys) data)
                  (cdr (assq (car keys) k-values)))
                 (%add-data! (car keys) (cdr (assq (car keys) k-values)) data)))]
          [(procedure? k-values)
           (update-data! (data->alist k-values) data)]
          [(list? k-values)
           (do ((keys k-values (cdr keys)))
             ((null? keys) data)
             (%delete-data! (car keys) data))]
          [else (error 'update-data! "no matched keys")])
        (error 'update-data! "should accept one data")))

  )
