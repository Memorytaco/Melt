#!chezscheme
(library
  (melt invoke)
  (export
    invoke
    create-invocation
    invoke-guard
    invoke-procs
    invoke-data
    invoke-update-data!
    invoke-delete-data!
    invoke-add-proc!
    create-proc
    execute-proc)
  (import (scheme)
          (melt utils)
          (melt data)
          (melt cell))

  (define invoke
    (lambda (invocation)
      (let ((proc-alist (data->alist (invoke-procs invocation))))
        (do ((procs-list (map cdr (sort (lambda (pre aft)
                                          (> (car pre) (car aft)))
                                        proc-alist))
                         (cdr procs-list)))
          ((null? procs-list) (void))
          (execute-proc (car procs-list))))))

  ;; create invocation
  (define create-invocation
    (case-lambda
      [()
       (define invocation
         (create-data `((iguard . #t)
                        (procs . ,(create-data))
                        (data . ,(create-data)))))
       (update-data! `((self . ,invocation)) invocation)
       (make-cell invocation)]
      [(iguard procs data)
       (let ((invocation (create-data `((iguard . ,iguard)
                                        (procs . ,procs)
                                        (data . ,data)))))
         (update-data! `((self . ,invocation)) invocation)
         (make-cell invocation))]))

  (define (invoke-guard invocation)
    (data-value-query 'iguard (invocation)))

  (define (invoke-procs invocation)
    (data-value-query 'procs (invocation)))

  (define (invoke-data invocation)
    (data-value-query 'data (invocation)))

  ;; update data
  (define (invoke-update-data! key value invocation)
    (let ((data (invoke-data invocation)))
      (update-data! `((,key . ,value)) data)))

  ;; delete one item
  (define (invoke-delete-data! key invocation)
    (let ((data (invoke-data invocation)))
      (update-data! (list key) data)))

  ;; add one proc
  (define (invoke-add-proc! proc invocation)
    (let ((procs (invoke-procs invocation)))
      (update-data! `((,(+ 1 (apply max (if (null? (data-keys procs)) '(0) (data-keys procs)))) . ,proc))
                    procs)))

  ;; create proc for chain execute unite
  (define (create-proc proc . args)
    (create-data `((proc . ,proc)
                   (args . ,args))))

  ;; execute one proc
  (define (execute-proc proc)
    (apply (data-value-query 'proc proc)
           (data-value-query 'args proc)))

  )
