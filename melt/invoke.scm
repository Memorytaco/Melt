#!chezscheme
(library (melt invoke)
  (export init-chain
          execute-chain
          chain-data-keys
		  chain-data-alist
		  chain-data-query
		  chain-data-update!
		  chain-condition-value
          chain-hooks-list-query
		  
		  
          create-hook
          hook-execute
          get-hook-args
          get-hook-proc
          get-hook-keys
          get-hook-alist
          add-hook!)
  (import (scheme)
          (melt utils)
          (melt structure)
		  (melt data))
  
  (import type-hook)
  (import type-chain)
  (import type-data)

  (define create-hook
    (lambda (name type arg)
      (cond
       [(eq? type 'data)
        (make-hook name 'data #f arg)]
       [(eq? type 'proc)
        (make-hook name 'proc arg #f)]
       [else (error type "type must be 'data or 'proc!")])))
  
  ;; accept a chain record return the data key list
  (define (chain-data-keys chain)
    (if (chain? chain)
        (data-keys (chain-data chain))
        (error chain "argument should be type chain!")))
  
  ;; accept a chain record return the data alist
  (define (chain-data-alist chain)
    (if (chain? chain)
        (data-cont (chain-data chain))
        (error chain "argument should be type chain!")))

  ;; if the key not in chain keys, return #f
  ;; otherwise return the value
  (define (chain-data-query key chain)
	(if (chain? chain)
		(if (element-exist? key (chain-data-keys chain))
			(cdr (assq key (chain-data-alist chain)))
			#f)
		(error chain "argument should be type chain!")))
  
  ;; keys and datas must be a list and must be match
  ;; if the keys doesn't exist, create it!
  ;; otherwise alter it!
  (define chain-data-update!
    (case-lambda
      [(chain data)
       (update-data! (chain-data chain)
					 data)]
      [(chain keys values)
       (chain-data-update! chain (create-data keys values))]))

  ;; condition is described in structure
  ;; execution is one procedure
  ;; data is the data type
  (define init-chain
    (lambda (condition execution data)
      (if (not (data? data))
          (error 'data "in init-chain : data should be 'data type"))
      (let ((chain (make-chain condition (list execution) data)))
        (cond
		 [(element-exist? 'chain (chain-data-keys chain))
		  (error 'data "You shouldn't use 'chain as key!")]
		 [(element-exist? 'hooks (chain-data-keys chain))
		  (error 'data "You shouldn't use 'hooks as key!")])
		
		(chain-data-update! chain (create-data '(chain) `(,chain)))
		(chain-data-update! chain (create-data '(hooks) '(((data . ())
														   (proc . ())))))
        chain)))
  
  ;; execute chain
  (define (execute-chain chain)
    (if (chain-condition-value chain)
        (do ((execute-list (chain-execution chain) (cdr execute-list)))
            ((null? execute-list) #t)
          ((car execute-list)))))
  
  (define (chain-condition-value chain)
    (let ((condition (chain-condition chain)))
      (cond
       [(procedure? condition)
        (condition)]
       [else condition])))
  
  (define (get-hook-proc hook)
    (if (and (hook? hook)
             (eq? 'proc (hook-type hook)))
        (car (hook-proc-arg hook))
        (error hook "argument must be hook type or hook must be 'proc type! : get-hook-proc")))
  
  (define (get-hook-args hook)
    (if (and (hook? hook)
             (eq? 'proc (hook-type hook)))
        (cdr (hook-proc-arg hook))
        (error hook "argument must be hook type or hook must be 'proc type! : get-hook-args")))
  
  (define (get-hook-keys hook)
    (if (and (hook? hook)
             (eq? 'data (hook-type hook)))
        (car (hook-data hook))
        (error hook "argument must be hook type or hook must be 'data type! : get-hook-keys")))
  
  (define (get-hook-alist hook)
    (if (and (hook? hook)
             (eq? 'data (hook-type hook)))
        (cdr (hook-data hook))
        (error hook "argument must be hook type or hook must be 'data type! : get-hook-alist")))
  
  (define (hook-execute hook)
    (if (eq? 'proc (hook-type hook))
        (apply (get-hook-proc hook)
               (if (list? (get-hook-args hook))
				   (get-hook-args hook)
				   (error 'hook-execute "the arguments must be wrapped in a list")))
        (error 'hook "hook type is not 'proc! : hook-execute")))

  (define (chain-hooks-list-query chain hook-type-key)
	(if (not (element-exist? hook-type-key '(proc data)))
		(error 'hook-type-key "key is not 'proc or 'data! : chain-hooks-list-query"))
	(let ((hooks-alist (chain-data-query 'hooks chain)))
	  (cdr (assq hook-type-key hooks-alist))))

    ;; not export this function.
  (define (chain-hooks-update! chain hook symbol)
	(define symbol-list '(before after data))
	
	;; just return the updated data, doesn't modify anything
	(define (update-chain-hook-procedure chain hook symbol)
	  (create-data '(hooks)
				   `((,(assq 'data
							 (chain-data-query 'hooks
											   chain))
					  (proc . ,(cond
								[(eq? 'before symbol)
								 (cons (hook-name hook)
									   (cdr (assq 'proc
												  (chain-data-query 'hooks
																	chain))))]
								[(eq? 'after symbol)
								 (append (cdr (assq 'proc
													(chain-data-query 'hooks
																	  chain)))
										 (list (hook-name hook)))]))))))

	(define (update-chain-hook-data chain hook symbol)
	  (create-data '(hooks)
				   `(((data ,(cons (hook-name hook)
								   (cdr (assq 'data
											  (chain-data-query 'hooks
																chain)))))
					  ,(assq 'proc
							 (chain-data-query 'hooks
											   chain))))))
	;; check the input
	(cond
     [(not (chain? chain))
      (error 'chain "must be a chain record!")]
     [(not (hook? hook))
      (error 'hook "must be a hook record!")]
     [(not (element-exist? symbol symbol-list))
      (error 'symbol "must be 'before, 'after or 'data(if hook type is data) symbol!")])

	(cond
	 [(eq? 'proc (hook-type hook))
	  (chain-data-update! chain (update-chain-hook-procedure chain hook symbol))]
	 [(eq? 'data (hook-type hook))
	  (chain-data-update! chain (update-chain-hook-data chain hook symbol))]
	 [else (error 'hook "hook type is not correct!")]))
  
  ;; add-hook to the chain
  (define add-hook!
    (case-lambda
      [(chain hook symbol)
       (define symbol-list '(before after data))
       (cond
        [(not (chain? chain))
         (error 'chain "must be a chain record!")]
        [(not (hook? hook))
         (error 'hook "must be a hook record!")]
        [(not (element-exist? symbol symbol-list))
         (error 'symbol "must be 'before, 'after or 'data(if hook type is data) symbol!")])
       (cond
        [(eq? symbol 'before)
         (execution-set! chain
                         (cons (lambda ()
                                 (hook-execute hook))
                               (chain-execution chain)))
		 (chain-hooks-update! chain hook symbol)]
        [(eq? symbol 'after)
         (execution-set! chain
                         (append (chain-execution chain)
                                 (list (lambda ()
                                         (hook-execute hook)))))
		 (chain-hooks-update! chain hook symbol)]
        [(eq? symbol 'data)
         (if (eq? 'data (hook-type hook))
             (begin
			   (chain-data-update! chain (hook-data hook))
			   (chain-hooks-update! chain hook symbol))
			 (error 'hook "type is not data!"))]
        [else (error symbol "the symbol must be 'before 'after or 'data!")])]
      [(chain hook)
	   (cond
		[(eq? 'data (hook-type hook))
		 (add-hook! chain hook 'data)]
		[(eq? 'proc (hook-type hook))
		 (add-hook! chain hook 'after)]
		[else (error 'hook "incorrect hook type!")])]))
  
  (define chain-connect
    (lambda ()
      (format #t "not ready")))
  
  )
