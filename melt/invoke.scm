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
		  chain-hooks-update!
		  
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

  ;; create hook , if type is data, the arg must be data type
  ;; otherwise if type is proc, the arg must be (procdure . args-list)
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
		(data-value-query key (chain-data chain))
		(error chain "in chain-data-query : invoke.scm")))
  
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
		 [(memv 'chain (chain-data-keys chain))
		  (error 'data "You shouldn't use 'chain as key!")]
		 [(memv 'hooks (chain-data-keys chain))
		  (error 'data "You shouldn't use 'hooks as key!")])
		
		(chain-data-update! chain '(chain) `(,chain))
		(chain-data-update! chain '(hooks) `(,(create-data '(data proc)
														   '(() ()))))
        chain)))
  
  ;; execute chain
  (define (execute-chain chain)
    (if (chain-condition-value chain)
        (do ((execute-list (chain-execution chain) (cdr execute-list)))
            ((null? execute-list) #t)
          ((car execute-list)))))

  ;; if the condition is procedure, execute it to get the value
  ;; if the condition is a value, return it
  (define (chain-condition-value chain)
    (let ((condition (chain-condition chain)))
      (cond
       [(procedure? condition)
        (condition)]
       [else condition])))

  ;; return type proc hook's procedure
  (define (get-hook-proc hook)
    (if (and (hook? hook)
             (eq? 'proc (hook-type hook)))
        (car (hook-proc-arg hook))
        (error hook "argument must be hook type or hook must be 'proc type! : get-hook-proc")))

  ;; return type proc hook's arguments
  (define (get-hook-args hook)
    (if (and (hook? hook)
             (eq? 'proc (hook-type hook)))
        (cdr (hook-proc-arg hook))
        (error hook "argument must be hook type or hook must be 'proc type! : get-hook-args")))

  ;; return type data hook's keys in its data field
  (define (get-hook-keys hook)
    (if (and (hook? hook)
             (eq? 'data (hook-type hook)))
        (data-keys (hook-data hook))
        (error hook "argument must be hook type or hook must be 'data type! : get-hook-keys")))

  ;; return type data hook's assoc list
  (define (get-hook-alist hook)
    (if (and (hook? hook)
             (eq? 'data (hook-type hook)))
        (data-cont (hook-data hook))
        (error hook "argument must be hook type or hook must be 'data type! : get-hook-alist")))

  ;; execute the hook
  (define (hook-execute hook)
    (if (eq? 'proc (hook-type hook))
        (apply (get-hook-proc hook)
               (if (list? (get-hook-args hook))
				   (get-hook-args hook)
				   (error 'hook-execute "the arguments must be wrapped in a list")))
        (error 'hook "hook type is not 'proc! : hook-execute")))

  ;; get the proc or data type hooks list, it's a list of names
  (define (chain-hooks-list-query hook-type-key chain)
	(if (not (memv hook-type-key '(proc data)))
		(error 'hook-type-key "key is not 'proc or 'data! : chain-hooks-list-query"))
	(let ((chain-hooks (chain-data-query 'hooks chain)))
	  (data-value-query hook-type-key chain-hooks)))

  ;; not export this function.
  (define (chain-hooks-update! chain hook symbol)
	(define symbol-list '(before after data))
	
	;; just return the updated data, doesn't modify anything
	(define (update-chain-hook-procedure chain hook symbol)
	  (let ((hook-name-list (chain-hooks-list-query 'proc chain))
			(name (hook-name hook)))
		 (update-data! (chain-data-query 'hooks chain)
					   (create-data '(proc)
									(list (cond [(eq? 'before symbol)
												 (cons name
													   hook-name-list)]
												[(eq? 'after symbol)
												 (append hook-name-list
														 (list name))]))))))
	
	(define (update-chain-hook-data chain hook symbol)
	  (let ((hook-name-list (chain-hooks-list-query 'data chain))
			(name (hook-name hook)))
		(update-data! (chain-data-query 'hooks chain)
					  (create-data '(data)
								   `(,(cons name hook-name-list))))))
	;; check the input
	(cond
     [(not (chain? chain))
      (error 'chain "must be a chain record!")]
     [(not (hook? hook))
      (error 'hook "must be a hook record!")]
     [(not (memv symbol symbol-list))
      (error 'symbol "must be 'before, 'after or 'data(if hook type is data) symbol!")])

	(cond
	 [(eq? 'proc (hook-type hook))
	  (update-chain-hook-procedure chain hook symbol)]
	 [(eq? 'data (hook-type hook))
	  (update-chain-hook-data chain hook symbol)]
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
        [(not (memv symbol symbol-list))
         (error 'symbol "must be 'before, 'after or 'data(if hook type is data) symbol!")])
       ;; update the data and the hooks name list
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
	  ;; accept two arg, it's an abbr
      [(chain hook)
	   (cond
		[(eq? 'data (hook-type hook))
		 (add-hook! chain hook 'data)]
		[(eq? 'proc (hook-type hook))
		 (add-hook! chain hook 'after)]
		[else (error 'hook "incorrect hook type!")])]))
  
  )
