(define-module (Flax process utils)
  #:use-module (Flax post)
  #:use-module (Flax utils)
  
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)

  #:export (navigate))


;; navigate between files, read the post, use action to process each
;; post, and then return a list, this action is defined by user and the
;; action is a function which accept a post . you can do what you want.
;; Don't use the flag keyword in any situation!
;; You can set the environment keyword if you need to.
(define* (navigate file-tree action #:key (flag #f) (environment (getcwd)))
  (if (not (pair? file-tree))
      '()
      (let ((environment* (if flag
			      (string-append environment "/" (car file-tree))
			      environment))
	    (file-tree* (if flag
			    (cdr file-tree)
			    file-tree)))
	(append (if (pair? (car file-tree*))
		    (navigate (car file-tree*)
			      action
			      #:flag #t
			      #:environment environment*)
		    (action (if (not (is-directory? (string-append environment* "/" (car file-tree*))))
				(read-post (string-append environment* "/" (car file-tree*)))
				'())))
		(navigate (cdr file-tree*)
			  action
			  #:flag #f
			  #:environment environment*)))))
