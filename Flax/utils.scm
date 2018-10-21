(define-module (Flax utils)
  #:use-module (Flax site)
  
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  
  #:export (get-absolute-path
            decompose-file-name
            compose-file-name
            mkdir-p
            delete-file-recursively
            remove-stat
	    is-directory?
            get-file-tree-list
	    make-user-module
	    config-load
	    string-split-at))

;; return the absolute path of the file
;; TODO : need to do better
(define (get-absolute-path file-name)
  (if (absolute-file-name? file-name)
      file-name
      (string-append (getcwd) "/" file-name)))

;; load-file
(define (config-load file-name)
  (let ((path-to-file-name (get-absolute-path file-name)))
    (if (file-exists? path-to-file-name)
	(let ((obj (load path-to-file-name)))
	  (if (is-site? obj)
	      obj
	      (begin
		(format (current-error-port) "Invalid obj ~%expect site but got ~a~%" obj)
		'())))
	(begin
	  (format (current-error-port) "Error ! File ~a doesn't exist!~%" file-name)
	  '()))))


;; decompose a string
;; example
;; "////usr/lib/share" => ("usr" "lib" "share")
(define (decompose-file-name file-name)
  "split filename into components seperated by '/' "
  (match file-name
    ("" '())      ;; if string is empty
    ("/" '(""))   ;; if string only has one '/'
    (_ (delete "" (string-split file-name #\/)))))

;; components is a list of strings
;; like ("hello" "nice" "good") => /hello/nice/good
(define (compose-file-name components)
  (string-join components "/" 'prefix))

;; if dir is directory,return #t else return #f
(define (is-directory? file-name)
  (if (eq? 'directory (stat:type (stat file-name)))
      #t
      #f))

;; create directory
;; mark !! need to improve!!!!!!!!!!!!!!!!!!!!!!!!!!
(define (mkdir-p dir)
  "Create the dir just like use makedir bash command but clever"
  (define dir-list (decompose-file-name dir))
  (let ((file-name (if (absolute-file-name? dir)
		       (string-append "/" (car dir-list))
		       (car dir-list))))
    (while (not (eq? '() dir-list))
      (if (file-exists? file-name)
	  (if (not (is-directory? file-name))
	      (begin
		(format (current-error-port) "There conficts file exists!!~%")
		(break)))
	  (mkdir file-name))
      (set! dir-list (cdr dir-list))
      (if (not (eq? '() dir-list))
	  (set! file-name (string-append file-name "/" (car dir-list)))))
    #t))

;; remove the file tree list stat and return
;; the ~file-tree-list~ is the value of procedure
;; ~file-system-tree~ which is in (ice-9 ftw)
(define (remove-stat file-tree-list)
  (match file-tree-list
    ((name stat)
      name)
    ((name stat children ... )
      (cons (car file-tree-list) (map remove-stat children)))))

;; if file-name is a file,return the file name
;; if the file-name is a directory, return the tree list of the directory
;; just like execute "tree file-name"
(define (get-file-tree-list file-name)
  (remove-stat (file-system-tree file-name)))

;; return a new user module with the additional modules loaded
(define (make-user-module modules)
  (let ((module (make-fresh-user-module)))
      (for-each (lambda (iface)
                  (module-use! module (resolve-interface iface)))
                modules)
      module))

(define (string-split-at str char-pred)
  (let ((i (string-index str char-pred)))
    (if i
	(list (string-take str i)
	      (string-drop str (1+ i)))
	(list str))))
