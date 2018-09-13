(define-module (Flax utils)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 match)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-13)
               #:use-module (srfi srfi-19)
               #:use-module (srfi srfi-26)

               #:export (absolute-path
                         relative-path
                         decompose-file-name
                         compose-file-name
                         mkdir-p))

;; return the absolute path of the file
(define absolute-path
  (lambda (file-name)
    "Return the absolute path of the file"
    (if (absolute-file-name? file-name)
      file-name
      (string-append (getcwd) "/" file-name))))

;; decompose a string
;; example
;; "/usr/lib/share" => ("usr" "lib" "share")
(define decompose-file-name
  (lambda (filename)
    "Split filename into components seperated by '/' "
    (match filename
      ("" '())  ;; if string is empty
      ("/" '("")) ;; if string only has one '/'
      (_ (delete "" (string-split filename #\/))))))

;; components is a list of strings
;; like ("hello" "nice" "good") => /hello/nice/good
(define compose-file-name
  (lambda (components)
    "put all strings of the filename together"
    (string-join components "/" 'prefix)))

;; create directory
(define (mkdir-p dir)
  "create directory ~dir~ and all its ancestors"
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    ;; get all complement of charset #<charset {#\null #\. #\0 ....}>
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
            (root        (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-errors
           ;; NOTE : couldn't understand
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda (args)
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))
