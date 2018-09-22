(define-module (Flax theme)
  #:use-module (Flax post)
  #:use-module (srfi srfi-9)
  #:export (make-theme
	    is-theme?
	    get-theme-layout
	    set-theme-layout
	    get-theme-key
	    set-theme-key

	    default-index-theme
	    default-post-theme
	    ))

(define-record-type <theme>
  (make-theme key layout-sxml-tree)
  is-theme?
  (key get-theme-key set-theme-key)  ;; you'd better set the key as symbol
  (layout-sxml-tree get-theme-layout set-theme-layout)) ;; the layout is actually a procedure which return a sxml tree

;; the default theme for me to writing my blog
(define (add-css sxml-tree)
  (format #t "This is only for my blog"))

;; procedure will return the sxml tree
(define default-index-theme
  (make-theme 'index
	      (lambda (theme post)
		`(html (head (link (@ (rel "icon")
				      (href "../assets/images/logos/temp.ico")
				      (type "image/x-ico")))
			     (link (@ (rel "stylesheet")
				      (href "../assets/css/main/w3.css")))
			     (link (@ (rel "stylesheet")
				      (href "../assets/css/main/main.css"))))
		       (body ,((get-theme-layout theme) post))))))
(define default-post-theme
  (make-theme 'post
	      (lambda (post)
		`(div (@ (class "w3-content w3-center w3-padding-32")
			 (style "max-width:1400"))
		      (header (@ (class "w3-container w3-center w3-padding-32"))
			      (h1 "Lasga")
			      (h4 "Sometimes code, Alwayse imagine")
			      (p "by " 
				 (span (@ (class "w3-tag"))
				       ,(post-ref post 'Author))))
		      (div (@ (class "w3-row"))
			   (div (@ (class "w3-card-4 w3-margin w3-white"))
				(div (@ (class "w3-row")
					(style "background-color:black;"))
				     (img (@ (src ,(post-ref post 'imag))
					     (alt "preview image"))))
				(h2 ,(post-ref post 'title))
				(h5 (span (@ (class "w3-opacity"))
					  ,(post-ref post 'datetime)))
				(hr)
				(div (@ (class "w3-panel w3-border w3-light-grey w3-round-large"))
				     (h5 "Last Update Time :"
					 (span (@ (class "w3-opacity"))
					       ,(post-ref post 'update-time))
					 "."))
				(hr))
			   (div (@ (class "w3-row w3-left-align"))
				(div (@ (class "w3-card-4 w3-margin w3-white"))))
			   ; the post content
			   ,(post-ref post 'content))))))
