(define-module (Flax theme)
  #:use-module (Flax post)

  #:use-module (srfi srfi-9)

  #:export (make-theme
	    is-theme?
	    get-theme-processor
	    set-theme-processor
	    get-theme-key
	    set-theme-key

	    default-index-theme
	    default-post-theme
	    default-processor))

(define-record-type <theme>
  (make-theme key processor)
  is-theme?
  (key get-theme-key set-theme-key)  ;; you'd better set the key as symbol
  (processor get-theme-processor set-theme-processor)) ;; the processor is a procedure which process the sxml tree

;; the default theme for me to writing my blog
(define (add-css sxml-tree)
  (format #t "This is only for my blog"))


(define default-index-theme
  (make-theme 'index
	      (lambda (theme post)
		`(html (head (link (@ (rel "icon")
				      (href "../assets/images/logos/temp.ico")
				      (type "image/x-ico")))
			     (link (@ (rel "stylesheet")
				      (href "../assets/css/main/w3.css")))
			     (link (@ (rel "stylesheet")
				      (href "../assets/css/main/main.css")))
			     (script (@ (type "text/javascript")
					(src "../assets/scripts/scrollup.js")))
			     (script (@ (type "text/javascript")
					(src "../assets/scripts/Topmenu.js")))
			     (script (@ (type "text/javascript")
					(src "../assets/scripts/jquery-3.3.1.min.js")))
			     (script (@ (type "text/javascript")
					(src "../assets/scripts/highlight.pack.js"))))
		       (body (@ (class "w3-light-grey"))
			     (div (@ (id "main")
                                     (class "w3-bar")
                                     (onmouseover "MenuToggle(this)")
                                     (onmouseout "MenuToggle(this)"))
                                  ;; three bars
                                  (div (@ (id "main")
                                          (class "menu w3-bar-item"))
                                       (div (@ (class "bar1")))
                                       (div (@ (class "bar2")))
                                       (div (@ (class "bar3"))))
                                  (div (@ (class "w3-bar-item w3-row w3-hide")
                                          (style "margin-left: 35%;"))
                                       (a (@ (class "w3-bar-item w3-button w3-black")
                                             (href "#"))
                                          "Home")
                                       (div (@ (class "w3-dropdown-hover"))
                                            (button (@ (class "w3-button"))
                                                    "tech"))))
			     ,((get-theme-processor theme) post)
			     (footer (@ (class "w3-container w3-center"))
				     (div (p "Created in 2018, Powered by Flax"))))))))

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
				(div (@ (class "w3-card-4 w3-margin w3-white"))
				     ,(get-post-sxml post))))))))

;; procedure will return the sxml tree
(define default-processor
  (make-theme 'meta
	      (lambda* (#:key theme-list post-object)
		((get-theme-processor (assq-ref theme-list 'index)) (assq-ref theme-list 'post) post-object))))
