(import (scheme))
(import (melt lib markdown))
(import (melt lib sxml))

(define-syntax test-input
  (syntax-rules ()
	[(_ output input body)
	 (call-with-output-file output
	   (lambda (port)
		 (sxml->html
		  (call-with-input-file input
			body)
		  port)))]))

; (test-input "paragraph-test" "test3" (lambda (port) (compose-paragraph (list) port)))

; (test-input "paragraph" "test" (lambda (port) (compose-paragraph (list) port)))

; (test-input "context" "test" (lambda (port) (list (context-switch 'line port) (context-switch 'line port))))


(test-input "Hello.html" "test" (lambda (port) (markdown->sxml port)))

