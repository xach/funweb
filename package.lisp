;;;; package.lisp

(defpackage #:funweb
  (:use #:cl)
  (:nicknames #:fw)
  (:export #:configuredp
           #:configure
           #:configure-from-file
           #:object-not-configured)
  (:export #:host
           #:port
           #:http-method)
  (:export #:*request*
           #:url-path
           #:parameter-value
           #:header-value
           #:request-property)
  (:export #:start-server
           #:stop-server)
  (:export #:make-output-file
           #:path
           #:url
           #:download-url)
  (:export #:define-app
           #:define-handler
           #:app-data
           #:*app*
           #:static-url
           #:output-url
           #:fill-template
           #:template-values
           #:make-output-file
           #:path
           #:file
           #:app
           #:no-base-directory
           #:base-directory
           #:relative-to)
  (:export #:make-response
           #:make-redirection-response
           #:make-not-found-response
           #:make-file-response
           #:make-error-response))

