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
  (:export #:define-app
           #:define-handler
           #:app-data
           #:fill-template
           #:template-values
           #:make-output-file
           #:path
           #:file
           #:app
           #:no-base-directory
           #:base-directory
           #:relative-to))

