;;;; funweb.asd

(asdf:defsystem #:funweb
  :description "Describe funweb here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:trivial-backtrace
               #:html-template)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "protocols")
               (:file "request")
               (:file "response")
               (:file "serve-directory")
               (:file "template")
               (:file "handler")
               (:file "app")
               (:file "output")
               (:file "server")))

