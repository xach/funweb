;;;; funweb.asd

(asdf:defsystem #:funweb
  :description "Describe funweb here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "protocols")
               (:file "request")
               (:file "response")
               (:file "handler")
               (:file "app")
               (:file "server")))

