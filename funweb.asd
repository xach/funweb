;;;; funweb.asd

(asdf:defsystem #:funweb
  :description "Simple, fun websites"
  :author "Zach Beane <xach@xach.com>"
  :license "BSD"
  :depends-on (#:hunchentoot
               #:trivial-backtrace
               #:quri
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

