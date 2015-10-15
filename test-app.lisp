;;;; test-app.lisp

(in-package #:funweb)

(define-app test-app ())

(define-handler test-app (:get "hello-world") ()
  "Hello, world")
