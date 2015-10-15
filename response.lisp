;;;; response.lisp

(in-package #:funweb)

(defun guess-content-type (object)
  (typecase object
    (string
     "text/html")
    ((simple-array (unsigned-byte 8) (*))
     "application/octet-stream")
    (t
     "application/octet-stream")))

(defun make-response (body &key content-type)
  (unless content-type
    (setf content-type (guess-content-type body)))
  (list :body body :content-type content-type))

(defun make-redirection-response (location &key permanently)
  (list :redirect location :permanent permanently))

(defun make-not-found-response ()
  (list :not-found t))

(defun send-response (response)
  (etypecase response
    (string
     response)
    (list
     (case (first response)
       (:body
        (let ((content-type (getf response :content-type))
              (body (second response)))
          (when content-type
            (setf (tbnl:content-type*) content-type))
          body))
       (:redirect
        (let ((code (if (getf response :permanent)
                        tbnl:+http-moved-permanently+
                        tbnl:+http-moved-temporarily+)))
          (tbnl:redirect (second response) :code code)))
       (:not-found
        (setf (tbnl:return-code*) 404)
        (setf (tbnl:content-type*) "text/plain")
        (tbnl:abort-request-handler "404 - not found (fw)"))))))
