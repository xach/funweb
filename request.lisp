;;;; request.lisp

(in-package #:funweb)

(defmethod host ((request tbnl:request))
  (let* ((host (tbnl:host request))
         (colon (position #\: host)))
    (if colon
        (subseq host 0 colon)
        host)))

(defmethod port ((request tbnl:request))
  (let* ((host (tbnl:host request))
         (colon (position #\: host)))
    (if colon
        (parse-integer host :start (1+ colon))
        80)))

(defmethod url-path ((request tbnl:request))
  (tbnl:script-name request))

(defmethod http-method ((request tbnl:request))
  (tbnl:request-method request))

(defmethod parameter-value (name (request tbnl:request))
  (let ((lookup-name (etypecase name
                       (symbol (string-downcase name))
                       (string name))))
    (or (tbnl:get-parameter lookup-name request)
        (tbnl:post-parameter lookup-name request))))

(defmethod (setf request-property) (new-value name (request tbnl:request))
  (setf (tbnl:aux-request-value name request) new-value))

(defmethod request-property (name (request tbnl:request))
  (tbnl:aux-request-value name request))

(defclass mock-request ()
  ((host
    :initarg :host
    :reader host)
   (port
    :initarg :port
    :reader port)
   (http-method
    :initarg :http-method
    :reader http-method)
   (url-path
    :initarg :url-path
    :reader url-path)
   (parameters
    :initarg :parameters
    :initform nil
    :reader parameters)
   (properties
    :initarg :properties
    :initform (make-hash-table)
    :reader properties)))

(defmethod parameter-value (name (request mock-request))
  (getf (parameters request) name))

(defmethod print-object ((request mock-request) stream)
  (print-unreadable-object (request stream :type t :identity t)
    (format stream "~A:~A ~S ~S"
            (host request)
            (port request)
            (http-method request)
            (url-path request))))

(defmethod (setf request-property) (new-value name (request mock-request))
  (setf (gethash name (properties request)) new-value))

(defmethod request-property (name (request mock-request))
  (gethash name (properties request)))

(defun make-mock-request (host port method path
                          &rest parameters &key &allow-other-keys)
  (make-instance 'mock-request
                 :host host
                 :port port
                 :http-method method
                 :url-path path
                 :parameters parameters))
