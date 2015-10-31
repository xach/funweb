;;;; protocols.lisp

(in-package #:funweb)

;;; Generic

(defgeneric configuredp (object)
  (:documentation
   "Returns true if OBJECT is configured, in some sense."))

(defgeneric configure (object plist)
  (:method (object (plist list))
    (apply #'reinitialize-instance
           object
           plist)))

(defgeneric configure-from-file (object file)
  (:method (object file)
    (with-open-file (stream file)
      (with-standard-io-syntax
        (let ((plist (read stream)))
          (configure object plist))))))

(define-condition object-not-configured (error)
  ((object
    :initarg :object
    :reader object-not-configured-object)
   (missing-configuration
    :initarg :missing-configuration
    :initform nil
    :reader object-not-configured-missing-configuration))
  (:report
   (lambda (condition stream)
     (format stream "Object ~A is not configured~
                     ~@[: missing configuration for ~{~A~^, ~}~]"
             (object-not-configured-object condition)
             (object-not-configured-missing-configuration condition)))))


(defgeneric host (object))
(defgeneric port (object))

(defgeneric find-handler (request object))

(defgeneric http-method (object))

;;; Requests

(defvar *request* nil)

(defgeneric url-path (request))
(defgeneric parameter-value (name request))
(defgeneric header-value (name request))
(defgeneric request-property (name request))
(defgeneric (setf request-property) (new-value name request))


;;; Server

(defgeneric start (server))
(defgeneric stop (server))
(defgeneric apps (server))
(defgeneric find-app (name server))
(defgeneric (setf find-app) (new-value name server))


;;; App

(defvar *app* nil)

(defgeneric server (app))
(defgeneric handlers (app))
(defgeneric name (app))
(defgeneric url-path-prefix (app))
(defgeneric handlers (app))
(defgeneric find-handler-function (app))
(defgeneric find-handler (request app))
(defgeneric template-directory (app))
(defgeneric output-directory (app))

(defgeneric app-data (key app))
(defgeneric (setf app-data) (new-value key app))

(defgeneric find-template (name app))
(defgeneric fill-template (name values))
(defgeneric template-values (object))

(define-condition no-app-active (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "No app is currently active"))))


;;; Output files

(defgeneric make-output-file (app name type))
(defgeneric path (output))
(defgeneric url (output))


;;; Handlers

(defgeneric app (handler))
(defgeneric url-path-suffix (handler))
(defgeneric fun (handler))


;;; Relative file reference

(define-condition no-base-directory (error)
  ((object
    :initarg :object
    :reader no-base-directory-object))
  (:report
   (lambda (condition stream)
     (format stream "~A does not have a base directory"
             (no-base-directory-object condition)))))

(defgeneric base-directory-delegate (object)
  (:method (object)
    object))

(defgeneric no-base-directory (object)
  (:method (object)
    (error 'no-base-directory :object object)))

(defgeneric base-directory (object)
  (:method ((object pathname))
    object)
  (:method ((object string))
    (parse-namestring object))
  (:method ((object t))
    (no-base-directory object)))

(defmethod base-directory :around ((object t))
  (let ((value (call-next-method)))
    (unless value
      (no-base-directory object))))

(defgeneric relative-to (object pathname)
  (:method (object pathname)
    (let ((directory (pathname-directory pathname)))
      (unless (or (null directory) 
                  (eql (first directory)
                       :relative))
        (error "~A is not a relative pathname" pathname)))
    (merge-pathnames pathname (base-directory
                               (base-directory-delegate object)))))
