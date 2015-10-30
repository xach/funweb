;;;; app.lisp

(in-package #:funweb)

(deftype http-method ()
  'keyword)

(defclass app ()
  ((name
    :initarg :name
    :reader name)
   (base-directory
    :initarg :base-directory
    :reader base-directory)
   (system-name
    :initarg :system-name
    :reader system-name)
   (host
    :initarg :host
    :accessor host)
   (url-path-prefix
    :initarg :url-path-prefix
    :accessor url-path-prefix)
   (handlers
    :initarg :handlers
    :reader handlers
    :documentation "A two-level table, keyed first on HTTP request
    method as a keyword (e.g. :GET), then on path suffix as a
    string (e.g. \"/make-bubble\".")
   (template-directory
    :initarg :template-directory
    :accessor template-directory)
   (output-directory
    :initarg :output-directory
    :accessor output-directory)
   (output-url
    :initarg :output-url
    :accessor output-url)
   (static-directory
    :initarg :static-directory
    :accessor static-directory)
   (static-url
    :initarg :static-url
    :accessor static-url )
   (app-data-table
    :initarg :app-data-table
    :accessor app-data-table)
   (dispatch-fun
    :initarg :dispatch-fun
    :accessor dispatch-fun
    :documentation "The app's dispatch fun is responsible for finding
    the right handler within the app to handle the request.")
   (server
    :initarg :server
    :accessor server))
  (:default-initargs
   :url-path-prefix ""
   :handlers (make-hash-table)
   :app-data-table (make-hash-table)
   :server *server*))

(defmethod initialize-instance :before ((object app) &key name)
  (unless name
    (error "NAME required for app initialization"))
  (unless (symbolp name)
    (error "NAME must be a symbol")))

(defmethod print-object ((object app) stream)
  (print-unreadable-object (object stream :type t)
    (let ((*package* (find-package :keyword)))
      (format stream "~S" (name object)))))

(defmethod slot-unbound ((class t) (app app) (slot-name (eql 'system-name)))
  (name app))

(defmethod slot-unbound ((class t) (app app) (slot-name (eql 'base-directory)))
  (let* ((system (asdf:find-system (system-name app) nil))
         (home (and system (asdf:system-source-directory system))))
    (when home
      (setf (slot-value app 'base-directory) home))))

(defmethod slot-unbound ((class t) (app app)
                         (slot-name (eql 'static-directory)))
  (setf (slot-value app 'static-directory)
        (relative-to app "static/")))

(defmethod slot-unbound ((class t) (app app)
                         (slot-name (eql 'template-directory)))
  (setf (slot-value app 'static-directory)
        (relative-to app "template/")))

(defmethod slot-unbound ((class t) (app app)
                         (slot-name (eql 'output-directory)))
  (setf (slot-value app 'static-directory)
        (relative-to app "output/")))


(defmacro define-app (name (&key (class 'app)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (find-app ',name *server*) (make-instance ',class :name ',name))))


(defgeneric configuredp (app)
  (:method ((app app))
    (and (slot-set-p app 'host)
         (slot-set-p app 'url-path-prefix)
         (slot-set-p app 'base-directory))))

(defgeneric map-handlers (fun app)
  (:method (fun (app app))
    (let ((seen (make-hash-table)))
      (maphash (lambda (http-method path-table)
                 (declare (ignore http-method))
                 (maphash (lambda (path-suffix handler)
                            (declare (ignore path-suffix))
                            (unless (gethash handler seen)
                              (setf (gethash handler seen) handler)
                              (funcall fun handler)))
                          path-table))
               (handlers app)))))

(defgeneric ensure-method-handlers (http-method app)
  (:method (http-method (app app))
    (check-type http-method http-method)
    (let ((handlers (handlers app)))
      (or (gethash http-method handlers)
          (setf (gethash http-method handlers)
                (make-string-table :case-sensitive t))))))

(defgeneric add-handler (handler app)
  (:method (handler (app app))
    (let ((suffix (path-suffix handler)))
      (flet ((add (http-method)
               (let ((table (ensure-method-handlers http-method app)))
                 (setf (gethash suffix table) handler))))
        (let ((method (http-method handler)))
          (if (listp method)
              (map nil #'add method)
              (add method)))
        handler))))

(defgeneric find-handler (http-method path-suffix app)
  (:method (http-method path-suffix app)
    (check-type http-method http-method)
    (check-type path-suffix string)
    (let ((method-handlers (gethash http-method (handlers app))))
      (when method-handlers
        (gethash path-suffix method-handlers)))))

(defmethod (setf find-handler) (new-value http-method path-suffix app)
  (check-type http-method http-method)
  (check-type path-suffix string)
  (let ((method-handlers (ensure-method-handlers http-method app)))
    (setf (gethash path-suffix method-handlers) new-value)))

(defun two-level-dispatch-table (app)
  (let ((http-method-table (make-hash-table))
        (prefix (url-path-prefix app)))
    (labels ((ensure-table (method)
               (or (gethash method http-method-table)
                   (setf (gethash method http-method-table)
                         (make-string-table :case-sensitive t)))))
      (map-handlers (lambda (handler)
                      (let ((methods (http-method handler)))
                        (unless (listp methods)
                          (setf methods (list methods)))
                        (dolist (method methods)
                          (let ((table (ensure-table method))
                                (key (concatenate 'string
                                                  prefix
                                                  (path-suffix handler))))
                            (setf (gethash key table) (fun handler))))))
                    app))
    http-method-table))

(defun two-level-lookup (table http-method path)
  (let ((t1 (gethash http-method table)))
    (gethash path t1)))

(defun app-hosted-url-p (app url)
  "Returns the URL path of URL if it is hosted by the app (e.g. it has the same host and port), NIL otherwise."
  (multiple-value-bind (protocol user host port path)
      (quri:parse-uri url)
    (declare (ignore protocol user port))
    (when (equalp host (host app))
      path)))

(defun make-handler-dispatcher (app)
  "Given a fully configured app (with host and path prefix),
  create a closure that takes a request object and returns a handler
  to handle it, or NIL if no handler matches."
  ;;; This is a function ripe for optimization and precomputing, if
  ;;; necessary. But no need to make it complicated until there are
  ;;; real apps with many, many handlers.
  (unless (configuredp app)
    (error 'object-not-configured
           :object app
           :missing-configuration '(host url-path-prefix)))
  (let ((app-host (host app))
        (table (two-level-dispatch-table app)))
    (lambda (request)
      (cond ((equal request :table)
             table)
            (t
             (let ((host (host request))
                   (path (url-path request)))
               (when (wild-equals host app-host)
                 (two-level-lookup table (http-method request) path))))))))

(defun dispatch-functions (app)
  (let ((output-path (app-hosted-url-p app (output-url app)))
        (static-path (app-hosted-url-p app (static-url app))))
    (remove nil
            (list (make-handler-dispatcher app)
                  (when output-path
                    (make-directory-dispatcher (output-directory app)
                                               output-path))
                  (when static-path
                    (make-directory-dispatcher (static-directory app)
                                               static-path))))))

(defun make-dispatch-fun (app)
  (let ((dispatchers (dispatch-functions app)))
    (lambda (request)
      (some (callfun request) dispatchers))))

(defmethod configure :after ((app app) plist)
  (when (configuredp app)
    (setf (dispatch-fun app) (make-dispatch-fun app))))

(defun find-app-handler (request app)
  (funcall (dispatch-fun app) request))

(defmethod (setf find-handler) :after ((new-value t)
                                       (http-method t)
                                       (path-suffix t)
                                       (app app))
  (configure app nil))

(defmethod app ((app-name symbol))
  (find-app app-name *server*))
