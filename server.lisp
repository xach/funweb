;;;; server.lisp

(in-package #:funweb)

(defclass acceptor (tbnl:acceptor)
  ((server
    :initarg :server
    :reader server)))

(defmethod tbnl:acceptor-dispatch-request ((acceptor acceptor)
                                                  request)
  (funcall (dispatcher (server acceptor)) request))

(defclass server ()
  ((host
    :initarg :host
    :accessor host)
   (port
    :initarg :port
    :accessor port)
   (acceptor
    :initarg :acceptor
    :initform nil
    :accessor acceptor)
   (startedp
    :initarg :startedp
    :initform nil
    :accessor startedp)
   (dispatcher
    :initarg :dispatcher
    :reader dispatcher
    :initform 'server-dispatcher)
   (apps
    :initarg :apps
    :initform (make-hash-table)
    :accessor apps)
   (error-log-file
    :initarg :error-log-file
    :reader error-log-file)))

(defvar *server* (make-instance 'server))

(defun map-apps (fun server)
  (maphash (lambda (name app)
             (declare (ignore name))
             (funcall fun app))
           (apps server)))

(defmethod configuredp ((server server))
  (and (slot-set-p server 'host)
       (slot-set-p server 'port)
       (slot-set-p server 'error-log-file)))

(defmethod example-configuration ((server server))
  (list :host "localhost"
        :port 8000
        :error-log-file "/tmp/funweb-error.log"))

(defun acceptor-synced-p (server)
  (let ((acceptor (acceptor server)))
    (when (and acceptor (configuredp server))
      (and
       (equal (host server)
              (tbnl:acceptor-address acceptor))
       (equal (port server)
              (tbnl:acceptor-port acceptor))))))

(defmethod start ((server server))
  (unless (configuredp server)
    (error 'object-not-configured
           :object server
           :missing-configuration '(host port error-log-file)))
  (unless (startedp server)
    (map-apps (lambda (app)
                (unless (configuredp app)
                  (error 'object-not-configured
                         :object app)))
              server)
    (let ((acceptor (make-instance 'acceptor
                                   :address (host server)
                                   :port (port server)
                                   :server server)))
      (when (error-log-file server)
        (setf (tbnl:acceptor-message-log-destination acceptor)
              (error-log-file server)))
      (setf (acceptor server) acceptor)
      (setf (tbnl:acceptor-access-log-destination acceptor) nil)
      (tbnl:start acceptor)
      (setf (startedp server) t))))

(defmethod stop ((server server))
  (when (startedp server)
    (tbnl:stop (acceptor server))
    (setf (startedp server) nil)))

(defun start-server ()
  (start *server*))

(defun stop-server ()
  (stop *server*))


(defmethod find-app (name (server server))
  (values (gethash name (apps server))))

(defmethod (setf find-app) (new-value name (server server))
  (check-type new-value app)
  (setf (gethash name (apps server)) new-value))

(defun error-response-code ()
  (random-file-string 8))

(defun get-response (request server)
  (let ((response
         (catch 'response
           (block nil
             (handler-bind ((error
                             (lambda (condition)
                               (return
                                 (make-error-response condition
                                                      :code (error-response-code)
                                                      :backtrace (backtrace-string))))))
               (map-apps (lambda (*app*)
                           (when (configuredp *app*)
                             (let ((handler-fun (find-app-handler request
                                                                  *app*)))
                               (when handler-fun
                                 (return (funcall handler-fun request))))))
                         server))))))
    (or response
        (make-not-found-response))))

(defun server-dispatcher (request)
  (send-response (get-response request *server*)))

(defmethod configure :after ((server server) plist)
  ;; FIXME: This is to update the dispatch funs...maybe do it via some
  ;; other method?
  (map-apps (lambda (app) (configure app nil)) server))

(defmethod configure-from-file :after ((server server) file)
  (map-apps
   (lambda (app)
     (format t "; configuring ~A~%" app)
     (let ((app-config-file
            (merge-pathnames
             (make-pathname :name (string-downcase (name app))
                            :directory (list :relative "apps")
                            :defaults file)
             file)))
       (format t ";; config file: ~A~%" app-config-file)
       (when (probe-file app-config-file)
         (configure-from-file app app-config-file))))
   server))
