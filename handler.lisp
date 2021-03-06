;;;; handler.lisp

(in-package #:funweb)

(defclass handler ()
  ((http-method
    :initarg :http-method
    :reader http-method)
   (path-suffix
    :initarg :path-suffix
    :reader path-suffix)
   (fun
    :initarg :fun
    :accessor fun)
   (app
    :initarg :app
    :reader app)))

(defmacro define-handler (app-name (http-method path-suffix) (&rest parameters)
                          &body body)
  (check-type http-method http-method)
  (check-type path-suffix string)
  (let ((app (find-app app-name *server*))
        (handler (gensym))
        (app-var (gensym))
        (fun (gensym))
        (handler-name (make-symbol (format nil "~A ~S ~S"
                                           app-name
                                           http-method
                                           path-suffix))))
    (unless app
      (error "Unknown app -- ~S" app-name))
    `(let* ((,app-var (find-app ',app-name *server*))
            (,fun (named-lambda ,handler-name (*request*)
                    (let ,(loop for parameter in parameters
                                for var-name = (if (consp parameter)
                                                   (first parameter)
                                                   parameter)
                                for cgi-name = (if (consp parameter)
                                                   (second parameteR)
                                                   parameter)
                                collect `(,var-name
                                          (parameter-value ',cgi-name *request*)))
                      (declare (ignorable ,@(mapcar (lambda (parameter)
                                                      (if (consp parameter)
                                                          (first parameter)
                                                          parameter))
                                                    parameters)))
                      ,@body)))
            (,handler (find-handler ',http-method ',path-suffix ,app-var)))
       (if ,handler
           (setf (fun ,handler) ,fun)
           (setf (find-handler ',http-method ',path-suffix ,app-var)
                 (make-instance 'handler
                                :http-method ',http-method
                                :path-suffix ',path-suffix
                                :app ,app-var
                                :fun ,fun)))
       t)))

(defmethod (setf fun) :after (new-value (handler handler))
  (configure (app handler) nil))


