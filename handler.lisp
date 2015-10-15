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
    :reader fun)
   (app
    :initarg :app
    :reader app)))

(defmacro define-handler (app-name (http-method path-suffix) (&rest parameters)
                          &body body)
  (check-type http-method http-method)
  (check-type path-suffix string)
  (let ((app (find-app app-name *server*))
        (handler (gensym))
        (app-var (gensym)))
    (unless app
      (error "Unknown app -- ~S" app-name))
    `(let* ((,app-var (find-app ',app-name *server*))
            (,handler (make-instance 'handler
                                    :http-method ',http-method
                                    :path-suffix ',path-suffix
                                    :app ,app-var
                                    :fun (lambda (*request*)
                                           (let ,(loop for parameter in parameters
                                                       collect `(,parameter
                                                                 (parameter-value ',parameter *request*)))
                                             (declare (ignorable ,@parameters))
                                             ,@body)))))
       (setf (find-handler ',http-method ',path-suffix ,app-var)
             ,handler)
       t)))



