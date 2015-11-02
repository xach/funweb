;;;; output.lisp

(in-package #:funweb)

(defclass output-file ()
  ((app
    :initarg :app
    :reader app)
   (suffix
    :initarg :suffix
    :reader suffix)))

(defmethod make-output-file ((app app) name type)
  (let ((suffix (format nil "~A/~A.~A"
                        (random-file-string 8)
                        name
                        type)))
    (make-instance 'output-file
                   :suffix suffix
                   :app app)))


(defmethod path ((output-file output-file))
  (merge-pathnames (pathname (suffix output-file))
                   (output-directory (app output-file))))

(defmethod url ((output-file output-file))
  (format nil "~A~A" (output-url (app output-file))
          (suffix output-file)))

(defmethod download-url ((output-file output-file))
  (let ((app (app output-file)))
    (if (slot-set-p app 'download-url)
        (format nil "~A~A" (download-url app) (suffix output-file))
        (url output-file))))
