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

(defun hash-suffix (suffix)
  (let ((a (char suffix 0))
        (b (char suffix 1)))
    (format nil "~A/~A/~A" a b suffix)))

(defmethod path ((output-file output-file))
  (let ((suffix (suffix output-file))
        (app (app output-file)))
    (merge-pathnames (pathname
                      (if (output-directory-hashed-p app)
                          (hash-suffix suffix)
                          suffix))
                     (output-directory app))))

(defmethod url ((output-file output-file))
  (let ((app (app output-file))
        (suffix (suffix output-file)))
    (format nil "~A~A" (output-url app) suffix)))

(defmethod download-url ((output-file output-file))
  (let ((app (app output-file)))
    (if (slot-set-p app 'download-url)
        (format nil "~A~A" (download-url app) (suffix output-file))
        (url output-file))))
