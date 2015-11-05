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

(defun enhash-legacy-directory (base file &key dry-run)
  "Convert the output directory BASE to a two-level hashed
directory. Used to prep an app for OUTPUT-DIRECTORY-HASHED-P."
  (let ((wild (make-pathname :directory '(:relative :wild-inferiors)
                             :defaults file))
        (count 0))
    (dolist (legacy-file (directory (merge-pathnames wild base)) count)
      (let* ((enough (enough-namestring legacy-file base))
             (hashed (maybe-hash-pathname enough))
             (new-file (merge-pathnames hashed base)))
        (unless (equal new-file legacy-file)
          (incf count)
          (unless dry-run
            (ensure-directories-exist new-file)
            (rename-file legacy-file new-file)))))))
