;;;; template.lisp

(in-package #:funweb)

(defvar *template-defaults* #p"template.html")

(defun find-app-template (name app)
  (let ((file
         (merge-pathnames (merge-pathnames name *template-defaults*)
                          (template-directory app))))
    (let ((html-template:*warn-on-creation* nil))
      (html-template:create-template-printer file))))

(defun fill-app-template (name app values)
  (with-output-to-string (stream)
    (html-template:fill-and-print-template (find-template name app)
                                           values
                                           :stream stream)))

(defmethod fill-template (name values)
  (fill-app-template name *app* values))
