;;;; utils.lisp

(in-package #:funweb)

(defvar *random-file-alphabet*
  (concatenate 'string
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "abcdefghijklmnopqrstuvwxyz"
               "0123456789"))

(defun random-file-character ()
  (aref *random-file-alphabet* (random (length *random-file-alphabet*))))

(defun random-file-string (size)
  (map-into (make-string size) #'random-file-character))

(defun make-string-table (&key case-sensitive)
  (make-hash-table :test (if case-sensitive
                             'equal
                             'equalp)))

(defun wild-equals (thing target)
  (or (eql thing t)
      (equal thing "*")
      (equal thing target)))

(defun slot-set-p (object slot-name)
  (and (slot-boundp object slot-name)
       (not (null (slot-value object slot-name)))))

(defun callfun (object)
  (lambda (fun)
    (funcall fun object)))

(defun backtrace-string ()
  (with-output-to-string (s)
    (trivial-backtrace:print-backtrace-to-stream s)))
