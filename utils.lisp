;;;; utils.lisp

(in-package #:funweb)

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
