;;;; serve-directory.lisp

(in-package #:funweb)

(defvar *max-url-pathname-parts* 10)

(defun parse-url-pathname-suffix (suffix &key (start 0))
  "Return a pathname composed of the parts of URL suffix SUFFIX, or
NIL if such a pathname cannot be constructed (because the suffix is
invalid)."
  (check-type suffix string)
  (block nil
    (let ((type-seen nil)
          (marks (list start))
          (mark-count 0)
          (pos start))
      (labels ((fail (reason char)
                 (return (values nil reason char)))
               (mark ()
                 (when (< *max-url-pathname-parts* (incf mark-count))
                   (fail "Too many parts" nil))
                 (push pos marks))
               (marked-parts (marks)
                 (push nil marks)
                 (let* ((parts
                         (nreverse
                          (mapcar (lambda (start end)
                                    (subseq suffix start (and end (1- end))))
                                  (rest marks) marks)))
                        (tail (last parts 2))
                        (directory (list* :relative (ldiff parts tail)))
                        (name (first tail))
                        (type (second tail)))
                   (make-pathname :name name
                                  :type type
                                  :directory directory)))
               (word-char-p (char)
                 (or (alphanumericp char)
                     (digit-char-p char)
                     (eql char #\-)))
               (in-start (char)
                 (unless (word-char-p char)
                   (fail "Bad start character" char))
                 #'in-word)
               (in-word (char)
                 (cond ((word-char-p char)
                        #'in-word)
                       ((eql char #\.)
                        #'in-type-dot)
                       ((eql char #\/)
                        #'in-separator-slash)
                       (t (fail "Bad character in word" char))))
               (in-separator-slash (char)
                 (mark)
                 (if (word-char-p char)
                     #'in-word
                     (fail "Bad character after slash" char)))
               (in-type-dot (char)
                 (mark)
                 (setf type-seen t)
                 (if (word-char-p char)
                     #'in-type
                     (fail "Bad character after type dot" char)))
               (in-type (char)
                 (if (word-char-p char)
                     #'in-type
                     (fail "Bad character in type" char))))
        (let ((state #'in-start))
          (loop for i from start below (length suffix)
                do
                (setf pos i)
                (setf state (funcall state (char suffix i)))))
        (when type-seen
          (marked-parts marks))))))

(defun make-directory-dispatcher (base-path url-prefix)
  (unless (probe-file base-path)
    (cerror "Create it" "~A cannot be probed" base-path )
    (ensure-directories-exist base-path))
  (let* ((prefix-length (length url-prefix))
         (responder (lambda (request)
                      (let ((path (parse-url-pathname-suffix (url-path request)
                                                            :start prefix-length)))
                       (when path
                         (setf path (merge-pathnames path base-path)))
                       (if (and path (probe-file path))
                           (make-file-response path)
                           (make-not-found-response)))) ))
    (lambda (request)
      (let* ((request-path (url-path request))
             (suffix-start (mismatch request-path url-prefix)))
        (when (and suffix-start
                   (= suffix-start prefix-length))
          responder)))))
