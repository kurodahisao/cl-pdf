;;; -*- syntax:common-lisp package:cl-pdf -*-

(in-package "PDF")

(defun read-line-safe (stream)
  (handler-case
      (octets-to-string (coerce (loop for byte = (read-byte stream)
                                      until (or (eql byte (char-code #\return))
                                                (eql byte (char-code #\linefeed)))
                                      collect byte)
                                '(vector (unsigned-byte 8))))
    (end-of-file (c)
      (declare (ignore c))
      nil)
    (error (c)
      (declare (ignore c))
      ;; (warn "~A" c)
      "")))

(defun read-line* (stream)
  (handler-case
      (coerce (loop for char = (read-char stream)
                    until (or (eql char #\return)
                              (eql char #\linefeed))
                    collect char)
              'string)
    (end-of-file (c) (declare (ignore c)) nil)))

(defun make-xref (stream)
  (file-position stream 0)
  (loop for position = (file-position stream)
        for line = (read-line* stream)
        for (num zero obj) = (ignore-errors
                              (with-input-from-string (i line)
                                (list (read i)
                                      (read i)
                                      (read i))))
        while line
        when (search "/Root" line) collect
          (list position line) into root-list
        when (and (numberp num) (numberp zero) (eql 'obj obj)) collect
          (list position num zero obj) into object-list
        finally (return (values object-list root-list))))

(defun find-and-read-trailer ()
  (handler-case
      (read-xref-and-trailer (find-cross-reference-start))
    ((or unexpected-eof-error pdf-parse-error) (c)
      (declare (ignore c))
      (multiple-value-bind (object-list root-list)
          (make-xref *pdf-input-stream*)
        (loop for (position gen zero obj) in object-list
              do (make-indirect-object gen zero position))
        (loop for (position string) in root-list
              collect (read-trailer position) into trailer
              finally (return (first trailer)))))))

(defun read-pdf ()
  (let ((trailer (find-and-read-trailer)))
    (setf (catalog *document*) (get-dict-value trailer "/Root")
          (docinfo *document*) (get-dict-value trailer "/Info"))
    (load-all-indirect-objects)
    (print (catalog *document*))
    (print (content (catalog *document*)))
    (print (get-dict-value (content (catalog *document*)) "/Pages"))
    (let ((root-page-node
           (change-class (get-dict-value (content (catalog *document*)) "/Pages")
                         'page-node))
	  (pages-vector (make-array 1 :fill-pointer 0 :adjustable t)))
      (collect-pages root-page-node pages-vector root-page-node)
      (setf (pages root-page-node) pages-vector)
      (change-dict-value (content root-page-node) "/Count"
                         (lambda () (length (pages root-page-node))))
      (change-dict-value (content root-page-node) "/Kids" (pages root-page-node))
      (renumber-all-indirect-objects)
      (setf (root-page *document*) root-page-node))))

(defun read-trailer (&optional position)
  (if position
      (file-position *pdf-input-stream* position)
      (progn
        (eat-keyword "trailer")
        (skip-whitespace t)))
  (eat-chars "<<")
  (read-dictionary))
