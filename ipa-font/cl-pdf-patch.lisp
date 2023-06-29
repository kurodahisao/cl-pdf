;;; -*- coding:cp932; syntax:common-lisp -*-
;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;;
;;; (c) copyright 2016 by KURODA Hisao (littlelisper@pm.me)
;;;

(in-package "PDF")

(defclass pseudo-font-object (object-ref)
  ((name :accessor name :initform (gen-name "/CLF") :initarg :name)))

(defun find-font-object (font &key (embed *embed-fonts*))
  (let ((font-object (cdr (assoc font (fonts *document*)))))
    (unless font-object
      (let* ((font-name (format nil "/CLF~A" (name font)))
             (font-indirect-object (find-font-resources font-name)))
        ;; If the existing-document already has the same font,
        ;; It will make PSEUDO-FONT-OBJECT instead of FONT-OBJECT.
        (if (null font-indirect-object)
            (progn
              (setf font-object (make-instance 'font-object :font font
                                        ; font has own unique name
                                  :name font-name :embed embed))
              (push (cons font font-object) (fonts *document*)))
          (setf font-object (make-instance 'pseudo-font-object
                              :obj-number (obj-number font-indirect-object)
                              :gen-number (gen-number font-indirect-object)
                              :name font-name)))))
    font-object))

(defun add-font-to-page (font &key (embed *embed-fonts*))
  (let ((font-object (cdr (assoc font (fonts *page*)))))
    (unless font-object
      (setf font-object (find-font-object font :embed embed))
      (push (cons font font-object) (fonts *page*))
      ;; To avoid duplicates, call CHANGE-DICT-VALUE instead of ADD-DICT-VALUE
      (change-dict-value (font-objects *page*) (name font-object) font-object))
    font-object))


(defmethod initialize-instance :after ((obj pdf-stream) &key empty &allow-other-keys)
  (unless empty
    (change-dict-value obj "/Length"    ; To avoid duplicate /Length
                       #'(lambda ()
                           (let ((content (content obj)))
                             (if (consp content)
                                 (reduce #'+ content :key #'length)
                               (length content)))))))

;;;
;;; Follows are New Definitions
;;;

(defun change-page-dict-value (page key new-value)
  "Look KEY up in the page dictionary.  If it is not found, look it up
in the parent page dictionary, and change its original value with new-value."
  ;; Some values in a page dictionary can be inherited from the parent
  ;; page's dictionary, such as the MediaBox.  This is a handy way to
  ;; look them up.  See PDF-REF Table 3.18.
  (let ((value (resolve-dict-value page key)))
    (if value
        (change-dict-value page key new-value)
      (let ((parent-page (resolve-dict-value page "/Parent")))
        (when parent-page
          (change-page-dict-value parent-page key new-value))))))

(defun find-font-resources (fontname)
  (let* ((resources (get-dict-value (content *page*) "/Resources"))
         (fonts (get-dict-value resources "/Font")))
    (loop for (name . object) in (dict-values fonts)
        when (string= fontname name)
        return object)))
