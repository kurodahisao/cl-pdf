;;; -*- coding:cp932; syntax:common-lisp -*-
;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;;
;;; (c) copyright 2016 by KURODA Hisao (littlelisper@pm.me)
;;;

(in-package "PDF")

#||
To Break Windows TTC file into TTFs, See http://yozvox.web.fc2.com/556E697465545443.html
# unzip http://yozvox.web.fc2.com/unitettc.zip
$ unitettc64.exe msmincho.ttc 
UniteTTC Copyright (C) Y.Oz 2017
Font #1: msmincho001.ttf
Font #2: msmincho002.ttf
Ok.
$ mv msmincho001.ttf msmincho.ttf 
$ mv msmincho002.ttf mspmincho.ttf 
||#

#||
Install IPA Fonts via https://moji.or.jp/ipafont/ipa00303/
See https://github.com/archimag/cl-pdf/blob/master/unicode-readme.txt
# unzip http://www.fractalconcept.com/fcweb/download/ttf2pt1.zip
$ c:/Users/kuroda/Dropbox/honda/ttf2pt1/ttf2pt1.exe -a -F c:/Windows/Fonts/ipam.ttf ipam
CL-PDF(350): (load-ipa-font "ipam.ufm" "c:/Windows/Fonts/ipam.ttf")
#<TTU-FONT-METRICS IPAMincho @ #x101cf7e22>
CL-PDF(351): (get-font "IPAMincho")
#<FONT ipamincho @ #x102d96872>
CL-PDF(352): (jexample)
#P"tmp.pdf"
||#

#-allegro
(defun make-temp-file-name ()
  (swank/sbcl::temp-file-name))
#-allegro
(defmacro with-input-from-buffer ((input buffer &rest args) &body body)
  `(flexi-streams:with-input-from-sequence (,input ,buffer ,@args) ,@body))
#-allegro
(defmacro with-output-to-buffer ((buffer &rest args) &body body)
  `(flexi-streams:with-output-to-sequence (,buffer ,@args) ,@body))
#-allegro
(defun match-regexp (regexp string)
  (cl-ppcre:scan regexp string))
#-allegro
(defun read-vector (buffer in)
  (ql-allegro:read-vector buffer in))
#-allegro
(defun write-vector (buffer out &key (start 0) (end (1- (length buffer))))
  (loop for i from 0
     for byte across buffer
     when (<= start i end) do (write-byte byte out)))
#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro octets-to-string (&rest args)
    `(sb-ext:octets-to-string ,@args))
  (defmacro string-to-octets (&rest args)
    `(sb-ext:string-to-octets ,@args)))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :deflate)
  (require :inflate))

(defclass ipa-font-metrics (ttu-font-metrics) ())

(defclass ipa-font (cid-font) ())

(defun load-ipa-font (ufm-file &optional ttf-file)
  (let ((ttufm (read-ipa-file ufm-file 'ipa-font-metrics)))
    (when ttf-file
      (with-open-file (in ttf-file :direction :input :element-type '(unsigned-byte 8))
	(setf (length1 ttufm)
	      (file-length in)
	      (binary-data ttufm)
	      (make-array (length1 ttufm) :element-type '(unsigned-byte 8)))
	(read-sequence (binary-data ttufm) in)))
    ttufm))

(defun read-ipa-file (filename &optional (font-metrics-class 'ipa-font-metrics))
  "Reorder cmap for UniJIS-UTF16-H"
  (let ((min-code #xfffe)
        (max-code 0)
        void-char encoding-vector pdf-widths font-metrics)
    (with-open-file (s filename :direction :input :external-format +external-format+)
      (setf font-metrics (afm-font-metrics s font-metrics-class)))
    (setf void-char (gethash "VoidCharacter" (characters font-metrics)))
    (iter (for (nil char-metrics) in-hashtable (characters font-metrics))
          (for gid = (index char-metrics))
          (for code = (code char-metrics))
          (for name = (name char-metrics))
          (for cid = (if (<= #x20 code #x7e)
                         (1+ (- code #x20))
                       (and (string= name "aj" :end1 2)
                            (ignore-errors (parse-integer name :start 2)))))
          (when (and (<= 0 code #xfffe) (numberp cid))
            (when (> code max-code) (setf max-code code))
            (when (< code min-code) (setf min-code code))
            (setf (aref (c2g font-metrics) (* 2 cid))
		  (code-char (ldb (byte 8 8) gid))
		  (aref (c2g font-metrics) (+ (* 2 cid) 1))
		  (code-char (ldb (byte 8 0) gid)))
	    (vector-push-extend cid (cid-widths font-metrics))
	    (vector-push-extend (vector (round (* 1000 (width char-metrics)))) (cid-widths font-metrics))))
    (setf encoding-vector (make-array (1+ max-code) :initial-element void-char)
          pdf-widths (make-array (1+ max-code) :initial-element 0))
    (iter (for (nil char-metrics) in-hashtable (characters font-metrics))
          (for code = (code char-metrics))
          (when (<= min-code code max-code)
            (setf (aref encoding-vector code) char-metrics
                  (aref pdf-widths code) (round (* 1000 (width char-metrics))))))
    (setf (min-code font-metrics) min-code
          (max-code font-metrics) max-code
          (encoding-vector font-metrics) encoding-vector
          (pdf-widths font-metrics) pdf-widths
          (encoding-scheme font-metrics) :unicode-encoding
          (gethash (string-downcase (font-name font-metrics)) *font-metrics*) font-metrics
          (leading font-metrics) (- 1 (descender font-metrics))
          (italic-sin font-metrics) (sin (/ (* pi (italic-angle font-metrics)) -180)))
    font-metrics))

(defun reduce-widths-array (widths &optional (default-width 1000))
  "Reduce CL-PDF style Width Array. (Should not be applied to Proportional Fonts).
   [ 16902 [ 1000 ] 18288 [ 1000 ] 2652 [ 1000 ] ....] →
   [ 1 95 500 325 325 500 327 389 500 ....]"
  (let ((wlist                          ; First, remove default widths from the array
         (sort (loop for pre = nil then elm
                   for elm across widths
                   if (and (vectorp elm)
                           (not (every #'(lambda (w) (= default-width w)) elm)))
                   collect (list pre elm))
               #'< :key #'first)))
    (loop with array = (make-array 0 :fill-pointer t :adjustable t)
        with start = (first (first wlist))
        with end = (first (first wlist))
        with pre-width = (second (first wlist))
        for pre-cid = start then cid
        for (cid width) in (rest wlist)
        do (if (and (eql pre-cid (1- cid))
                    (equalp width pre-width))
               (setq end cid)
             (progn                     ; pre-width must be length=1 -- Not like #(400 325 500).
               (assert (>= (length pre-width) 1))
               (incf end (1- (length pre-width)))
               (vector-push-extend start array)
               (vector-push-extend end array)
               (vector-push-extend (aref pre-width 0) array)
               (setq start cid
                     end cid
                     pre-width width)))
        finally (return array))))

(defmethod make-dictionary ((font ipa-font) &key (dw 1000) prefix &allow-other-keys)
  (make-instance
      'dictionary
    :dict-values
    `(("/Type" . "/Font")
      ("/Subtype" . "/CIDFontType2")
      ("/BaseFont" . ,(concatenate 'string "/" prefix "+" (base-font font)))
      ("/CIDSystemInfo"
       . ,(make-instance
              'dictionary
            :dict-values
            `(("/Registry" . ,(pdf-string "Adobe"))
              ("/Ordering" . ,(pdf-string "Japan1"))
              ("/Supplement" . 6))))
      ("/FontDescriptor" . ,(descriptor font))
      ("/DW" . ,dw)
      ("/W" . ,(reduce-widths-array (widths font) dw))
      ("/CIDToGIDMap"
       . ,(make-instance
              'indirect-object
            :content
            (make-instance
                'pdf-stream
              :content (c2g font)
              :no-compression (not *compress-fonts*)))))))

(defun generate-subset-prefix (&optional (number (get-universal-time)) (length 6))
  (loop for i from 0 below length
      for n = number then q
      for (q r) = (multiple-value-list (floor n 26))
      collect (code-char (+ (char-code #\A) r))))

(defmethod font-descriptor ((fm ipa-font-metrics) &key (embed *embed-fonts*) prefix &allow-other-keys)
  (flet ((conv-dim (d) (round (* 1000 d))))
    (make-instance
     'indirect-object
     :content
     (make-instance
      'dictionary ; :obj-number 0 :no-link t
      :dict-values
      `(("/Type" . "/FontDescriptor")
	("/FontName"  . ,(concatenate 'string "/" prefix "+" (font-name fm)))
	("/Flags"
	 . ,(logior
	     (if (fixed-pitch-p fm) 1 0)
	     ;; 4 ? non-ascii present
	     32
	     (if (< 0 (italic-angle fm)) 64 0)))
	("/FontBBox" . ,(map 'vector #'conv-dim (font-bbox fm)))
	("/ItalicAngle" . ,(conv-dim (italic-angle fm)))
	("/Ascent" . ,(conv-dim (ascender fm)))
	("/Descent" . ,(conv-dim (descender fm)))
	("/CapHeight" . ,(conv-dim (cap-height fm)))
	("/XHeight" . ,(conv-dim (x-height fm)))
	("/StemV" . ,10)
	,@(when (and embed (binary-data fm))
	    `(("/FontFile2"
	       . ,(make-instance
		   'indirect-object
		   :content
		   (make-instance
		    'pdf-stream
		    :content (binary-data fm)
		    :no-compression (not *compress-fonts*)
		    :dict-values `(("/Length1" . ,(length1 fm)))))))))))))

(defmethod make-dictionary ((fm ipa-font-metrics)
                            &key font (encoding (encoding font)) (embed *embed-fonts*)
                                 (prefix (generate-subset-prefix)))
  (declare (ignore encoding))
  (let* ((font-descriptor (font-descriptor fm :embed embed :prefix prefix :errorp nil))
	 (cid-font (make-instance
		    'ipa-font
		    :base-font (font-name fm)
		    :descriptor font-descriptor
		    :widths (cid-widths fm)
		    :c2g (c2g fm))))
    (make-instance
     'dictionary
     :dict-values
     `(("/Type" . "/Font")
       ("/Subtype" . ,(add-/ (font-type fm)))
       ("/BaseFont" . ,(concatenate 'string "/" prefix "+" (font-name fm)))
       ("/Encoding" . "/UniJIS-UTF16-H")
       ("/DescendantFonts"
	. ,(vector
	    (make-instance
	     'indirect-object
	     :content (make-dictionary cid-font :prefix prefix))))))))

(defun make-c2g-subset (font-metrics character-list)
  "Set c2g index to null when char not included in character-list."
  (setf (c2g font-metrics)
    (make-c2g-subset-seq font-metrics character-list)))

(defun make-c2g-subset-seq (font-metrics character-list)
  "Set c2g index to null when char not included in character-list."
  (loop with c2g = (copy-seq (c2g font-metrics))
      with character-code-list = (mapcar #'char-code character-list)
      initially (fill c2g 0)
      for name being the hash-keys of (characters font-metrics)
      using (hash-value char-metrics)
      for code = (code char-metrics)
      for gid = (index char-metrics)
      when (member code character-code-list) do
        (let ((cid (if (<= #x20 code #x7e)
                       (1+ (- code #x20))
                     (parse-integer name :start 2))))
          (setf (aref c2g (* 2 cid))
            (code-char (ldb (byte 8 8) gid))
            (aref c2g (+ (* 2 cid) 1))
            (code-char (ldb (byte 8 0) gid))))
      finally (return c2g)))

(defparameter +octets+ #+allegro :octets #-allegro :latin-1)
(defparameter +cp932+ #+allegro :932 #-allegro :cp932)

(defun string-to-vector (string)
  (if (stringp string)
      (string-to-octets string :external-format +octets+ :null-terminate nil)
    string))

(defun compress-string (string)
  (let ((input-octets (string-to-vector string)))
    (deflate-octets input-octets)))

(defun deflate-octets (input-octets &optional (type :zlib))
  #-allegro (declare (ignore type))
  #-allegro
  (let ((input (coerce input-octets '(simple-array (unsigned-byte 8) (*))))
        (chunks ()))
    (flet ((cb (octet-vector end)
             (push (subseq octet-vector 0 end) chunks)))
      (let ((compressor
             (make-instance 'salza2:zlib-compressor
                            :callback #'cb)))
        (salza2:compress-octet-vector input compressor)
        (salza2:finish-compression compressor)))
    (reverse chunks))
  #+allegro
  (with-input-from-buffer (in input-octets)
    (with-output-to-buffer (out)
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	    (deflate (make-instance 'util.zip:deflate-stream :target out :compression type)))
	(loop for bytes = (read-vector buffer in)
            until (zerop bytes) do
              (loop for pos = 0 then (write-vector buffer deflate :start pos :end bytes)
                  until (>= pos bytes)))
	(close deflate)))))

(defun deflate-file (input-filename output-filename &optional (type :zlib))
  #-allegro (declare (ignore input-filename output-filename type))
  #-allegro (error "Not Supported.")
  #+allegro
  (with-open-file (in input-filename :external-format :octets :direction :input)
    (with-open-file (out output-filename :direction :output :external-format :octets :if-exists :supersede)
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	    (deflate (make-instance 'util.zip:deflate-stream :target out :compression type)))
	(loop for bytes = (read-vector buffer in)
            until (zerop bytes) do
              (loop for pos = 0 then (write-vector buffer deflate :start pos :end bytes)
                  until (>= pos bytes)))
	(close deflate)))))             ; finish compression

;;;
;;; Remove Embedded FontFile2
;;;

(defun remove-embedded-fonts (pdf file-out &key (basefont-names
                                                 '("MS-" ; Excelの吐くFont名
                                                   "ＭＳ" ; Adobe PDF
                                                   "IPA")))
  "Remove Embedded FontFile2 and Replace double-char Font Names."
  (with-existing-document (pdf)
    (loop for object across (objects *document*)
        for content = (content object)
        for dict-values = (and (typep content 'dictionary) (dict-values content))
        for fontname = 
          (loop for dict in dict-values
              for (key . value) = dict
              when (or (string= key "/BaseFont")
                       (string= key "/FontName"))
              return (fontname-includes-name-p value basefont-names))
        when fontname do
          (loop for dict in dict-values
              for (key . value) = dict
              when (or (string= key "/BaseFont")
                       (string= key "/FontName")) do
                ;; Rename FontSubset Name e.g. /BCDGEE+MS-Gothic → /MS-Gothic
                (when (match-regexp "/[A-Z][A-Z][A-Z][A-Z][A-Z][A-Z]+.*" value)
                  (let* ((fname (subseq fontname 8)) ; 日本語フォント名を英名に置換へる
                         (rname (fontname-to-standard-name fname)))
                    (setf (cdr dict) (concatenate 'string "/" rname))))
              when (string= key "/FontFile2") do
                ;; Remove Embedded contents and its Reference
                (setf (content value) nil) ; Remove Embedded /FontFile2
                (setf (cdr dict) nil)
              when (and (string= key "/CIDToGIDMap")
                        (typep value 'indirect-object)) do
                (setf (content value) nil) ; Remove /CIDToGIDMap
                (setf (cdr dict) nil)
              when (and (not (proportional-p fontname))
                        (string= key "/W") ; Reduce /W
                        (vectorp value)) do
                ;; if every item of /W is numberp, then already reduced.
                (unless (every #'numberp value)
                  (setf (cdr dict)
                    (reduce-widths-array value)))))
    (write-document file-out)))

(defun fontname-to-standard-name (fontname)
  "日本語フォント名を英名に置換へる"
  (cond ((string= "ＭＳＰゴシック" fontname)
         "MS-PGothic")
        ((string= "ＭＳＰ明朝" fontname)
         "MS-PMincho")
        ((string= "ＭＳゴシック" fontname)
         "MS-Gothic")
        ((string= "ＭＳ明朝" fontname)
         "MS-Mincho")
        (t fontname)))

;;;
;;; Remove Embedded FontFile2 and Replace Font Name (AdobeのPSをps2pdfした出力專用)
;;; - ただし歐文フォントを取ってしまふと化ける
;;;
(defun remove-embedded-fonts-2 (pdf file-out &key (basefont-names t))
  "Remove Embedded Fonts."
  (with-existing-document (pdf)
    (loop for object across (objects *document*)
        for content = (content object)
        for dict-values = (and (typep content 'dictionary) (dict-values content))
        for registry = 
          (loop for dict in dict-values
              for (key . value) = dict
              when (and (string= key "/Registry")
                        (string= value "(Adobe)"))
              return (cons key value))
        for fontname = 
          (loop for dict in dict-values
              for (key . value) = dict
              when (or (string= key "/BaseFont")
                       (string= key "/FontName"))
              return (fontname-includes-name-p value basefont-names))
        when registry do
          (loop for dict in dict-values
              for (key . value) = dict
              when (and (string= key "/Ordering")
                        (string= value "(WinCharSetFFFF)"))
              do (setf (cdr dict) "(Identity)"))
        when fontname do
          (loop for dict in dict-values
              for (key . value) = dict
              when (or (string= key "/BaseFont")
                       (string= key "/FontName")) do
                ;; Rename FontSubset Name e.g. /BCDGEE+MS-Gothic → /MS-Gothic
                (when (match-regexp "/[A-Z][A-Z][A-Z][A-Z][A-Z][A-Z]+.*" value)
                  (let* ((fname (subseq fontname 8)) ; 日本語フォント名を英名に置換へる
                         (rname (fontname-to-standard-name fname)))
                    (setf (cdr dict) (concatenate 'string "/" rname))))
              when (string= key "/FontFile2") do
                ;; Remove Embedded contents and its Reference
                (setf (content value) nil) ; Remove Embedded /FontFile2
                (setf (cdr dict) nil)))
    (write-document file-out)))

;;;
;;; Compress Embedded FontFile2
;;;

(defun inflate-fontfile2 (fontobj)
  "Inflate FontFile2's pdf-stream."
  (inflate-pdf-stream (content (font-descendantfonts-fontfile2 fontobj))))

(defun (setf fontfile2-pdf-stream) (octets fontobj)
  "Substitute pdf-stream destructively by octets."
  (let ((pdf-stream (content (font-descendantfonts-fontfile2 fontobj))))
    ;; substitute pdf-stream
    (setf (content pdf-stream) (deflate-octets octets))
    ;; modify dict-values (avoid duplicated /Length)
    (loop with length1 = (length octets)
        for (key . value) in (dict-values pdf-stream)
        if (functionp value) collect
          (cons key value) into dict-values ; This must be /Length
        else if (string= "/Length" key) do
          (quote "Omit Length with its immediate value.")
        else if (string= "/Length1" key) collect
          (cons key length1) into dict-values
        else collect
             (cons key value) into dict-values
        finally (setf (dict-values pdf-stream) dict-values))))

(defparameter +zenkaku+ "ＭＳ")

(defparameter +zen-utf8+
  (octets-to-string (string-to-octets +zenkaku+ :external-format :utf-8)
                    :external-format +octets+))

(defparameter +zen-cp932+
  (octets-to-string (string-to-octets +zenkaku+ :external-format +cp932+)
                    :external-format +octets+))

(defun pdf-name-to-string (pdf-name)
  "PDFにoctetsで埋め込まれた名前をstringに變換する (ASCII, UTF-8 and SJIS)"
  (if (search +zen-utf8+ pdf-name)
      (octets-to-string (string-to-octets pdf-name
                                          :external-format +octets+)
                             :external-format :utf-8)
    (if (search +zen-cp932+ pdf-name)
        (octets-to-string (string-to-octets pdf-name
                                            :external-format +octets+)
                               :external-format +cp932+)
      pdf-name)))

(defun fontname-includes-name-p (pdfname names)
  (if (eql names t)
      (pdf-name-to-string pdfname)
    (loop with fontname = (pdf-name-to-string pdfname)
        for name in names
        when (search name fontname)
        return fontname)))

(defun proportional-p (fontname)
  (or (search "P" fontname)
      (search "Ｐ" fontname)))

(defun compress-pdf-fonts (pdf file-out
                           &key (table-name-list '("cvt " "fpgm" "glyf" "head" "hhea" "hmtx" "loca" "maxp" "post" "prep" "vhea" "vmtx" "cmap")) ; `cmap' needed here
                                (basefont-names '("MS-" ; Excelの吐くFont名
                                                  ;; "ＭＳ" ; Adobe PDF (※が出なくなる)
                                                  "CIDFont+" ; Microsoft Print to PDF
                                                  )))
  (with-existing-document (pdf)
    (let* ((basefonts
            (loop for fontobj in (document-fonts-with-text *document*)
                for encoding = (font-encoding fontobj)
                for basefont = (font-basefont fontobj)
                nconc (and (string= "/Identity-H" encoding)
                           (fontname-includes-name-p basefont basefont-names)
                           (list fontobj))))
           (fontfile2s                  ; 二重にFontFile2處理をしないために
            (remove-duplicates basefonts :test #'equalp :key #'font-descendantfonts)))
      (loop with temp = (make-temp-file-name)
          for fontobj in fontfile2s
          do (let* ((inflate (inflate-fontfile2 fontobj))
                    (font-loader        ; 現行のFontFile2を一旦loadし、
                     (with-input-from-buffer (input inflate)
                       (zpb-ttf2:open-font-loader-from-stream input))))
               ;; 不要なtableを除いてdumpしてから、
               (with-open-file (stream temp :direction :io :if-exists :supersede)
                 (zpb-ttf2:dump-font-loader-to-stream font-loader stream table-name-list))
               ;; 新たなFontFile2として設置しなほす
               (with-open-file (stream temp)
                 (let ((sequence (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                   (read-sequence sequence stream)
                   (setf (fontfile2-pdf-stream fontobj) sequence))))))
    (write-document file-out)))

(defun document-fonts-with-text (*document*)
  (let ((pages (pages (root-page *document*))))
    (remove-duplicates (loop for page across pages
                           append (page-fonts page)))))

(defun page-fonts (page)
  (let* ((fonts (extract-page-fonts page)))
    (loop for (fontname . fontobj) in fonts
        for tounicode = (font-tounicode-stream fontobj)
        ;; for ordering = (font-ordering fontobj)
        for encoding = (font-encoding fontobj)
        ;; for basefont = (font-basefont fontobj)
        if (and (null tounicode) (null encoding)) do
          (warn "~A has no encoding neither table." fontname)
        else collect fontobj)))

(defun font-descendantfonts-fontfile2 (fontobj)
  (fontdescriptor-fontfile2 (descendantfonts-fontdescriptor (font-descendantfonts fontobj))))

(defun fontdescriptor-fontfile2 (fontdescriptor)
  (cdr (assoc "/FontFile2" (dict-values (ensure-dictionary fontdescriptor)) :test #'string=)))

(defun descendantfonts-fontdescriptor (descendantfonts)
  (if (typep descendantfonts 'indirect-object)
      (loop for df across (content descendantfonts)
          for fontdescriptor = (descendant-fontdescriptor df)
          when (fontdescriptor-fontfile2 fontdescriptor) return fontdescriptor)
    (loop for df across descendantfonts
        for fontdescriptor = (descendant-fontdescriptor df)
        when (fontdescriptor-fontfile2 fontdescriptor) return fontdescriptor)))

(defun descendant-fontdescriptor (descendantfont)
  (cdr (assoc "/FontDescriptor" (dict-values (ensure-dictionary descendantfont)) :test #'string=)))

(defun font-fontdescriptor-fontfile2 (fontobj)
  (fontdescriptor-fontfile2 (font-fontdescriptor fontobj)))

(defun font-fontdescriptor (fontobj)
  (cdr (assoc "/FontDescriptor" (dict-values (ensure-dictionary fontobj)) :test #'string=)))

;;;
;;; compress font removing glyphs
;;;

(defun make-subset-pdf-fonts (pdf file-out
                              &key (table-name-list '("cvt " "fpgm" "glyf" "head" "hhea" "hmtx" "loca" "maxp" "post" "prep" "vhea" "vmtx"))
                                   (font-file "pdf/ipam.ttf")
                                   (font-encoding "/UniJIS-UTF16-H")
                                   (basefont-name "IPAMincho")
                                   (page-numbers t))
  (with-existing-document (pdf)
    (let ((pages (pages (root-page *document*)))
          (font-text-hash-table (make-hash-table)))
      ;; collect font and texts
      (loop for page across pages
          for i from 0
          when (or (eql page-numbers t)     ; 必要な頁が指定されてゐれば、
                   (member i page-numbers)) ; 其の頁だけを對象とする
          do (page2text page font-text-hash-table (list basefont-name)))
      ;; replace fontfile2
      (loop with temp = (make-temp-file-name)
          for fontobj being the hash-keys of font-text-hash-table
          using (hash-value text)
          for encoding = (font-encoding fontobj)
          for basefont = (font-basefont fontobj)
          when (and (string= font-encoding encoding)
                    (search basefont-name basefont))
          do (let* ((zpb-ttf2:*dump-character-list*
                     ;; Find the font using the same fontfile2 and unify the associated text
                     (collect-charset-using-the-font fontobj text font-text-hash-table))
                    (font-loader (with-open-file (input font-file)
                                   (zpb-ttf2:open-font-loader-from-stream input)))
                    (font-metrics (gethash (string-downcase basefont-name) *font-metrics*))
                    (c2g (make-c2g-subset-seq font-metrics zpb-ttf2:*dump-character-list*)))
               ;; subset化されたttfファイルをtempにdumpして、
               (with-open-file (stream temp :direction :io :if-exists :supersede)
                 (zpb-ttf2:dump-font-loader-to-stream font-loader stream table-name-list))
               ;; tempの内容をFontFile2として設置しなほす
               (with-open-file (stream temp)
                 (let ((sequence (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                   (read-sequence sequence stream)
                   (setf (fontfile2-pdf-stream fontobj) sequence)))
               ;; さらにc2gの壓縮
               (setf (cidtogidmap-pdf-stream fontobj) c2g))))
    (write-document file-out)))

(defun collect-charset-using-the-font (thefont thetext hashtable)
  "Find the font using the same fontfile2 and unify the associated text."
  (flet ((string-to-charset (string &optional rest)
           (remove-duplicates (map 'list #'identity (apply #'concatenate 'string string rest)))))
    (let ((descendantfonts (font-descendantfonts thefont)))
      (when descendantfonts
        (loop with thefontfile2 = (font-descendantfonts-fontfile2 thefont)
            for fontobj being the hash-keys of hashtable
            using (hash-value text)
            when (and (font-fontdescriptor fontobj)
                      (eql (font-fontdescriptor-fontfile2 fontobj) thefontfile2))
            collect text into collection
            finally (return (string-to-charset thetext collection)))))))

(defun (setf cidtogidmap-pdf-stream) (c2g fontobj)
  "Substitute pdf-stream destructively by octets."
  (let ((cidtogidmap (font-descendantfonts-cidtogidmap fontobj)))
    ;; substitute pdf-stream
    (setf (content cidtogidmap)
      (make-instance
          'pdf-stream
        :content c2g
        :no-compression (not *compress-fonts*)))))

(defun font-descendantfonts-cidtogidmap (fontobj)
  (descendantfonts-cidtogidmap (font-descendantfonts fontobj)))

(defun descendantfonts-cidtogidmap (descendantfonts)
  (loop for df across descendantfonts
      thereis (descendantfont-cidtogidmap df)))

(defun descendantfont-cidtogidmap (descendantfonts)
  (cdr (assoc "/CIDToGIDMap" (dict-values (ensure-dictionary descendantfonts)) :test #'string=)))

;;;
;;; Extract Texts from PDF
;;;

(defun pdf2text (pdf &key (basefont-names t) (page-numbers t))
  "PDFからfont捌にtextを抽出する."
  (with-existing-document (pdf)
    (let ((pages (pages (root-page *document*))))
      (loop with font-text-hash-table = (make-hash-table)
          for page across pages
          for i from 0
          when (or (eql page-numbers t)     ; 必要な頁が指定されてゐれば、
                   (member i page-numbers)) ; 其の頁だけを對象とする
          do (page2text page font-text-hash-table basefont-names)
          finally (return (values font-text-hash-table
                                  (loop for text being the hash-values
                                      of font-text-hash-table
                                      collect text)))))))

(defun page2text (page &optional (font-text-hash-table (make-hash-table)) (basefont-names t))
  "PDF Page からfont捌にtextを抽出する."
  (let* ((fonts (extract-page-fonts page))
         (fontalist
          (loop for (fontname . fontobj) in fonts
              for tounicode = (font-tounicode-stream fontobj)
              for ordering = (font-ordering fontobj)
              for encoding = (font-encoding fontobj)
              if (and (null tounicode) (null encoding)) do
                (warn "~A has no encoding neither table." fontname)
              else collect
                   (list fontname encoding tounicode ordering fontobj))))
    (loop for font in fontalist
        for tounicode = (third font)
        unless (null tounicode) do
          (setf (third font)
            (collect-tounicode-table tounicode)))
    (let* ((contents-buffers (collect-page-contents-buffer page))
           (concatenate-buffer (concatenate-contents-buffers contents-buffers))
           (texts-list (collect-contents-text concatenate-buffer)))
      (loop with previous = nil
          for (fontname . texts) in texts-list
          for realname = (if (null fontname)
                             (setq fontname previous)
                           (setq previous fontname))
          for fontlist = (assoc realname fontalist :test #'string=)
          for encoding = (second fontlist)
          for tounicode = (third fontlist)
          for ordering = (fourth fontlist)
          for fontobj = (fifth fontlist)
          for basefont = (and fontobj (font-basefont fontobj)) ; fontobjが取れない場合がある
          when (or (eql basefont-names t)
                   (loop for basename in basefont-names
                       thereis (search basename basefont)))
          do (let ((string (cond ((search "Identity" encoding)
                                  (if (not (null tounicode))
                                      (read-texts-as-cid-string texts tounicode)
                                    (if (search "UCS" ordering)
                                        (read-texts-as-ucs-string texts)
                                      (error "Unknown Ordering ~A when Encoding is ~A." ordering encoding))))
                                 ((search "UTF16" encoding)
                                  (read-texts-as-ucs-string texts))
                                 ((or (search "WinAnsi" encoding) (search "RKSJ" encoding))
                                  (read-texts-as-plain-text texts +cp932+))
                                 ((or (null encoding) (string= encoding "/StandardEncoding"))
                                  (read-texts-as-plain-text texts +cp932+))
                                 ((and (null encoding) (not (null tounicode)))
                                  (read-texts-as-cid-string texts tounicode))
                                 (t (error "Unknown Pattern of Encoding ~A and ToUnicode ~A" encoding tounicode)))))
               (assoc-string-to-font fontobj font-text-hash-table string))
          finally (return font-text-hash-table)))))

(defun assoc-string-to-font (font hashtable string)
  "string を font に紐付ける."
  (when (listp string)
    (setq string (apply #'concatenate 'string string)))
  (let ((value (gethash font hashtable)))
    (when (listp value)
      (setq value (apply #'concatenate 'string value)))
    (setf (gethash font hashtable) (concatenate 'string value string))))

(defun ucs-octets-to-plain-text (octets)
  (loop with string = (make-array 0 :element-type 'character :fill-pointer t :adjustable t)
      for i from 0 by 2 below (length octets)
      do (let ((oct0 (aref octets i))
               (oct1 (aref octets (1+ i))))
           (let ((code (dpb oct0 (byte 16 8) oct1)))
             (vector-push-extend (code-char code) string)))
      finally (return string)))


(defun string-2byte-code-to-octets (string &key (start 0) (end (length string)))
  (loop with *read-base* = 16
      with octets = (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t)
      for i from start by 2 below end
      do (vector-push-extend (read-from-string string nil nil :start i :end (+ i 2))
                             octets)
      finally (return octets)))

(defun ucs-code-string-to-plain-text (string)
  (let ((octets (string-2byte-code-to-octets string :start 1 :end (1- (length string)))))
    (ucs-octets-to-plain-text octets)))

(defun ucs-string-to-plain-text (text)
  (cond ((typep text '(array (unsigned-byte 8) (*)))
         (babel:octets-to-string text :encoding :utf-16))
        ((and (stringp text)
              (eql #\< (char text 0)))
         (ucs-code-string-to-plain-text text))
        ((and (stringp text)
              (eql #\( (char text 0)))
         (let ((octets (string-char-code-to-octets text :start 1 :end (1- (length text)))))
           (babel:octets-to-string octets :encoding :utf-16)))))

(defun read-texts-as-ucs-string (texts)
  (if (listp texts)
      (loop for tex in texts
          collect (ucs-string-to-plain-text tex))
    (ucs-string-to-plain-text texts)))

(defun cid-octets-to-plain-text-1 (octets tounicode)
  (loop with *read-base* = 16
      with string = (make-array 0 :element-type 'character :fill-pointer t :adjustable t)
      for oct across octets
      do (let* ((cid (format nil "<~2,'0X>" oct))
                (assoc (assoc cid tounicode :test #'string-equal)))
           (if (null assoc)
               (vector-push-extend (code-char oct) string)
             (let* ((unicode (first (last assoc)))
                    (code (read-from-string unicode nil nil :start 1 :end 5)))
               (vector-push-extend (code-char code) string))))
      finally (return string)))

(defun cid-octets-to-plain-text-2 (octets tounicode)
  (loop with *read-base* = 16
      with string = (make-array 0 :element-type 'character :fill-pointer t :adjustable t)
      for i from 0 by 2 below (length octets)
      do (let* ((oct0 (aref octets i))
                (oct1 (aref octets (1+ i)))
                (code (dpb oct0 (byte 16 8) oct1)))
           (loop for uelm in tounicode
               for code1 = (first uelm)
               for code2 = (if (= (length uelm) 3) (second uelm) code1)
               for src1 = (read-from-string code1 nil nil :start 1 :end 5)
               for src2 = (read-from-string code2 nil nil :start 1 :end 5)
               when (<= src1 code src2) return
                 ;; http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/pdf_reference_1-7.pdf
                 ;; P. 474-475
                 (let* ((lastelm (first (last uelm)))
                        (dest
                         (if (listp lastelm)
                             (let* ((m (- code src1))
                                    (unicode (nth m lastelm)))
                               (+ (read-from-string unicode nil nil :start 1 :end 5)))
                           (let ((unicode lastelm))
                             (+ (read-from-string unicode nil nil :start 1 :end 5)
                                (- code src1))))))
                   (vector-push-extend (code-char dest) string))
               finally (warn "CID: (~A . ~A) = <~4,'0X> Cannot Decode." oct0 oct1 code)))
      finally (return string)))

(defun cid-octets-to-plain-text (octets tounicode)
  (if (= (length (first (first tounicode))) 4) ; <00> or <0000>
      (cid-octets-to-plain-text-1 octets tounicode)
    (cid-octets-to-plain-text-2 octets tounicode)))

(defun cid-code-string-to-plain-text (string tounicode)
  (let ((octets (string-2byte-code-to-octets string :start 1 :end (1- (length string)))))
    (cid-octets-to-plain-text octets tounicode)))

(defun string-char-code-to-octets (string &key (start 0) (end (length string)))
  (loop with octets = (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t)
      for i from start below end
      for code = (char-code (aref string i))
      do (vector-push-extend code octets)
      finally (return octets)))

(defun cid-string-to-plain-text (text tounicode)
  (cond ((typep text '(array (unsigned-byte 8) (*)))
         (cid-octets-to-plain-text text tounicode))
        ((and (stringp text)
              (eql #\< (char text 0)))
         (cid-code-string-to-plain-text text tounicode))
        ((and (stringp text)
              (eql #\( (char text 0)))
         (let ((octets (string-char-code-to-octets text :start 1 :end (1- (length text)))))
           (cid-octets-to-plain-text octets tounicode)))))

(defun read-texts-as-cid-string (texts tounicode)
  (if (listp texts)
      (loop for text in texts
          collect (cid-string-to-plain-text text tounicode))
    (cid-string-to-plain-text texts tounicode)))

(defun text-to-plain-text (text encoding)
  (cond ((typep text '(array (unsigned-byte 8) (*)))
         (babel:octets-to-string text :encoding encoding))
        ((and (stringp text)
              (eql #\< (char text 0)))
         (let ((octets (string-2byte-code-to-octets text :start 1 :end (1- (length text)))))
           (babel:octets-to-string octets :encoding encoding)))
        ((and (stringp text)
              (eql #\( (char text 0)))
         (let ((octets (string-char-code-to-octets text :start 1 :end (1- (length text)))))
           (babel:octets-to-string octets :encoding encoding)))))

(defun read-texts-as-plain-text (texts encoding)
  (if (listp texts)
      (loop for text in texts
          collect (text-to-plain-text text encoding))
    (text-to-plain-text texts encoding)))

(defun concatenate-contents-buffers (buffer-list)
  (loop for string in buffer-list
      for buffer = (string-to-vector string) then 
        (concatenate 'vector buffer (string-to-vector string))
      finally (return (coerce buffer '(array (unsigned-byte 8) (*))))))

(defun collect-contents-text (string)
  (let ((input-buffer (string-to-vector string)))
    (with-input-from-buffer (stream input-buffer)
      (loop for token = (read-stream-token stream)
          until (null token)
          if (equal "BI" token) do    ; `EI'まで讀み飛ばし
            (extract-contents-image stream)
          else if (equal "BT" token) append ; `ET'までTextを囘收
            (extract-contents-text stream)))))

(defun extract-contents-text (stream)
  "`ET'に出合ふまでTextを囘收する"
  (loop with fontname
      with stack = (make-array 0 :fill-pointer t :adjustable t)
      for token = (read-stream-token stream)
      until (equal "ET" token)
      if (equal "Tj" token) collect
        (cons fontname (vector-pop stack)) into text-list
      else if (equal "TJ" token) collect
        (cons fontname (vector-pop stack)) into text-list
      else if (equal "'" token) collect
        (cons fontname (vector-pop stack)) into text-list
      else if (equal "\"" token) collect
        (prog1 (cons fontname (vector-pop stack))
          (vector-pop stack)
          (vector-pop stack)) into text-list
      else if (equal "Tf" token) do
        (setq fontname 
          (progn (vector-pop stack)
                 (vector-pop stack)))
      else if (equal "Tm" token) do
        (loop repeat 6 do (vector-pop stack))
      else if (equal "TL" token) do
        (vector-pop stack)
      else if (equal "Tc" token) do
        (vector-pop stack)
      else if (equal "Tw" token) do
        (vector-pop stack)
      else if (equal "Tr" token) do
        (vector-pop stack)
      else if (equal "Ts" token) do
        (vector-pop stack)
      else if (equal "Tz" token) do
        (vector-pop stack)
      else if (equal "Td" token) do
        (progn (vector-pop stack) (vector-pop stack))
      else if (equal "TD" token) do
        (progn (vector-pop stack) (vector-pop stack))
      else if (equal "T*" token) do
        (quote "Do Nothing")
      else do
           (vector-push-extend token stack)
      finally (return text-list)))

(defun extract-contents-image (stream)
  "`EI'まで讀み飛ばし"
  (loop for c1 = nil then c2
      for c2 = nil then c3
      for c3 = (read-char stream)
      until (and (eql c1 #\newline) (eql c2 #\E) (eql c3 #\I))))

(defun font-tounicode-stream (fontname)
  (let ((content (font-tounicode fontname)))
    (when content
      (inflate-pdf-stream content))))

(defun font-encoding (fontobj)
  (let ((dictionary (ensure-dictionary fontobj)))
    (when dictionary
      (cdr (assoc "/Encoding" (dict-values dictionary) :test #'string=)))))

(defun font-basefont (fontobj)
  (cdr (assoc "/BaseFont" (dict-values (ensure-dictionary fontobj)) :test #'string=)))

(defun font-descendantfonts (fontobj)
  (let ((dictionary (ensure-dictionary fontobj)))
    (when dictionary
      (cdr (assoc "/DescendantFonts" (dict-values dictionary) :test #'string=)))))

(defun cidsysteminfo-ordering (cidsysteminfo)
  (cdr (assoc "/Ordering" (dict-values (ensure-dictionary cidsysteminfo)) :test #'string=)))

(defun descendant-cidsysteminfo (descendantfont)
  (cdr (assoc "/CIDSystemInfo" (dict-values (ensure-dictionary descendantfont)) :test #'string=)))

(defun font-ordering (fontobj)
  (let* ((descendantfonts (font-descendantfonts fontobj))
         (font-array (if (typep descendantfonts 'indirect-object)
                         (content descendantfonts)
                       descendantfonts)))
    (assert (or (null font-array) (= 1 (length font-array)))
        (font-array) "Not Supported multiple descendantfonts. ~A" font-array)
    (loop for df across font-array
        for cidsysteminfo = (descendant-cidsysteminfo df)
        thereis (cidsysteminfo-ordering cidsysteminfo))))

(defun font-tounicode (fontobj)
  (let* ((dictionary (ensure-dictionary fontobj))
         (tounicode (and dictionary (cdr (assoc "/ToUnicode" (dict-values dictionary) :test #'string=)))))
    (when tounicode (content tounicode))))

(defun collect-tounicode-table (buffer)
  (with-input-from-buffer (stream (string-to-vector buffer))
    (loop for table = (read-tounicode-table stream)
        while table append table)))

(defun read-tounicode-table (stream)
  (let ((begin (loop for token = (or (read-stream-token stream)
                                     (return-from read-tounicode-table nil))
                   until (or (string= token "beginbfchar")
                             (string= token "beginbfrange"))
                   finally (return token))))
    (if (string= begin "beginbfchar")
        (loop for token = (read-stream-token stream)
            until (string= token "endbfchar") collect
              (list token (read-stream-token stream)))
      (loop for token = (read-stream-token stream)
          until (string= token "endbfrange") collect
            (let ((src1 token)
                  (src2 (read-stream-token stream))
                  (src3 (read-stream-token stream)))
              (when (eql token #\[)
                (setq src3 (read-pdf-array stream)))
              (list src1 src2 src3))))))

(defun extract-page-resource (page)
  (cdr (assoc "/Resources" (dict-values (content page)) :test #'string=)))

(defun extract-page-fonts (page)
  (let* ((dictionary (ensure-dictionary (extract-page-resource page)))
         (resource (and dictionary (dict-values dictionary)))
         (font (cdr (assoc "/Font" resource :test #'string=))))
    (if font
        (dict-values (ensure-dictionary font))
      (let ((xobject (cdr (assoc "/XObject" resource :test #'string=))))
        (when xobject
          (loop for xvalue in (dict-values xobject)
              append (extract-page-fonts (cdr xvalue))))))))

(defun collect-page-contents-buffer (page)
  "注: content が content (pdf-stream) の中にobject指定されてゐる場合にはbuffer抽出できない"
  (let ((contents (extract-page-contents page)))
    (if (vectorp contents)
        (loop for c across contents
            collect (inflate-pdf-stream (content c)))
      (let* ((resource (dict-values (ensure-dictionary (extract-page-resource page))))
             (xobject (cdr (assoc "/XObject" resource :test #'string=))))
        (if (and xobject (dict-values xobject))
            (loop for xvalue in (dict-values xobject)
                for object = (cdr xvalue)
                collect (inflate-pdf-stream (content object)))
          (unless (null contents)
            (list (inflate-pdf-stream (content contents)))))))))

(defun extract-page-contents (page)
  (cdr (assoc "/Contents" (dict-values (content page)) :test #'string=)))

(defun filter-of-pdf-stream (stream)
  (cdr (assoc "/Filter" (dict-values stream) :test #'string=)))

(defun content-of-pdf-stream (stream)
  (content stream))

(defun (setf content-of-pdf-stream) (octets stream)
  (setf (content stream) (deflate-octets octets)))

(defun no-compression-of-pdf-stream (stream)
  (no-compression stream))

(defun inflate-pdf-stream (pdf-stream &optional (type :zlib))
  #-allegro (declare (ignore type))
  (let ((filter (filter-of-pdf-stream pdf-stream))
        (content (content-of-pdf-stream pdf-stream)))
    (cond ((null filter)
           (format t ";; Inside non-filter file~%")
           (apply #'concatenate (type-of (first content)) content))
          ((string= filter "/FlateDecode")
           (format t ";; Inside inflate-file~%")
           (with-output-to-buffer (out)
             (loop for c in content
                for stream = (string-to-vector c)
                do #-allegro
                  (with-input-from-buffer (in stream)
                    (deflate:inflate-zlib-stream in out))
                  #+allegro
                  (with-input-from-buffer (in stream)
                    (let ((inflate (make-instance 'util.zip:inflate-stream
                                                  :compression type :input-handle in)))
                      (loop for byte = (read-byte inflate nil nil)
                         while byte do (write-byte byte out)))))))
          (t (error "Filter ~A not supported." filter)))))

(defun read-pdf-array (stream)
  (loop for token = (read-stream-token stream)
      until (eql token #\])
      if (eql token #\[) do
        (error "READ-PDF-ARRAY #\\[ Should not be Found here.")
      else if (null token) do
        (warn "READ-PDF-ARRAY: #\\] Not Found.")
        (loop-finish)
      else collect
           token into token-list
      finally (return token-list)))

(defun read-pdf-code-string (stream)
  (loop with string = (make-array 1  :initial-element #\< :element-type 'character :fill-pointer t :adjustable t)
      for c = (code-char (read-byte stream))
      until (eql c #\>) do
        (vector-push-extend c string)
      finally (progn (vector-push-extend #\> string)
                     (return string))))

(defun read-byte-char (stream)
  (code-char (read-byte stream)))

(defun read-pdf-char-string (stream)
  (loop with string = (make-array 1 :element-type 'character :initial-element #\( :fill-pointer t :adjustable t)
      for c = (read-byte-char stream)
      until (eql c #\)) do
        (let ((ignore nil))
          (when (eql #\\ c)
            (setq c (read-byte-char stream))
            ;; \n Line feed (LF)
            ;; \r Carriage return (CR)
            ;; \t Horizontal tab (HT)
            ;; \b Backspace (BS)
            ;; \f Form feed (FF)
            ;; \( Left parenthesis
            ;; \) Right parenthesis
            ;; \\ Backslash
            ;; \ddd Character code ddd (octal)
            (case c
              (#\n (setq c #\linefeed))
              (#\r (setq c #\return))
              (#\t (setq c #\tab))
              (#\b (setq c #\backspace))
              (#\f (setq c #\page))
              (#\( (setq c #\())
              (#\) (setq c #\)))
              (#\\ (setq c #\\))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (let* ((c0 c)
                      (c1 (read-byte-char stream))
                      (c2 (read-byte-char stream)))
                 (setq c
                   (code-char (read-from-string (format nil "#o~C~C~C" c0 c1 c2))))))
              (t (setq ignore t))))
          (when (null ignore)
            (vector-push-extend c string)))
      finally (progn (vector-push-extend #\) string)
                     (return string))))

(defun read-pdf-name (stream)
  (loop with string = (make-array 1 :initial-element #\/ :element-type 'character :fill-pointer t :adjustable t)
      for c = (code-char (read-byte stream))
      until (white-char-p c) do
        (vector-push-extend c string)
      finally (return string)))

(defun key-char-p (c)
  (case c
      (#\[ t)
      (#\] t)
      (#\< t)
      (#\( t)
      (#\/ t)
      (t nil)))

#-allegro
(defmacro unread-byte (stream) nil)

(defun pseudo-read-byte (stream error-p eof-value)
  #+allegro (read-byte stream error-p eof-value)
  #-allegro (flexi-streams:peek-byte stream error-p eof-value))

(defun read-stream-token (stream)
  (let ((c (loop for c = (or (read-byte stream nil nil)
                             (return-from read-stream-token nil))
               while (white-char-p (code-char c))
               finally (return c))))
    (case (code-char c)
      (#\[ (read-pdf-array stream))
      (#\] #\])
      (#\< (read-pdf-code-string stream))
      (#\( (read-pdf-char-string stream))
      (#\/ (read-pdf-name stream))
      (t (loop with string = (make-array 1 :initial-element (code-char c) :element-type 'character :fill-pointer t :adjustable t)
             for byte = (pseudo-read-byte stream nil nil)
             for c = (if byte (code-char byte))
             if (null byte) do
               (return string)
             else if (or (white-char-p c) (key-char-p c)) do
               (unread-byte stream)
               (return string) 
             else do
                  (read-byte stream)    ; skip
                  (vector-push-extend c string))))))

;;;;
;;;; Examples
;;;;

#||

(defparameter +sample-string-list+
    '(" !@#$%^&*()_+"
      "　！＠＃＄％＾＆＊（）＿＋｜→"
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐ"
      "ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰ"
      "0123456789-=`"
      "０１２３４５６７８９‐＝"
      "ｲﾛﾊﾆﾎﾍﾄﾁﾘﾇﾙｦ"
      "色は匂へど散りぬるを"
      "我が世誰ぞ常ならむ"
      "有爲の奧山今日越えて"
      "淺き夢見じ醉ひもせず"
      "いろはにほへとちりぬるを"
      "わかよたれそつねならむ" 
      "うゐのおくやまけふこえて"
      "あさきゆめみしゑひもせす"))

(defun jexample (&optional (file #P"tmp.pdf"))
  (with-document ()
    (with-page ()
      (with-outline-level ("Example" (register-page-reference))
	(let ((mincho (get-font "IPAMincho")))
          #+ignore
          (loop with x = 0
              for y from 800 by 40 downto 0
              for text in +sample-string-list+
              do (in-text-mode
                  (set-font mincho 36.0)
                  (move-text x y)
                  (draw-text text)))
          (loop initially (translate 230 500)
              repeat 150
              for i = 0.67 then (* i 1.045)
              do (in-text-mode
                  (set-font mincho i)
                  (set-rgb-fill (/ (random 255) 255.0)
                                (/ (random 255) 255.0) (/ (random 255) 255.0))
                  (move-text (* i 3) 0)
                  (show-text (nth (random 8) (last +sample-string-list+ 8))))
                 (rotate 13)))))
    (write-document file)))

||#
