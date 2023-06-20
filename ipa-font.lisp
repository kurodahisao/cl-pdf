;;; -*- coding:utf-8; syntax:common-lisp -*-
;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;;
;;; (c) copyright 2016 by KURODA Hisao (littlelisper@pm.me)
;;;

(in-package "PDF")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(load-ipa-font)))

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
