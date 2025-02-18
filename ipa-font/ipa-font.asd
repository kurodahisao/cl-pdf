;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(eval-when (:load-toplevel :execute)
  (progn (ql:quickload :asdf-encodings)
         (ql:quickload :cl-pdf-parser)
         (ql:quickload :babel)
         (ql:quickload :cl-ppcre)
         (ql:quickload :flexi-streams)
         #-allegro (ql:quickload :deflate)
         #-allegro (ql:quickload :salza2)))

(defpackage "PDF"
  (:export "LOAD-IPA-FONT"
           "REMOVE-EMBEDDED-FONTS"
           "COMPRESS-PDF-FONTS"
           "PDF2TEXT"
           "PDF-ADJUST-OYAMOJI")
  #+allegro
  (:import-from "EXCL" "STRING-TO-OCTETS" "OCTETS-TO-STRING" "WITH-INPUT-FROM-BUFFER" "WITH-OUTPUT-TO-BUFFER" "READ-VECTOR" "WRITE-VECTOR" "UNREAD-BYTE" "MATCH-REGEXP")
  #+allegro
  (:import-from "SYS" "MAKE-TEMP-FILE-NAME")
  #-allegro
  (:import-from "ASDF/BACKWARD-INTERFACE" "RUN-SHELL-COMMAND"))

(defsystem :ipa-font
  :name "cl-pdf-ipa-font"
  :author "KURODA Hisao <littlelisper@pm.me>"
  :maintainer "KURODA Hisao <littlelisper@pm.me>"
  :description "CL-PDF IPA Font Extension"
  :long-description "CL-PDF Extension for IPA Font Manipulation"
  :components ((:file "cl-pdf-patch" :depends-on ()
                      :encoding #+allegro :932 #-allegro :cp932)
               (:file "adobe-japan-1-7-ordering" :depends-on ()
                      :encoding #+allegro :932 #-allegro :cp932)
               (:file "ipa-font" :depends-on ()
                      :encoding #+allegro :932 #-allegro :cp932)
               (:file "oyamoji" :depends-on ()
                      :encoding #+allegro :932 #-allegro :cp932))
  :depends-on (:asdf-encodings :zpb-ttf2 :cl-pdf-parser :babel :cl-ppcre :flexi-streams #-allegro :deflate #-allegro :salza2))
