;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defpackage "PDF"
  (:export "LOAD-IPA-FONT"
           "REMOVE-EMBEDDED-FONTS"
           "COMPRESS-PDF-FONTS"
           "PDF2TEXT")
  #+allegro
  (:import-from "EXCL" "STRING-TO-OCTETS" "OCTETS-TO-STRING" "WITH-INPUT-FROM-BUFFER" "WITH-OUTPUT-TO-BUFFER" "READ-VECTOR" "WRITE-VECTOR" "UNREAD-BYTE" "MATCH-REGEXP")
  #+allegro
  (:import-from "SYS" "MAKE-TEMP-FILE-NAME"))

(defsystem :ipa-font
  :name "cl-pdf-ipa-font"
  :author "KURODA Hisao <littlelisper@pm.me>"
  :maintainer "KURODA Hisao <littlelisper@pm.me>"
  :description "CL-PDF IPA Font Extension"
  :long-description "CL-PDF Extension for IPA Font Manipulation"
  :components ((:file "ipa-font" :depends-on ()
                      :encoding #+allegro :932 #-allegro :cp932))
  :depends-on (:asdf-encodings :zpb-ttf2 :cl-pdf-parser :babel :cl-ppcre :flexi-streams #-allegro :deflate #-allegro :salza2))
