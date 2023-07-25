;;; -*- syntax:common-lisp package:cl-pdf coding:cp932 -*-

(in-package "PDF")

(defparameter +pdfunite+ "pdfunite ~{~A ~}")

(defun escape-pathname (string)
  ;; 要求されるfile-nameのescapeの方法がcommand毎に違ふ…
  ;; package ETC の中にも同じ函數名の違ふ定義があるので注意!
  #+unix
  (loop with result = (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
      for char across (namestring string)
      if (member char '(#\space #\( #\) #\# #\; #\:)) do
        (vector-push-extend #\\ result)
      do (vector-push-extend char result)
      finally (return result))
  #+mswindows
  (format nil "\"~A\"" string))

(defun unite-pdf-command (&rest files)
  (run-shell-command (format nil +pdfunite+ (mapcar #'escape-pathname files)) :show-window nil))

(defun unite-pdf-files (&rest files)
  (apply #'unite-pdf-command files))

;;;;
;;;; 親文字揃へ
;;;;
(defun get-canonicalized-pdf (input)
  "parseに失敗したらpdfuniteに通す"
  (handler-case
      (progn
        (with-existing-document (input)
          ())                           ; do nothing
        input)
    (pdf-parse-error (cond)
      (declare (ignore cond))
      (let ((temp (make-temp-file-name)))
        (unite-pdf-files (probe-file input) temp)
        (assert (probe-file temp))
        temp))))

#||
(pdf:pdf-adjust-oyamoji "~/project/meiji-bible/_TZ_3948-DESKTOP-6TTM51T.pdf" "baz.pdf" :bottom-1 -442.5 :bottom-2 -442.5) ; 一段
||#

(defun pdf-adjust-oyamoji (input output &key (bottom-1 -214.28) ; 二段組上段行末位置
                                             (bottom-2 -466.70) ; 二段組下段行末位置
                                             (oyamoji-size 9.5862) ; 親文字フォントサイズ
                                             (ruby-size 4.7931)) ; ルビフォントサイズ
  "親文字揃へ: parse-errorがあればpdfuniteに食はせる"
  (adjust-oyamoji-aux (get-canonicalized-pdf input) output
                      :bottom-1 bottom-1 :bottom-2 bottom-2
                      :oyamoji-size oyamoji-size :ruby-size ruby-size))

(defvar *page-count*)

(defun adjust-oyamoji-aux (input output &key bottom-1 ; 二段組上段行末位置
                                             bottom-2 ; 二段組下段行末位置
                                             oyamoji-size ; 親文字フォントサイズ
                                             ruby-size)
  "親文字揃へ: dvipdfmxの出力をpdfuniteに食はせてから使ふこと"
  (with-existing-document (input)
    (let ((pages (pages (root-page *document*))))
      (loop for page across pages
          for *page-count* from 1
          do (page-adjust-oyamoji page
                                  :bottom-1 bottom-1 :bottom-2 bottom-2
                                  :oyamoji-size oyamoji-size :ruby-size ruby-size))
      (write-document output))))

(defun page-adjust-oyamoji (page &key bottom-1 ; 二段組上段行末位置
                                      bottom-2 ; 二段組下段行末位置
                                      oyamoji-size ; 親文字フォントサイズ
                                      ruby-size)
  "PDF Page の text を親文字揃へする."
  (let ((contents (extract-page-contents page)))
    (assert (vectorp contents))
    (loop for c across contents
        for pdf-stream = (content c)
        for origin = (inflate-pdf-stream pdf-stream)
        for octets = (content-adjust-oyamoji origin
                                             :bottom-1 bottom-1 :bottom-2 bottom-2
                                             :oyamoji-size oyamoji-size :ruby-size ruby-size)
        do (setf (content-of-pdf-stream pdf-stream) octets))))

;; [<1173>]=國
;; [<0279>]=空白
;; [<1173>-1000<0279>]=??
;; [<3107>]=イ
;; 片假名の傍線部分でET
;;  7.19 0 Td (右に寄せてルビを打つ)
;; -7.19 0 Td (左に寄せてルビから復帰)
;; 31be31b831ae31b531bf (まへのふみ)
;; (Td x y) 右=+x 下=-y
;; "<31cd>"  ろ
;; "<3193>"  こ
;; "<2f4a>" 踊り字
;; <319f31ad318231cc31ac> たねいれぬ
;; <31b231953197> ひさし
;; <319831b5> じふ
;; <31ab31d3> にん
;; <318031be31ca> あまり
;; "<0b06>" "-500" "<0279>" 側
;; "<348a>" 使

(defvar *font-size*)
(defvar *pos-x* 0)
(defvar *pos-y* 0)
(defvar *line-count*)
(defvar *min-y*)

;;;
;;; TmとTdのx,yの動きから改行を檢知し、行毎のtokenを集め、
;;; 行末からのルビのはみ出しと、親文字の浮きを調整する。
;;;
(defun content-adjust-oyamoji (input-buffer &key bottom-1 ; 二段組上段行末位置
                                                 bottom-2 ; 二段組下段行末位置
                                                 oyamoji-size ; 親文字フォントサイズ
                                                 ruby-size ; ルビフォントサイズ
                                                 #+ignore (line-width 16.5)) ; 行巾
  (let ((head-y 0))
    (flet ((compute-pdf-token-td (token stack output)
             (let* ((second (vector-pop stack))
                    (first (vector-pop stack))
                    (y (read-from-string second))
                    (x (read-from-string first)))
               (when (> (abs x) oyamoji-size) ; 親文字より大きな横の動きがあれば改行とみる
                 (incf *line-count*)
                 (let* ((temp (line-adjust-ruby stack head-y bottom-1 bottom-2 ruby-size))
                        (buffer (line-adjust-oyamoji temp head-y bottom-1 bottom-2 ruby-size oyamoji-size)))
                   (line-buffer-output buffer output))
                 (setq head-y *pos-y*)  ; 次の行の初期y
                 (incf *pos-x* x))
               (incf *pos-y* y)
               (vector-push-extend first stack)
               (vector-push-extend second stack)
               (vector-push-extend token stack)))
           (compute-pdf-token-tm (token stack output)
             (let* ((args (loop repeat 6 collect (vector-pop stack)))
                    (y (read-from-string (first args)))
                    (x (read-from-string (second args))))
               (unless (and (= 0 y) (= 0 x))
                 (unless (= x *pos-x*)  ; 新たに有效な Text matrix が現れれば改行とみなす
                   (incf *line-count*)
                   (let* ((temp (line-adjust-ruby stack head-y bottom-1 bottom-2 ruby-size))
                          (buffer (line-adjust-oyamoji temp head-y bottom-1 bottom-2 ruby-size oyamoji-size)))
                     (line-buffer-output buffer output))
                   (setq head-y *pos-y*) ; 次の行の初期y
                   (setq *pos-x* x)))
               (setq *pos-y* y)
               (loop for arg in (reverse args)
                   do (vector-push-extend arg stack)
                   finally (vector-push-extend token stack)))))
      (with-output-to-buffer (output)
        (with-input-from-buffer (input input-buffer)
          (loop with *line-count* = 0
              with stack = (make-array 0 :fill-pointer t :adjustable t)
              for token = (read-stream-token input)
              until (null token)
              if (equal "Tm" token) do
                (compute-pdf-token-tm token stack output)
              else if (equal "Td" token) do
                (compute-pdf-token-td token stack output)
              else do
                   (vector-push-extend token stack)
              finally (let* ((temp (line-adjust-ruby stack head-y bottom-1 bottom-2 ruby-size))
                             (buffer (line-adjust-oyamoji temp head-y bottom-1 bottom-2 ruby-size oyamoji-size)))
                        (line-buffer-output buffer output))))))))

(defun line-buffer-output (buffer output)
  (loop for token across buffer
      do (print-pdf-token token output)))

;;;
;;; 行を眺めてルビがはみ出てゐれば調整する
;;;
(defun line-adjust-ruby (buffer head-y bottom-1 bottom-2 ruby-size)
  (prog1 (line-adjust-ruby-aux buffer head-y bottom-1 bottom-2 ruby-size)
    (setf (fill-pointer buffer) 0)))

(defun dakuten-p (token)
  (or (equal token '("<0283>")) ; 濁點と半濁點
      (equal token '("<0284>"))))

(defun odoriji-p (token)
  (or (equal token '("<2f4a>"))         ; 踊り字と變體假名の場合、
      (equal token '("<ee6d>")) ; 次に濁點が來る可能性があるので覺えておく
      ;; (equal token '("<07d8>"))
      (equal token '("<ee0dee6d>"))))

(defun line-adjust-ruby-aux (buffer head-y bottom-1 bottom-2 ; 底位置(二段組上下)
                             ruby-size)
  (let ((adjust-p nil)
        (odoriji-p nil)
        (*pos-y* head-y))               ; 行開始時のy記憶
    (flet ((compute-pdf-token-tj (token stack)
             (let* ((bottom-line (if (> *pos-y* bottom-1) bottom-1 bottom-2))
                    (first (vector-pop stack))
                    (token-length (token-length first))
                    (char-bottom (- *pos-y* (* token-length ruby-size)))
                    (lower (- bottom-line (/ ruby-size 2)))
                    (upper (+ bottom-line (/ ruby-size 2)))
                    (bottom-exceed-p (and (= ruby-size *font-size*)
                                          (< lower char-bottom upper)))
                    (dakuten-p (and odoriji-p (dakuten-p first)))
                    (proceed-p (or (and adjust-p dakuten-p) ; adjust直後の濁點
                                   (and (not adjust-p) bottom-exceed-p))))
               (assert (not (and (not adjust-p) dakuten-p bottom-exceed-p)))
                                        ; adjust無しに濁點だけ底をはみ出ることは無い筈
               (if (and (not adjust-p)
                        (odoriji-p first))
                   (setq odoriji-p t)
                 (setq odoriji-p nil))
               (when proceed-p
                 (vector-push-extend "0" stack)
                 (vector-push-extend (princ-to-string ruby-size) stack)
                 (vector-push-extend "Td" stack)
                 (when (token-with-pad-p first)
                   (format t "Token Converted from ~A" first)
                   (setq first (remove-token-pads first))
                   (format t " to ~A at ~A ~A.~%" first *page-count* *line-count*)))
               (vector-push-extend first stack)
               (vector-push-extend token stack)
               (when proceed-p
                 (vector-push-extend "0" stack)
                 (vector-push-extend (princ-to-string (- ruby-size)) stack)
                 (vector-push-extend "Td" stack)
                 (setq adjust-p t)
                 (format t "~A (~A ~A) ~A (< lower=~A char-bottom=~A upper=~A)=~A ~A.~%" bottom-exceed-p *page-count* *line-count* first lower char-bottom upper (< lower char-bottom upper) bottom-line))))
           (compute-pdf-token-tf (token stack) ; font指定を記憶する
             (let ((second (vector-pop stack))
                   (first (vector-pop stack)))
               (setq *font-size* (read-from-string second))
               (vector-push-extend first stack)
               (vector-push-extend second stack)
               (vector-push-extend token stack)))
           (compute-pdf-token-td (token stack) ; y位置を記憶する
             (let ((second (vector-pop stack))
                   (first (vector-pop stack)))
               (when (not adjust-p)
                 (let ((y (read-from-string second)))
                   (unless (= y ruby-size) ; 先に調整した位置は無視 (ものすごくad-hoc)
                     (incf *pos-y* y))))
               (vector-push-extend first stack)
               (vector-push-extend second stack)
               (vector-push-extend token stack)))
           (compute-pdf-token-tm (token stack) ; y位置を記憶する
             (let* ((args (loop repeat 6 collect (vector-pop stack)))
                    (y (read-from-string (first args))))
               (when (not adjust-p)
                 (setq *pos-y* y))
               (loop for arg in (reverse args)
                   do (vector-push-extend arg stack)
                   finally (vector-push-extend token stack)))))
      (loop initially (setq adjust-p nil odoriji-p nil)
          with stack = (make-array 0 :fill-pointer t :adjustable t)
          for token across buffer
          if (equal "TJ" token) do
            (compute-pdf-token-tj token stack)
          else if (equal "Tf" token) do
            (compute-pdf-token-tf token stack)
          else if (equal "Td" token) do
            (compute-pdf-token-td token stack)
          else if (equal "Tm" token) do
            (compute-pdf-token-tm token stack)
          else do
               (vector-push-extend token stack)
          finally (return (if adjust-p
                              (let ((bottom *pos-y*))
                                        ; 二回目以降 bottom-1==bottom-2
                                (line-adjust-ruby-aux stack head-y bottom bottom ruby-size))
                            stack))))))

(defun token-with-pad-p (token)
  (if (listp token)
      (loop for elm in token
          thereis (digit-char-p (aref elm 0)))
    nil))

(defun remove-token-pads (token)
  "上へずらされたtokenは文字間を1/10に絞る"
  (if (listp token)
      (loop for elm in token
          if (digit-char-p (aref elm 0)) collect
            (princ-to-string (floor (parse-integer elm) 10))
          else collect elm)
    token))

(defun string-space-p (tok)
  (or (equal "<0279>" tok) (equal "<02790279>" tok)))

(defun string-end-space-p (tok)
  (search "0279>" tok))

(defun token-only-space (token)
  (loop for tok in token always (string-space-p tok)))

(defun canonicalize-token (token)
  "二個連續するspaceを一つに"
  (if (token-only-space token)
      '()
    (let ((reverse (reverse token)))    
      (cond ((and (string-space-p (first reverse)) ; [<0a4b> -999 <0279> 11 <0279>]
                  (string-space-p (third reverse)))
             (butlast token 2))         ; → [<0a4b> -999 <0279>]
            ((and (string-space-p (first reverse)) ; [<06480279> 16 <0279>]
                  (string-end-space-p (third reverse)))
             (butlast token 2))         ; → [<06480279>]
            (t token)))))

(defun token-length (token)
  ;; [<0b900279>]=2
  ;; [<06ea> 500 <0279>]=2.5
  ;; [<10d40e1c> -1000 <0279>]=2
  ;; [<0a4b> -999 <0279> 11 <0279>]=1.001 最後の二つ [11 <0279>] を無視
  (let ((ctoken (canonicalize-token token)))
    (if (listp ctoken)
        (reduce #'+ (mapcar #'(lambda (tok) (string-token-length tok)) ctoken))
      (string-token-length ctoken))))

(defun number-char-p (char)
  (or (digit-char-p char) (eql #\- char)))

(defun string-token-length (token)
  (cond ((eql #\< (aref token 0))       ; [<319b318231cc3182>]
         (/ (- (length token) 2) 4))
        ((number-char-p (aref token 0)) ; [<30da>10<30af>10<30cd>]
                                        ; [<0b06> -500 <0279>]
         (/ (read-from-string token) 1000.0))
        (t 0)))

(defun token-end-with-space (token)
  ;; [<0b900279>]は 0
  ;; [<06ea> 500 <0279>]は 500
  ;; [<164b> -500 <0279>]は -500
  ;; [<1908> 249 <0ebd> -749 <0279>]は -749
  ;; [<10d40e1c> -1000 <0279>]は -1000
  (let ((token (canonicalize-token token)))
    (loop with pad = 0
        for elm in token
        for head = (aref elm 0)
        if (number-char-p head) do
          (setq pad (read-from-string elm))
        else if (string-space-p elm) do
          (continue)
        else do (setq pad 0)
        finally (return (when (string-end-space-p elm) pad)))))


;;;
;;; お尻の親文字が行末から浮いてゐれば調整する
;;;
(defun line-adjust-oyamoji (buffer head-y bottom-1 bottom-2 ruby-size oyamoji-size)
  (when (canonicalize-end-of-buffer buffer) ; buffer最後が Space=<0279> で終ってゐる場合の強制處理
    (warn "Very Exceptional Case at (~A ~A)." *page-count* *line-count*))
  (prog1
      (line-adjust-oyamoji-aux buffer head-y bottom-1 bottom-2 ruby-size oyamoji-size)
    (setf (fill-pointer buffer) 0)))

(defun canonicalize-end-of-buffer (buffer)
  "buffer最後が Space=<0279> で終ってゐる場合に、直前のtokenを強制的にspaceと連結させる"
  (let ((length (length buffer)))
    (when (and (equal (aref buffer (- length 1)) "TJ")
               (equal (aref buffer (- length 2)) '("<0279>"))
               (equal (aref buffer (- length 3)) "Td")
               (< (read-from-string (aref buffer (- length 4))) -9.58)
               (equal (aref buffer (- length 6)) "Tf")
               (> (read-from-string (aref buffer (- length 7)))  9.58)
               (equal (aref buffer (- length 9)) "TJ"))
      (setf (aref buffer (- length 10))
        (append (aref buffer (- length 10)) '("<0279>"))))))

#+ignore
(defun not-oyamoji-2-p (token)
  "位置調整不要な例外的なものをここに竝べる"
  (let ((last-token (first (last token))))
    (or (equal last-token "<30e1>")     ; ふ
        (equal last-token "<30f8>")     ; れ
        (equal last-token "<30f6>")     ; り
        (equal last-token "<30ff>")     ; ん
        (equal last-token "<30c3>")     ; し
        (equal last-token "<1edc>")     ; ）
        (equal last-token "<1dd5>")     ; 、
        (equal last-token "<1ecf0279>") ; 、
        (and (equal (first (last token 3)) "<1ecf>") ; 、
             (equal last-token "<0279>"))
        (and (equal (first (last token 3)) "<1edc>") ; ）
             (equal last-token "<0279>"))
                                        ;  .....
        (or                             ; 以下「西方の人」試驗不十分
         (equal last-token "<30ad>")
         (equal last-token "<30af>")
         (equal last-token "<30cb>")
         (equal last-token "<30d7>")
         (equal last-token "<30f7>")
         (equal last-token "<30f5>")
         (equal last-token "<30b6>")
         (equal last-token "<30fe>")
         (equal last-token "<30f4>")
         (equal last-token "<1ed0>")
         (equal last-token "<1ecf>")
         (equal last-token "<1ed01ed41ed4>"))
        )))

(defun token-char-code-list (token)
  "token列を文字コードの列にする"
  (loop for tok in token
        when (char= #\< (aref tok 0)) ; 文字
          append
          (loop with value
              for i from 2 below (length tok)
              do (multiple-value-setq (value i)
                   (parse-integer tok :start (- i 1) :end (+ i 3) :radix 16))
                collect value)))

(defun not-oyamoji-p (token)
  "位置調整不要な例外的なものをここに竝べる"
  (let* ((code-list (token-char-code-list token))
         (char-code-list (remove #x0279 code-list))) ; 空白無視
    (if (null char-code-list)
        (values t nil)                ; 親文字 (漢字) ではないが、文字でもない
      (let ((code (first (last char-code-list))))
        (if (or (<= 12269 code 12868)    ; HIRAGANA KATAKANA
                (<= 20587 code 21070)    ; ALPHANUM
                (<= 515 code 1124)       ; HIRAGANA KATAKANA
                (<= 7887 code 7960)      ; DINGBATS
                (<= 12063 code 12268)    ; DINGBATS
                (<= 9276 code 9779)      ; GENERICROT
                (<= 20587 code 21070))   ; ALPHANUM
            (values t t)                 ; 親文字 (漢字) 以外の文字
          (values nil t))))))            ; 恐らく親文字 (漢字) 

(defun oyamoji-p (token)
  (multiple-value-bind (not-oyamoji-p mojip)
      (not-oyamoji-p token)
    (and mojip (not not-oyamoji-p))))

(defun line-adjust-oyamoji-aux (buffer head-y bottom-1 bottom-2 ; 底位置(二段組上下)
                                ruby-size oyamoji-size)
  (let ((adjust-p nil)
        (*pos-y* head-y))               ; 行開始時のy記憶
    (flet ((compute-pdf-token-tj (token stack)
             (let* ((bottom-line (if (> *pos-y* bottom-1) bottom-1 bottom-2))
                    (first (vector-pop stack))
                    (token-length (token-length first))
                    (char-bottom (- *pos-y* (* token-length oyamoji-size)))
                    (lower (+ bottom-line ruby-size))
                    (upper (+ bottom-line ruby-size ruby-size))
                    (diff (- char-bottom lower))
                    (bottom-float-p (and (oyamoji-p first) ; 調整は漢字のときのみ
                                         (= oyamoji-size *font-size*)
                                         (< lower char-bottom upper)
                                         (> (abs diff) 1))))
               (when (and (oyamoji-p first)
                          (= oyamoji-size *font-size*) ; 行末が空で埋めれらてゐときの處置:
                          (<= (abs diff) 1)) ; ぴったり收まってゐるが、
                 (let ((pad (token-end-with-space first))) ; tokenの最後に空白が埋められてゐる
                   (when (and pad (> pad -999))
                     (setq bottom-float-p t
                           diff (* oyamoji-size (/ (+ 1000 pad) 1000.0)))
                     (warn "Exceptional Case for ~A ~A at (~A ~A)." first pad *page-count* *line-count*))))
               (when bottom-float-p
                 (vector-push-extend "0" stack)
                 (vector-push-extend (princ-to-string (- diff)) stack)
                 (vector-push-extend "Td" stack))
               (vector-push-extend first stack)
               (vector-push-extend token stack)
               (when bottom-float-p
                 (vector-push-extend "0" stack)
                 (vector-push-extend (princ-to-string diff) stack)
                 (vector-push-extend "Td" stack)
                 (setq adjust-p t)
                 (format t "*O* ~A (~A ~A) ~A (< lower=~A char-bottom=~A upper=~A)=~A ~A.~%" bottom-float-p *page-count* *line-count* first lower char-bottom upper (< lower char-bottom upper) bottom-line))))
           (compute-pdf-token-tf (token stack) ; font指定を記憶する
             (let ((second (vector-pop stack))
                   (first (vector-pop stack)))
               (setq *font-size* (read-from-string second))
               (vector-push-extend first stack)
               (vector-push-extend second stack)
               (vector-push-extend token stack)))
           (compute-pdf-token-td (token stack) ; y位置を記憶する
             (let ((second (vector-pop stack))
                   (first (vector-pop stack)))
               (when (not adjust-p)
                 (let ((y (read-from-string second)))
                   (incf *pos-y* y)))
               (vector-push-extend first stack)
               (vector-push-extend second stack)
               (vector-push-extend token stack)))
           (compute-pdf-token-tm (token stack) ; y位置を記憶する
             (let* ((args (loop repeat 6 collect (vector-pop stack)))
                    (y (read-from-string (first args))))
               (when (not adjust-p)
                 (setq *pos-y* y))
               (loop for arg in (reverse args)
                   do (vector-push-extend arg stack)
                   finally (vector-push-extend token stack)))))
      (loop initially (setq adjust-p nil)
          with stack = (make-array 0 :fill-pointer t :adjustable t)
          for token across buffer
          if (equal "TJ" token) do
            (compute-pdf-token-tj token stack)
          else if (equal "Tf" token) do
            (compute-pdf-token-tf token stack)
          else if (equal "Td" token) do
            (compute-pdf-token-td token stack)
          else if (equal "Tm" token) do
            (compute-pdf-token-tm token stack)
          else do
               (vector-push-extend token stack)
          finally (return stack)))))

(defun print-pdf-token (token &optional (stream t))
  (cond ((stringp token)
         (write-sequence (string-to-octets (format nil " ~A" token) :external-format +octets+) stream))
        ((listp token)
         (write-sequence (string-to-octets (format nil " [~{~A~}]" token) :external-format +octets+) stream))
        (t (error "Unknown token ~A." token))))

;;;

(defun pdf-guess-line-bottom (input &key (start-page 2) (end-page 2))
  "親文字揃へ: parse-errorがあればpdfuniteに食はせる"
  (let* ((input (get-canonicalized-pdf input))
         (font-count (pdf-guess-oyamoji-size input :start-page start-page :end-page end-page)))
    (with-existing-document (input)
      (let* ((oyamoji-size (first (first font-count)))
             (line-and-page-list ; 先づ行を數へて一番多い頁だけを對象とする
               (loop for page from start-page to end-page
                     collect (cons (pdf-count-page-lines oyamoji-size :start-page page :end-page page) page)))
             (line-and-page (first (sort line-and-page-list #'> :key #'car)))
             (line-count (car line-and-page))
             (page (cdr line-and-page))
             (min-y-1                   ; 行の半分（上段部分）を走査
               (pdf-guess-line-bottom-aux oyamoji-size :start-page page :end-page page :end-line (/ (1- line-count) 2)))
             (min-y-2                   ; 全体（下段部分）を走査
               (pdf-guess-line-bottom-aux oyamoji-size :start-page page :end-page page)))
        (values min-y-1 min-y-2)))))

(defun pdf-count-page-lines (oyamoji-size &key (start-page 2) (end-page 2) (end-line 1000))
  (multiple-value-bind (min count)
      (pdf-guess-line-bottom-aux oyamoji-size :start-page start-page :end-page end-page :end-line end-line)
    (declare (ignore min))              ; 多値の二つ目だけを返す
    count))

(defun pdf-guess-line-bottom-aux (oyamoji-size &key (start-page 2) (end-page 2) (end-line 80))
  "親文字揃へ位置推定: 上段と下段の二値を返す"
  (let ((pages (pages (root-page *document*))))
    (loop with *min-y* = 0
       with *line-count* = 0
       for page across pages
       for *page-count* from 1
       when (<= start-page *page-count* end-page) do
         (page-guess-line-bottom page oyamoji-size :end-line end-line)
       finally (return (values (- *min-y* oyamoji-size) *line-count*)))))

(defun page-guess-line-bottom (page oyamoji-size &key (end-line 80))
  "PDF Page の text を親文字揃へする."
  (let ((contents (extract-page-contents page)))
    (assert (vectorp contents))
    (loop with *pos-y* = 0
        with *font-size* = 0
        for c across contents
        for pdf-stream = (content c)
        for origin = (inflate-pdf-stream pdf-stream)
        do (content-guess-line-bottom origin oyamoji-size :end end-line))))

(defun content-guess-line-bottom (input-buffer oyamoji-size &key (start 0) (end 80))
  (flet ((compute-pdf-token-tj (token stack)
           (declare (ignore token))
           (let* ((first (vector-pop stack))
                  (token-length (token-length first)))
             (when (> token-length 0)
               (when (< *pos-y* *min-y*)
                 (setq *min-y* *pos-y*)))))
         (compute-pdf-token-td (token stack)
           (declare (ignore token))
           (let* ((second (vector-pop stack))
                  (first (vector-pop stack))
                  (y (read-from-string second))
                  (x (read-from-string first)))
             (when (> (abs x) oyamoji-size) ; 行巾より大きな横の動きがあれば改行
               (incf *line-count*)
               (incf *pos-x* x))
             (incf *pos-y* y)))
         (compute-pdf-token-tm (token stack)
           (declare (ignore token))
           (let* ((args (loop repeat 6 collect (vector-pop stack)))
                  (y (read-from-string (first args)))
                  (x (read-from-string (second args))))
             (unless (and (= 0 y) (= 0 x))
               (unless (= x *pos-x*)    ; 新たに有效な Text matrix が現れれば改行とみなす
                 (incf *line-count*)
                 (setq *pos-x* x)))
             (setq *pos-y* y)))
         (compute-pdf-token-tf (token stack) ; font指定を記憶する
           (declare (ignore token))
           (let ((second (vector-pop stack))
                 (first (vector-pop stack)))
             (declare (ignore first))
             (setq *font-size* (read-from-string second)))))
    (with-input-from-buffer (input input-buffer)
      (loop with stack = (make-array 0 :fill-pointer t :adjustable t)
          for token = (read-stream-token input)
          while (and (<= start *line-count* end)
                     (not (null token)))
          if (equal "Tf" token) do
            (compute-pdf-token-tf token stack)
          else if (equal "TJ" token) do
            (compute-pdf-token-tj token stack)
          else if (equal "Tm" token) do
            (compute-pdf-token-tm token stack)
          else if (equal "Td" token) do
            (compute-pdf-token-td token stack)
          else do
               (vector-push-extend token stack)
          finally (return (values *min-y* *line-count*))))))

;;;

(defun pdf-guess-oyamoji-size (input &key (start-page 2) (end-page 2))
  "親文字揃へ: parse-errorがあればpdfuniteに食はせる"
  (let* ((hash-table (guess-oyamoji-size-aux (get-canonicalized-pdf input)
                                             :start-page start-page :end-page end-page))
         (font-count-list (loop for key being each hash-key of hash-table
                              using (hash-value value)
                              collect (cons key value))))
    (sort font-count-list #'> :key #'cdr)))

(defun guess-oyamoji-size-aux (input &key (start-page 2) (end-page 2))
  "親文字揃へ: dvipdfmxの出力をpdfuniteに食はせてから使ふこと"
  (with-existing-document (input)
    (let ((pages (pages (root-page *document*)))
          (hash-table (make-hash-table)))
      (loop for page across pages
          for *page-count* from 1
          when (<= start-page *page-count* end-page) do
            (page-guess-oyamoji-size page hash-table)
          finally (return hash-table)))))

(defun page-guess-oyamoji-size (page hash-table)
  "PDF Page の text を親文字揃へする."
  (let ((contents (extract-page-contents page)))
    (assert (vectorp contents))
    (loop for c across contents
        for pdf-stream = (content c)
        for origin = (inflate-pdf-stream pdf-stream)
        do (content-guess-oyamoji-size origin hash-table))))

(defun content-guess-oyamoji-size (input-buffer hash-table)
  (flet ((compute-pdf-token-tf (token stack) ; font指定を記憶する
           (declare (ignore token))
           (let ((second (vector-pop stack))
                 (first (vector-pop stack)))
             (declare (ignore first))
             (setq *font-size* (read-from-string second))
             (let ((n (gethash *font-size* hash-table)))
               (if (null n)
                   (setf (gethash *font-size* hash-table) 1)
                 (setf (gethash *font-size* hash-table) (1+ n)))))))
    (with-input-from-buffer (input input-buffer)
      (loop with *line-count* = 0
          with stack = (make-array 0 :fill-pointer t :adjustable t)
          for token = (read-stream-token input)
          until (null token)
          if (equal "Tf" token) do
            (compute-pdf-token-tf token stack)
          else do
               (vector-push-extend token stack)
          finally (return hash-table)))))

(defparameter +pdf2pdf+ "gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=~A -c .setpdfwrite -f ~A")
(defun pdf2pdf (pdf)
  "Canonicalize PDF"
  (let ((temp1 (swank/sbcl::temp-file-name))
        (temp2 (swank/sbcl::temp-file-name)))
    (ql-util:copy-file pdf temp1)
    (uiop/run-program:run-program (format nil +pdf2pdf+ temp2 temp1) :wait t)
    (ql-util:copy-file temp2 pdf :if-exists :overwrite)))
