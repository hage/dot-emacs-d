;;; package --- east asian ambiguous settings
;;; Commentary:
;;; cf.  http://attonblog.blogspot.jp/2014/03/unicode-east-asian-ambiguous.html

;;; Code:
(defun set-east-asian-ambiguous-width (width)
  "Emacs のバージョンに依存せず east-asian-ambiguous の幅を WIDTH に設定する."
  (cond ((= emacs-major-version 22) (set-east-asian-ambiguous-width-22 width))
        ((> emacs-major-version 22) (set-east-asian-ambiguous-width-23 width))))

;; for emacs 22
(defun set-east-asian-ambiguous-width-22 (width)
  "Emacs 22 の east-asian-ambiguous の幅を WIDTH に設定する."
  (if (= width 2)
      (utf-translate-cjk-set-unicode-range
       '((#x00a2 . #x00a3)
         (#x00a7 . #x00a8)
         (#x00ac . #x00ac)
         (#x00b0 . #x00b1)
         (#x00b4 . #x00b4)
         (#x00b6 . #x00b6)
         (#x00d7 . #x00d7)
         (#X00f7 . #x00f7)
         (#x0370 . #x03ff)
         (#x0400 . #x04FF)
         (#x2000 . #x206F)
         (#x2100 . #x214F)
         (#x2190 . #x21FF)
         (#x2200 . #x22FF)
         (#x2300 . #x23FF)
         (#x2500 . #x257F)
         (#x25A0 . #x25FF)
         (#x2600 . #x26FF)
         (#x2e80 . #xd7a3) (#xff00 . #xffef)))))

;; for over 23 (checked work in emacs 24)
(defun set-east-asian-ambiguous-width-23 (width)
  "Emacs 23 の east-asian-ambiguous の幅を WIDTH に設定する."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (dolist (range
             '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
                      (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
                      #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
                      (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
                      (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
                      #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
                      (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
                      (#x0148 . #x014B) #x014D (#x0152 . #x0153)
                      (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
                      #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
                      (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
                      #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
                      (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
                      (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
                      (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
                      (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
                      #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
                      #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
                      #x212B (#x2153 . #x2154) (#x215B . #x215E)
                      (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
                      (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
                      (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
                      #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
                      (#x2227 . #x222C) #x222E (#x2234 . #x2237)
                      (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
                      (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
                      (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
                      #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
                      (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
                      (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
                      (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
                      (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
                      (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
                      (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
                      #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
                      (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
                      (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
                      #xFFFD
                      ))
      (set-char-table-range table range width))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

(set-east-asian-ambiguous-width 2)

(provide 'east-asian-ambiguous)
;;; east-asian-ambiguous.el ends here
