;;; tura-yatex.el -- YaTeX関係の設定
(when (autoload-if-found 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (add-to-list 'auto-mode-alist '("\\.tex$" . yatex-mode))
  (eval-after-load "yatex"
    #'(progn
        (set-face-foreground 'YaTeX-font-lock-label-face "tomato1")
        (set-face-foreground 'YaTeX-font-lock-declaration-face "skyblue1")
        (set-face-foreground 'YaTeX-font-lock-crossref-face "palegreen2")
        (setq yatex-fill-column-width 126)
        (add-hook 'yatex-mode-hook
                  #'(lambda ()
                      (progn (set-fill-column yatex-fill-column-width)
                             (append env-table '(("histitem")))
                             (local-set-key "\M-q" 'fill-paragraph))))
    
        ;; histitem環境でM-RETしたときに、自動的に\\itemをつける
        (defun YaTeX-intelligent-newline-histitem ()
          (progn
            (insert "\\item ")
            (YaTeX-indent-line)))

        (defun YaTeX::aref (argp &optional labelcmd refcmd)
          (YaTeX::ref argp labelcmd refcmd))
        (defun YaTeX::fref (argp &optional labelcmd refcmd)
          (YaTeX::ref argp labelcmd refcmd))

        (cond
         ((eq system-type 'windows-nt)
          (setq
           tex-command "c:/bin/tex/bin/platex"
           dvi2-command "c:/bin/dviout/dviout"
           latex-message-kanji-code 1
           YaTeX-user-completion-table "~/.emacs/yatexrc"
           YaTeX-hilit-sectioning-face '(navy/DarkOliveGreen1 LightGray/Black)
           YaTeX-auto-math-mode nil
           YaTeX-kanji-code 1
           YaTeX-use-LaTeX2e t
           YaTeX::ref-labeling-section-level 4
           YaTeX-template-file "~/sys/template/report.tex"))
         ((equal system-type 'darwin)
          (setq
           tex-command "/Applications/UpTeX.app/teTeX/bin/platex"
           dvi2-command "open -a Skim"
           latex-message-kanji-code 4
           YaTeX-user-completion-table "~/.emacs.d/yatexrc"
           YaTeX-hilit-sectioning-face '(navy/DarkOliveGreen1 LightGray/Black)
           YaTeX-auto-math-mode nil
           YaTeX-kanji-code 4
           YaTeX-use-LaTeX2e t
           YaTeX::ref-labeling-section-level 4
           yatex-fill-column-width 180
           YaTeX-template-file "~/.emacs.d/templates/template.tex"))
         (t
          (setq
           latex-message-kanji-code "euc-jp"
           tex-command "start -play "
           dvi2-command "start "
           YaTeX-user-completion-table "~/.emacs/yatexrc"
           YaTeX-hilit-sectioning-face '(navy/DarkOliveGreen1 LightGray/Black)
           YaTeX-auto-math-mode nil
           YaTeX-kanji-code 1
           YaTeX-use-LaTeX2e t
           YaTeX::ref-labeling-section-level 4
           YaTeX-template-file "~/sys/template/report.tex")))

        ;; yatex outline-minor-mode
        (setq-default outline-level 'outline-level)
        (add-hook
         'yatex-mode-hook
         (function
          (lambda ()
            (progn
              (modify-syntax-entry ?% "<" (syntax-table))
              (modify-syntax-entry 10 ">" (syntax-table))
              (make-variable-buffer-local 'outline-level)
              (setq outline-level 'latex-outline-level)
              (make-variable-buffer-local 'outline-regexp)
              (setq outline-regexp
                    (concat "[ \t]*\\\\\\(documentstyle\\|documentclass\\|chapter\\|"
                            "section\\|subsection\\|subsubsection\\|paragraph\\)"
                            "\\*?[ \t]*[[{]")
                    )))))

        (defun latex-outline-level ()
          (save-excursion
            (looking-at outline-regexp)
            (let ((title (buffer-substring (match-beginning 1) (match-end 1))))
              (cond ((equal (substring title 0 4) "docu") 20)
                    ((equal (substring title 0 4) "para") 15)
                    ((equal (substring title 0 4) "chap") 0)
                    ((equal (substring title 0 4) "appe") 0)
                    (t (length title))))))
        (eval-after-load "popwin"
          #'(progn
              (defadvice YaTeX-showup-buffer (around popwin-yatex:YaTeX-showup-buffer (buffer &optional func select) activate)
                (popwin:display-buffer-1 buffer
                                         :default-config-keywords `(:noselect ,(not select))
                                         :if-config-not-found (lambda (buffer) ad-do-IT))))
          )
        )
    ))
