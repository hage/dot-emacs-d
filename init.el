;;; init.el --- initialize
;; -*- Mode: Emacs-Lisp ; Coding: utf-8 ; lexical-binding: t -*-

;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;
;; init.elのロード時間を視覚化する
;; http://qiita.com/yuttie/items/0f38870817c11b2166bd
;;
;; 以下のコードを有効にしてEmacsを起動し、起動後 M-x initchart-visualize-init-sequence
;; (load "~/.emacs.d/initchart")
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)
;;;;;;;;;;;;;;;;

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; tmux 内にいる && Emacs がインタラクティブに起動 && "Emacs" が window-name にないとき
;; window-name を設定する。
;; また、Emacs 終了時に automatic-rename を有効にする
(let ((case-fold-search nil)            ; case-sensitive
      (tmux-title-of-emacs "Emacs"))
  (when (and (getenv "TMUX")
             (not noninteractive)
             (not (string-match-p
                   (concat  "^[0-9]+: " tmux-title-of-emacs)
                   (shell-command-to-string "tmux lsw"))))
    (shell-command (format "tmux rename-window '%s'" tmux-title-of-emacs))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (shell-command "tmux set-window-option automatic-rename on")))))


;; 非常に重要な設定
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq gc-cons-threshold (* gc-cons-threshold 10))
(setq garbage-collection-messages t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ここにいっぱい設定を書く

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf basic-config
  :init (progn
          (put 'narrow-to-region 'disabled nil)
          (line-number-mode t)
          (menu-bar-mode -1)
          (modify-syntax-entry ?。 ".")
          (modify-syntax-entry ?、 ".")
          (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
          (set-face-background 'vertical-border "#334")
          (set-display-table-slot standard-display-table
                                  'vertical-border
                                  (make-glyph-code ? )))

  :custom ((line-number-mode           . t)
           (inhibit-startup-message    . t)
           (scroll-conservatively      . 2)
           (scroll-step                . 1)
           (scroll-margin              . 3)
           (next-line-add-newlines     . nil)
           (auto-save-default          . nil)
           (make-backup-files          . nil)
           (create-lockfiles           . nil)
           (vc-make-backup-files       . nil)
           (dabbrev-case-fold-search   . t)
           (case-replace               . nil)
           (kill-whole-line            . t)
           (completion-ignore-case     . t)
           (auto-coding-functions      . nil)
           (indent-tabs-mode           . nil)
           (recenter-positions         . '(top middle bottom))
           (auto-revert-check-vc-info  . t)
           (force-load-messages        . t)
           (tags-add-tables            . nil)
           (compilation-ask-about-save . nil)
           (ring-bell-function         . (lambda () (princ "[RING] ")))))

(leaf basic-key-bindings
  :init
  (global-unset-key (kbd "C-q"))
  (global-unset-key (kbd "C-w"))
  (global-unset-key (kbd "C-x DEL"))
  (leaf basic-key-bindings-bind
    :bind (("M-o"         . other-window)
           ("C-o"         . dabbrev-expand)
	   ("C-x h"       . help-command)
           ("C-q C-q"     . quoted-insert)
           ("C-q C-j"     . join-line)
           ("C-q k"       . kill-region)
           ("C-x h"       . help-command)
           ("C-q C-a"     . mark-whole-buffer)
           ("M-&"         . replace-regexp)
           ("M-_"         . next-error)
           ("M-~"         . call-last-kbd-macro)
           ("C-q C-r"     . revert-buffer)
           ("M-C-_"       . indent-region)
           ("M-H"         . backward-kill-word)
           ("M-/"         . xref-find-definitions-other-window)
           ("M-?"         . xref-find-definitions)
           ("C-x j"       . goto-line)
           ("M-s f"       . find-function)
           ("M-L"         . recenter-top-bottom)
           ("C-q a"       . align)
           ("C-q C-SPC"   . mark-sexp)
           ("C-q DEL"     . just-one-space)
           ("M-Q"         . quit-window)
           ("C-x C-m C-m" . mark-defun)
           ("M-f"         . forward-word)
           ("M-b"         . backward-word)
           ("M-F"         . forward-to-word)
           ("M-B"         . backward-to-word))))

(leaf simple-bookmark
  :init
  (defun simple-bookmark-set ()
    (interactive)
    (progn
      (bookmark-set "simple-bookmark")
      (princ "bookmark-set simple-bookmark")))
  (defun simple-bookmark-jump ()
    (interactive)
    (bookmark-jump "simple-bookmark"))

  :bind (("C-q SPC" . simple-bookmark-set)
         ("C-q b"   . simple-bookmark-jump)))

(leaf yank-and-indent
  :init
  (defun yank-and-indent ()
    "Yank and indent it."
    (interactive)
    (yank)
    (save-excursion
      (exchange-point-and-mark)
      (indent-region (point) (mark))))
  :hook
  ((emacs-lisp-mode-hook) . (lambda ()
                              (local-set-key (kbd "C-y") #'yank-and-indent)
                              (local-set-key (kbd "C-M-y") #'yank))))

(leaf show-paren-mode
  :custom ((show-paren-style . 'mixed))
  :config (show-paren-mode t))

(leaf recentf
  :custom ((recentf-max-saved-items . 1024)
           (recentf-auto-cleanup    . 'never)))

(leaf pbcopy
  :doc "Emacs Interface to pbcopy"
  :tag "pbcopy" "osx" "mac"
  :added "2021-03-23"
  :url "https://github.com/jkp/pbcopy.el"
  :ensure t
  :init (turn-on-pbcopy))

(leaf duplicate-thing
  :doc "Duplicate current line & selection"
  :tag "selection" "line" "duplicate" "command" "convenience"
  :added "2021-03-22"
  :url "https://github.com/ongaeshi/duplicate-thing"
  :ensure t
  :bind (("M-c" . duplicate-thing)))

(leaf migemo
  :doc "Japanese incremental search through dynamic pattern expansion"
  :req "cl-lib-0.5"
  :added "2021-03-22"
  :url "https://github.com/emacs-jp/migemo"
  :ensure t
  :custom ((migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
           (migemo-coding-system . 'utf-8)
           (migemo-options . '("-q" "--emacs")))
  :commands migemo-init
  :init
  (migemo-init))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2021-03-15"
  :url "https://github.com/magit/magit"
  :emacs>= 25.1
  :ensure t
  :bind (("C-q g g" . magit-status))
  :config
  (defun git-commit-prefix-select ()
    (if (= 10 (following-char))
        (insert ":")))
  (add-hook 'git-commit-setup-hook 'git-commit-prefix-select))

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :added "2021-03-23"
  :url "https://github.com/emacsorphanage/git-gutter"
  :emacs>= 24.3
  :ensure t
  :init (global-git-gutter-mode)
  :bind (("M-g M-n" . git-gutter:next-hunk)
         ("M-g M-p" . git-gutter:previous-hunk)))

(leaf color-theme-sanityinc-solarized
  :doc "A version of Ethan Schoonover's Solarized themes"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "themes" "faces" "emacs>=24.1"
  :added "2021-03-19"
  :url "https://github.com/purcell/color-theme-sanityinc-solarized"
  :emacs>= 24.1
  :ensure t
  :config (load-theme 'sanityinc-solarized-dark t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :added "2021-03-21"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :global-minor-mode prescient-persist-mode)

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :added "2021-03-17"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :custom ((ivy-use-virtual-buffers        . t)
           (enable-recursive-minibuffers   . t)
           (minibuffer-depth-indicate-mode . 1)
           (ivy-truncate-lines             . nil)
           (ivy-height                     . `,(/ (window-height) 2))
           (ivy-wrap                       . nil)
           (ivy-mode                       . 1)
           (ivy-initial-inputs-alist       . nil)
           (ivy-use-selectable-prompt      . t)
           (ivy-re-builders-alist . '((swiper . ivy-migemo--regex-plus)
                                      (t . ivy--regex-ignore-order))))
  :bind (("C-w C-i" . ivy-resume))
  :config
  (leaf ivy-hydra
    :doc "Additional key bindings for Ivy"
    :req "emacs-24.5" "ivy-0.13.4" "hydra-0.14.0"
    :tag "convenience" "emacs>=24.5"
    :added "2021-03-17"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :after ivy hydra
    :custom ((ivy-read-action-function . #'ivy-hydra-read-action)))
  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "ivy-0.13.4" "swiper-0.13.4"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :added "2021-03-17"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :after ivy
    :init (counsel-mode 1)
    :bind (("M-x"     . counsel-M-x)
           ("M-y"     . counsel-yank-pop)
           ("C-M-z"   . counsel-fzf)
           ("C-M-r"   . counsel-recentf)
           ("C-x b"   . counsel-ibuffer)
           ("M-s a"   . counsel-ag)
           ("M-I"     . counsel-imenu)
           (counsel-find-file-map
            ("C-l" . counsel-up-directory))))
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.4"
    :tag "matching" "emacs>=24.5"
    :added "2021-03-17"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :after ivy
    :bind (("C-s" . swiper-thing-at-point)))
  (leaf ivy-rich
    :doc "More friendly display transformer for ivy"
    :req "emacs-25.1" "ivy-0.13.0"
    :tag "ivy" "convenience" "emacs>=25.1"
    :added "2021-03-21"
    :url "https://github.com/Yevgnen/ivy-rich"
    :emacs>= 25.1
    :ensure t
    :after ivy
    :global-minor-mode t)
  (leaf ivy-prescient
    :doc "prescient.el + Ivy"
    :req "emacs-25.1" "prescient-5.1" "ivy-0.11.0"
    :tag "extensions" "emacs>=25.1"
    :added "2021-03-21"
    :url "https://github.com/raxod502/prescient.el"
    :emacs>= 25.1
    :ensure t
    :after prescient ivy
    :global-minor-mode t)
  (leaf ivy-migemo
    :doc "Use migemo on ivy"
    :req "emacs-24.3" "ivy-0.13.0" "migemo-1.9.2"
    :tag "matching" "emacs>=24.3"
    :added "2021-03-22"
    :url "https://github.com/ROCKTAKEY/ivy-migemo"
    :emacs>= 24.3
    :ensure t
    :after ivy migemo
    :commands ivy-migemo--regex-plus
    :bind ((ivy-minibuffer-map
            ("M-M" . ivy-migemo-toggle-migemo)))))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :added "2021-03-18"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :bind (("C-i" . company-indent-or-complete-common)
         (company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :global-minor-mode global-company-mode
  :custom ((company-idle-delay            . .8)
           (company-minimum-prefix-length . 3)
           (company-transformers          . '(company-sort-by-occurrence)))
  :config
  (leaf company-prescient
    :doc "prescient.el + Company"
    :req "emacs-25.1" "prescient-5.1" "company-0.9.6"
    :tag "extensions" "emacs>=25.1"
    :added "2021-03-21"
    :url "https://github.com/raxod502/prescient.el"
    :emacs>= 25.1
    :ensure t
    :after prescient company
    :global-minor-mode t)
  (leaf company-tabnine
    :doc "A company-mode backend for TabNine"
    :req "emacs-25" "company-0.9.3" "cl-lib-0.5" "dash-2.16.0" "s-1.12.0" "unicode-escape-1.1"
    :tag "convenience" "emacs>=25"
    :added "2021-03-23"
    :url "https://github.com/TommyX12/company-tabnine/"
    :emacs>= 25
    :ensure t
    :after company unicode-escape
    :init (add-to-list 'company-backends #'company-tabnine)))

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2021-03-21"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t
  :config
  (yas-global-mode))

(leaf expand-region
  :doc "Increase selected region by semantic units."
  :tag "region" "marking"
  :added "2021-03-22"
  :ensure t
  :bind (("M-SPC" . er/expand-region)))

(leaf popwin
  :doc "Popup Window Manager"
  :req "emacs-24.3"
  :tag "convenience" "emacs>=24.3"
  :added "2021-03-23"
  :url "https://github.com/emacsorphanage/popwin"
  :emacs>= 24.3
  :ensure t
  :bind (("C-w C-p" . popwin:keymap))
  :init (popwin-mode 1))

(leaf smartparens
  :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs."
  :req "dash-2.13.0" "cl-lib-0.3"
  :tag "editing" "convenience" "abbrev"
  :added "2021-03-23"
  :url "https://github.com/Fuco1/smartparens"
  :ensure t
  :hook emacs-lisp-mode-hook
  :bind ((smartparens-mode-map
          ("M-R" . sp-raise-sexp)
          ("M-D" . sp-splice-sexp)
          ("M-H" . sp-backward-barf-sexp)
          ("M-J" . sp-backward-slurp-sexp)
          ("M-K" . sp-forward-slurp-sexp)
          ("M-L" . sp-forward-barf-sexp))))

(provide 'init)
;;; init.el ends here
