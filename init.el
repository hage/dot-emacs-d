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

(leaf basic-functions
  :init
  (defun unshift (newelt place)
    "Destructive append NEWLT into PLACE.
(unshift 'z '(a b c)) => (a b c z)"
    (setcdr (last place) `(,newelt . nil))
    place))

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

(leaf keyboard-quit
  :preface
  (defun keyboard-quit-advice-before ()
    (run-with-timer 0.2 nil
                    (lambda ()
                      (redraw-display))))
  :advice ((:before keyboard-quit keyboard-quit-advice-before)))

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
  ((emacs-lisp-mode-hook ruby-mode-hook web-mode-hook) .
   (lambda ()
     (local-set-key (kbd "C-y") #'yank-and-indent)
     (local-set-key (kbd "C-M-y") #'yank))))

(leaf eldoc
  :blackout t)

(leaf autorevert
  :blackout auto-revert-mode)

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

(leaf smart-mark
  :doc "Restore point after C-g when mark"
  :tag "restore" "mark"
  :added "2021-03-23"
  :ensure t
  :init (smart-mark-mode 1))

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
  :blackout t
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
  :blackout t
  :custom ((ivy-use-virtual-buffers        . t)
           (ivy-count-format               . "(%d/%d) ")
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
    :blackout t
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
            ("M-M" . ivy-migemo-toggle-migemo))))
  (leaf ivy-yasnippet
    :doc "Preview yasnippets with ivy"
    :req "emacs-24.1" "cl-lib-0.6" "ivy-0.10.0" "yasnippet-0.12.2" "dash-2.14.1"
    :tag "convenience" "emacs>=24.1"
    :added "2021-03-30"
    :url "https://github.com/mkcms/ivy-yasnippet"
    :emacs>= 24.1
    :ensure t
    :after ivy yasnippet
    :bind (("C-w i" . ivy-yasnippet))))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :added "2021-03-18"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
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
    :after company
    :init (add-to-list 'company-backends #'company-tabnine)))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :added "2021-03-30"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :custom (flycheck-disabled-checkers . '(emacs-lisp-checkdoc)))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2021-03-27"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :hook ((ruby-mode-hook js2-mode-hook typescript-mode-hook) . lsp)
  :config
  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :added "2021-03-27"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 26.1
    :ensure t
    :after lsp-mode
    :custom ((lsp-ui-doc-enable . nil))))

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2021-03-21"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t
  :blackout (yas-minor-mode yas-global-mode)
  :init
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
  :hook emacs-lisp-mode-hook web-mode-hook ruby-mode-hook
  :defer-config (require 'smartparens-config)
  :bind ((smartparens-mode-map
          ("M-C-k" . sp-kill-hybrid-sexp)
          ("M-R" . sp-raise-sexp)
          ("M-D" . sp-splice-sexp)
          ("M-H" . sp-backward-barf-sexp)
          ("M-J" . sp-backward-slurp-sexp)
          ("M-K" . sp-forward-slurp-sexp)
          ("M-L" . sp-forward-barf-sexp))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :added "2021-03-24"
  :url "https://github.com/justbur/emacs-which-key"
  :emacs>= 24.4
  :ensure t
  :blackout t
  :custom ((which-key-side-window-max-width . 0.35)
           (which-key-max-display-columns . 1)
           (which-key-allow-imprecise-window-fit . t)
           (which-key-max-description-length . 70)
           (which-key-mode . 1))
  :config (which-key-setup-side-window-right))

(leaf perspective
  :doc "switch between named \"perspectives\" of the editor"
  :req "emacs-24.4" "cl-lib-0.5"
  :tag "frames" "convenience" "workspace" "emacs>=24.4"
  :added "2021-03-24"
  :url "http://github.com/nex3/perspective-el"
  :emacs>= 24.4
  :ensure t
  :init (persp-mode)
  :custom `((persp-mode-prefix-key . ,(kbd "M-t"))
            (persp-sort            . 'created)
            (persp-state-default-file . ,(concat user-emacs-directory ".persp-save.el")))
  :config
  (defun my-persp-switch-to-main ()
    (interactive)
    (persp-switch "main"))
  :bind ((persp-mode-map
          ("M-t M-t" . persp-switch-last)
          ("M-t k"   . persp-kill)
          ("M-t c"   . persp-switch)
          ("C-x b"   . persp-ivy-switch-buffer)
          ("C-x C-b" . counsel-switch-buffer)
          ("M-t 0"   . my-persp-switch-to-main))))

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1" "pkg-info-0.4"
  :tag "convenience" "project" "emacs>=25.1"
  :added "2021-03-25"
  :url "https://github.com/bbatsov/projectile"
  :emacs>= 25.1
  :ensure t
  :custom ((projectile-mode-line-prefix . " P"))
  :init (projectile-mode)
  :config
  (leaf counsel-projectile
    :doc "Ivy integration for Projectile"
    :req "counsel-0.13.0" "projectile-2.0.0"
    :tag "convenience" "project"
    :added "2021-03-25"
    :url "https://github.com/ericdanan/counsel-projectile"
    :ensure t
    :after counsel projectile
    :init (counsel-projectile-mode)
    :bind (projectile-mode-map
           ("M-C-o" . counsel-projectile)
           ("M-p M-p" . counsel-projectile-find-file-dwim))))

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs."
  :req "cl-lib-0.5"
  :tag "cursors" "editing"
  :added "2021-03-25"
  :url "https://github.com/magnars/multiple-cursors.el"
  :ensure t
  :bind (("C-w C-SPC" . mc/mark-all-dwim)
         ("C-x r t"   . mc/edit-lines)
         (mc/keymap
          ("C-k" . kill-line)
          ("C-y" . yank)
          ("M-w" . kill-ring-save))))

(leaf editorconfig
  :doc "EditorConfig Emacs Plugin"
  :req "cl-lib-0.5" "nadvice-0.3" "emacs-24"
  :tag "emacs>=24"
  :added "2021-03-26"
  :url "https://github.com/editorconfig/editorconfig-emacs#readme"
  :emacs>= 24
  :ensure t
  :blackout t
  :after nadvice
  :config (editorconfig-mode 1))

(leaf eshell
  :doc "the Emacs command shell"
  :tag "builtin"
  :added "2021-03-29"
  :config
  (leaf eshell-toggle
    :doc "Show/hide eshell under active window."
    :req "emacs-25.1" "dash-2.11.0"
    :tag "processes" "emacs>=25.1"
    :added "2021-03-29"
    :url "https://github.com/4da/eshell-toggle"
    :emacs>= 25.1
    :ensure t
    :bind ("C-w C-w" . eshell-toggle)))

(leaf shell-service
  :require server
  :config
  (unless (server-running-p)
    (server-start))
  (defun return-current-working-directory-to-shell ()
    (file-name-directory
     (expand-file-name
      (buffer-file-name
       (with-current-buffer
           (nth 1
                (assoc 'buffer-list
                       (nth 1 (nth 1 (current-frame-configuration)))))))))))

(leaf smart-compile
  :doc "an interface to `compile'"
  :tag "unix" "tools"
  :added "2021-03-30"
  :ensure t
  :init
  (defun save-and-compile-buffer ()
    "バッファをセーブしてcompileを実行。コマンドはcompile-commandに設定されているもの"
    (interactive)
    (when (buffer-file-name)
      (save-buffer)
      (compile compile-command)))
  :hook ((ruby-mode-hook) .
         (lambda ()
           (local-set-key (kbd "C-c C-c") #'save-and-compile-buffer)
           (local-set-key (kbd "C-c c") #'smart-compile))))

(leaf dimmer
  :doc "Visually highlight the selected buffer"
  :req "emacs-25.1"
  :tag "editing" "faces" "emacs>=25.1"
  :added "2021-04-02"
  :url "https://github.com/gonewest818/dimmer.el"
  :emacs>= 25.1
  :ensure t
  :custom ((dimmer-fraction . 0.2)
           (dimmer-adjustment-mode . :both))
  :global-minor-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major modes

(leaf elisp-mode
  :blackout (emacs-lisp-mode . "elisp"))

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :added "2021-03-25"
  :url "https://web-mode.org"
  :emacs>= 23.1
  :ensure t
  :mode "\\.phtml$" "\\.tpl\\.php$" "\\.jsp$" "\\.as[cp]x$" "\\.erb$" "\\.html?$" "\\.html\\.[^.]+$"
  :custom ((web-mode-auto-close-style                 . 1)
           (web-mode-enable-auto-closing              . t)
           (web-mode-enable-auto-pairing              . t)
           (web-mode-enable-auto-quoting              . t)
           (indent-tabs-mode                          . nil)
           (tab-width                                 . 2)
           (web-mode-markup-indent-offset             . 2)
           (web-mode-html-offset                      . 2)
           (web-mode-css-offset                       . 2)
           (web-mode-script-offset                    . 2)
           (web-mode-code-indent-offset               . 2)
           (web-mode-php-offset                       . 2)
           (web-mode-java-offset                      . 2)
           (web-mode-asp-offset                       . 2)
           (web-mode-enable-current-element-highlight . t))
  :defer-config (require 'sgml-mode nil t)
  :hook (web-mode-hook . (lambda () (sgml-electric-tag-pair-mode 1))))

(leaf emmet-mode
  :doc "Unofficial Emmet's support for emacs"
  :tag "convenience"
  :added "2021-03-26"
  :url "https://github.com/smihica/emmet-mode"
  :ensure t
  :custom((emmet-preview-default        . t)
          (emmet-indentation            . 2)
          (emmet-self-closing-tag-style . ""))
  :hook ((web-mode-hook) . (lambda ()
                            (emmet-mode t)
                            (local-set-key (kbd "C-M-i") #'emmet-expand-line))))

(leaf ruby-mode
  :doc "Major mode for editing Ruby files"
  :tag "builtin"
  :added "2021-03-26"
  :custom ((ruby-insert-encoding-magic-comment . nil))
  :config
  (leaf ruby-interpolation
    :doc "Ruby string interpolation helpers"
    :added "2021-03-30"
    :url "http://github.com/leoc/ruby-interpolation.el"
    :ensure t
    :hook ruby-mode-hook
    :blackout t))


(leaf js2-mode
  :doc "Improved JavaScript editing mode"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "javascript" "languages" "emacs>=24.1"
  :added "2021-03-27"
  :url "https://github.com/mooz/js2-mode/"
  :emacs>= 24.1
  :ensure t)

(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :req "emacs-24.3"
  :tag "languages" "typescript" "emacs>=24.3"
  :added "2021-03-27"
  :url "http://github.com/ananthakumaran/typescript.el"
  :emacs>= 24.3
  :ensure t)

(provide 'init)
;;; init.el ends here
