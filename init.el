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
(load custom-file)
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
          (line-number-mode t)
          (menu-bar-mode -1)
          (modify-syntax-entry ?。 ".")
          (modify-syntax-entry ?、 ".")
          (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on))

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
    :bind (("C-o"         . dabbrev-expand)
	   ("C-x h"       . help-command)
           ("C-q C-q"     . quoted-insert)
           ("C-q C-j"     . join-line)
           ("C-q k"       . kill-region)
           ("C-x h"       . help-command)
           ("C-o"         . dabbrev-expand)
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

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2021-03-15"
  :url "https://github.com/magit/magit"
  :emacs>= 25.1
  :ensure t
  :bind (("C-q g g" . magit-status)))

(leaf solarized-theme
  :doc "The Solarized color theme"
  :req "emacs-24.1" "dash-2.16"
  :tag "solarized" "themes" "convenience" "emacs>=24.1"
  :added "2021-03-16"
  :url "http://github.com/bbatsov/solarized-emacs"
  :emacs>= 24.1
  :ensure t
  :config (load-theme 'solarized-dark t))

(provide 'init)
;;; init.el ends here
