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

;;; tmux 内にいる && Emacs がインタラクティブに起動 && "Emacs" が window-name にないとき
;;; window-name を設定する。
;;; また、Emacs 終了時に automatic-rename を有効にする
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

(provide 'init)
;;; init.el ends here
