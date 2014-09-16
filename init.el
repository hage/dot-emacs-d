;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 非常に重要な設定
(global-set-key "\C-h" 'backward-delete-char-untabify)

;; 文字コード
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;;;
;;; キー・バインドの変更、新規割当
;;;
;; C-q 及び C-w でキーバインドを拡張する
(global-unset-key "\C-q")
(global-unset-key "\C-w")
(global-unset-key "\C-wc")
(global-set-key "\C-q\C-q" 'quoted-insert)
(global-set-key "\C-q\C-j" 'join-line)
(global-set-key "\C-ql" 'recenter)
(global-set-key "\C-qk" 'kill-region)
(global-set-key "\C-xh" 'help-command)
(global-set-key "\C-o" 'dabbrev-expand)
(global-set-key "\C-q\C-a" 'mark-whole-buffer)
(global-set-key "\M-&" 'replace-regexp)
(global-set-key "\M-_" 'next-error)
(global-set-key "\C-q\C-f" 'find-function)
(global-set-key "\M-~" 'call-last-kbd-macro)
(global-set-key "\C-q\C-r" 'revert-buffer)
(global-set-key "\M-\C-_" 'indent-region)
(global-set-key "\M-H" 'backward-kill-word)
(global-set-key "\M-/" 'find-tag-other-window)
(global-set-key "\C-xj" 'goto-line)
