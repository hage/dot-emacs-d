;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; init.elにエラーを仕込んでどうしようもなくなった時
;;   $ emacs -q -l ~/.emacs.d/emergency-init.el
;; で最低限の設定で起動できる

;; 非常に重要な設定
(global-set-key "\C-h" 'backward-delete-char-untabify)
(global-set-key "\C-xh" 'help-command)

;; 文字コード
(set-language-environment 'japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

(setq inhibit-startup-message t)
(if (not window-system)
    (menu-bar-mode -1))

(find-file "~/.emacs.d/init.el")
