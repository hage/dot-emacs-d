;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 非常に重要な設定
(global-set-key "\C-h" 'backward-delete-char-untabify)

;; 文字コード
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;;;
;;; ユーティリティ関数とマクロ
;;;
(defmacro exec-if-bound (sexplist)
  "関数が存在する時だけ実行する"
  `(if (fboundp (car ',sexplist))
       ,sexplist))

;;;
;;; MELPA & Cask & Pallet
;;; http://melpa.milkbox.net
;;; http://cask.github.io
;;; http://qiita.com/kametaro/items/2a0197c74cfd38fddb6b
;;;
(when (and (<= 24 emacs-major-version)
	   (require 'package))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (when (require 'cask)
    (cask-initialize)
    (require 'pallet)))

;;;
;;; いろいろ設定
;;;

;; メニューバーやスクロールバーなど余計なものを消す
(setq inhibit-startup-message t)
(exec-if-bound (scroll-bar-mode -1))
(exec-if-bound (tool-bar-mode -1))
(if (not window-system)
    (exec-if-bound (menu-bar-mode -1)))

;; mode-line
(line-number-mode t)			; 行番号を表示
(column-number-mode t)			; 列番号を表示

;; 余計な警告を出さないように
(put 'narrow-to-region 'disabled nil)

;; スクロール、カーソル移動
(setq scroll-conservatively 1		; 一行だけスクロール
      scroll-step 2)
(setq next-line-add-newlines nil)	; バッファの最後の行で next-line しても新しい行を作らない
(defun previous-line (arg)		; beginning-of-bufferと怒られないようにする
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
	  (line-move (- arg))
	((beginning-of-buffer end-of-buffer)))
    (line-move (- arg)))
  nil)

;; バックアップ・自動保存
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

;; show-paren-mode
(setq show-paren-delay 0.3)
(exec-if-bound (show-paren-mode t))

;; その他
(setq-default dabbrev-case-fold-search t) ; caseの区別なく探してきて、caseを保持したまま補完
(setq kill-whole-line t)		  ; C-k(kill-line) で行末の改行も含めて kill する
(setq comment-fill-column 1000)		  ; 行末コメントの位置が揃うように
(setq completion-ignore-case t)		  ; ファイルの補完をするときに case の区別をしない
(setq auto-coding-functions nil)	  ; セーブ時に勝手に文字コードを変更させない
(modify-syntax-entry ?。 ".")		  ; 句点を単語境界に
(modify-syntax-entry ?、 ".")		  ; 読点を単語境界に

;; 本当に終わってもいいの? と聞くようにする
(add-hook 'kill-emacs-query-functions
	  (function
	   (lambda ()
	     (y-or-n-p "Really quit Emacs? "))))

;;;
;;; isearch
;;;
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)


;;;
;;; Helm
;;;
(when (require 'helm-config nil t)
  (helm-mode 1)
  (global-set-key "\C-\M-o" 'helm-mini)
  (global-set-key "\M-x" 'helm-M-x)
  (global-set-key "\C-xb" 'helm-buffers-list)
  (global-set-key "\C-x\C-f" 'helm-find-files)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  ;; Correct ambiguous file name filtering
  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
	   (input-pattern (file-name-nondirectory pattern))
	   (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
	    (concat dirname
		    (if (string-match "^\\^" input-pattern)
			;; '^' is a pattern for basename
			;; and not required because the directory name is prepended
			(substring input-pattern 1)
		      (concat ".*" input-pattern))))))
  )


;;;
;;; キー・バインドの変更、新規割当
;;;
(global-unset-key "\C-q")
(global-unset-key "\C-w")
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
