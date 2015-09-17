;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 非常に重要な設定
(global-set-key "\C-h" 'backward-delete-char-untabify)

;; 文字コード
(set-language-environment 'japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;;;
;;; ユーティリティ関数とマクロ
;;;
(defmacro exec-if-bound (sexplist)
  "関数が存在する時だけ実行する"
  `(if (fboundp (car ',sexplist))
       ,sexplist))

(defun add-to-load-path-if-found (path)
  (let ((epath (expand-file-name path)))
    (if (file-exists-p epath)
        (progn
          (setq load-path (cons epath load-path))
          t)
      nil)))

(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (let ((file-exist (locate-library file)))
    (and file-exist
         (autoload function file docstring interactive type))
    file-exist))

(defun add-load-path-recurcive-if-found (my-elisp-directory)
  (interactive)
  (dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
                 (list dir (format "%s%d" dir emacs-major-version))))
    (when (and (stringp dir) (file-directory-p dir))
      (let ((default-directory dir))
        (setq load-path (cons default-directory load-path))
        (normal-top-level-add-subdirs-to-load-path)))))

(defun load-safe (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))

(defun string-strip (str)
  "文字列頭と末尾のホワイトスペース及び改行文字を削除する"
  (replace-regexp-in-string "^[ \n]*\\(.*\\)[ \n]*" "\\1" str))

(add-load-path-recurcive-if-found "~/.emacs.d/free")


;; 機種判別
(setq osxp (equal system-type 'darwin))	; osx環境であるかどうか


;;;
;;; MELPA & Cask & Pallet
;;; http://melpa.milkbox.net
;;; http://cask.github.io
;;; http://qiita.com/kametaro/items/2a0197c74cfd38fddb6b
;;;
;;; to update packages: M-x pallet-update
;;;
(when (and (<= 24 emacs-major-version)
	   (require 'package))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (when (require 'cask nil t)
    (cask-initialize)
    (when (require 'pallet nil t)
      (pallet--on))
    ))

;;;
;;; いろいろ設定
;;;

;; mode-line
(line-number-mode t)			; 行番号を表示
(setq column-number-mode nil)           ; 列番号は表示しない

(setq-default
 mode-line-buffer-identification
 `(,(propertize " %b " 'face '(:foreground "orange" :background "gray20" :weight bold))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                " "
                (window-system mode-line-frame-identification mode-line-buffer-identification)
                " "
                mode-line-position
                (vc-mode vc-mode)
                mode-line-modes
                mode-line-misc-info
                ;;mode-line-end-spaces
                ))
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode . "")
    (paredit-mode . " Pe")
    (eldoc-mode . " Ed")
    (abbrev-mode . "")
    (helm-mode . "")
    (undo-tree-mode . " Ut")
    (flymake-mode . " Fm")
    (magit-auto-revert-mode . "")       ; MRev
    (smooth-scroll-mode . "")           ; SScr
    (volatile-highlights-mode . "")     ; VHl
    (emmet-mode . " Emt")
    (robe-mode . " R")
    (company-mode . " cp")
    (alchemist-mode . " Alc")
    ;; Major modes
    (lisp-interaction-mode . "iLisp")
    (python-mode . "Py")
    (ruby-mode   . "Ruby")
    (emacs-lisp-mode . "Elisp")
    (elixir-mode . "Elixir")
    (markdown-mode . "Md")))
(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; メニューバーやスクロールバーなど余計なものを消す
(setq inhibit-startup-message t)
(exec-if-bound (scroll-bar-mode -1))
(exec-if-bound (tool-bar-mode -1))
(if (not window-system)
    (exec-if-bound (menu-bar-mode -1)))

;; 余計な警告を出さないように
(put 'narrow-to-region 'disabled nil)

;; スクロール、カーソル移動
(setq scroll-conservatively 1		; 一行だけスクロール
      scroll-step 1)
(setq next-line-add-newlines nil)	; バッファの最後の行で next-line しても新しい行を作らない
(defun previous-line (arg)		; beginning-of-bufferと怒られないようにする
  (interactive "p")
  (if (called-interactively-p 'interactive)
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
(setq-default dabbrev-case-fold-search t) ; caseの区別なく探す
(setq-default case-replace nil)           ; 入力したcaseに関係なくオリジナルをコピーする
(setq kill-whole-line t)		  ; C-k(kill-line) で行末の改行も含めて kill する
(setq comment-fill-column 1000)		  ; 行末コメントの位置が揃うように
(setq completion-ignore-case t)		  ; ファイルの補完をするときに case の区別をしない
(setq auto-coding-functions nil)	  ; セーブ時に勝手に文字コードを変更させない
(modify-syntax-entry ?。 ".")		  ; 句点を単語境界に
(modify-syntax-entry ?、 ".")		  ; 読点を単語境界に
(setq-default indent-tabs-mode nil)	  ; インデントにTABを使わない
(setq recenter-positions '(bottom top middle)) ; move-to-window-line-top-bottomの順番

;; hl-line-modeを有効に
(when (require 'hl-line nil t)
  ;; http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
  ;; 軽いhl-line
  (defun global-hl-line-timer-function ()
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight)))
  (setq global-hl-line-timer
        (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
  ;; (cancel-timer global-hl-line-timer)
  (set-face-background 'hl-line "gray10")
  )

;; recentf
(when (featurep 'recentf)
  (setq recentf-max-saved-items 2048)
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  )

;; 本当に終わってもいいの? と聞くようにする
(add-hook 'kill-emacs-query-functions
	  (lambda ()
            (y-or-n-p "Really quit Emacs? ")))

;; ファイルのセーブ前にそのバッファの末尾スペースを取り除く
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;; vcを起動しないようにする
(custom-set-variables
 '(vc-handled-backends nil))
;; vcが当てた不要なhookを外す
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

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
(global-set-key "\C-j" 'newline-and-indent)
(global-set-key (kbd "C-q a") 'align)
(global-set-key (kbd "C-w C-SPC") 'mark-sexp)

(setq ctl-q-map (key-binding (kbd "C-q")))
(setq ctl-w-map (key-binding (kbd "C-w")))

;;;
;;; ちょっとした関数とそのキーバインド
;;;
;; 簡易ブックマーク
(defun simple-bookmark-set ()
  (interactive)
  (progn
    (bookmark-set "simple-bookmark")
    (princ "bookmark-set simple-bookmark")))
(defun simple-bookmark-jump ()
  (interactive)
  (bookmark-jump "simple-bookmark"))
(global-set-key "\C-q " 'simple-bookmark-set)
(global-set-key "\C-qb" 'simple-bookmark-jump)

;; インデントして次の行に移動する
(defun indent-and-next-line ()
  (interactive)
  (indent-according-to-mode)
  (forward-line 1))
(define-key global-map "\M-n" 'indent-and-next-line)

;; 大文字・小文字の変更
(defvar changecase-word-type 0)
(defun changecase-word (cnt)
  "カーソルのすぐ左にある単語を大文字→先頭だけ大文字→小文字にする。"
  (interactive "p")
  (if (not (eq last-command 'changecase-word))
      (setq changecase-word-type 0))
  (cond ((= changecase-word-type 0)
         (upcase-word (- cnt))
         (setq changecase-word-type 1))
        ((= changecase-word-type 1)
         (capitalize-word (- cnt))
         (setq changecase-word-type 2))
        (t
         (downcase-word (- cnt))
         (setq changecase-word-type 0))))
(global-set-key "\M-u" 'changecase-word) ; M-u に割り当てる

;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun move-end-of-line-and-newline-and-indent ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "M-j") 'move-end-of-line-and-newline-and-indent)

(defun narrow-to-sexp ()
  "Make text outside current sexp invisible."
  (interactive)
  (mark-sexp)
  (narrow-to-region (region-beginning) (region-end))
  (setq mark-active nil))
(global-set-key (kbd "C-x n s") 'narrow-to-sexp)

;; バッファ全体をインデント
(defun my-indent-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun my-electric-indent ()
  "Indent specified region.
When resion is active, indent region.
Otherwise indent whole buffer."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (my-indent-whole-buffer)))

(global-set-key (kbd "C-M-\\") 'my-electric-indent)

;;;
;;; smartprens
;;;
(when (require 'smartparens-config nil t)
  (set-face-background 'sp-pair-overlay-face "navy")
  (set-face-background 'sp-wrap-overlay-face "blue4")
  (smartparens-global-mode))


;;;
;;; isearch
;;;
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
(set-face-foreground 'isearch-lazy-highlight-face "#fff")
(set-face-background 'isearch-lazy-highlight-face "gray55")
(set-face-foreground 'isearch "#fff")
(set-face-background 'isearch "tomato")


;;;
;;; duplicate-thing
;;;
(when (autoload-if-found 'duplicate-thing "duplicate-thing" nil t)
  (global-set-key (kbd "M-c") 'duplicate-thing))


;;;
;;; modeline-git-branch
;;;
(when (and (add-to-load-path-if-found "~/.emacs.d/develop/modeline-git-branch")
           (require 'modeline-git-branch nil t))
  (setq modeline-git-branch-wait-time 0.2)
  (modeline-git-branch-mode 1))


;;;
;;; eww
;;;
(eval-after-load "eww"
  #'(progn
      ;; ewwのサーチエンジンをgoogleに
      (setq eww-search-prefix "https://www.google.co.jp/search?q=")

      ;; http://rubikitch.com/2014/11/19/eww-nocolor/
      ;; [2014-11-17 Mon]背景・文字色を無効化する
      (defvar eww-disable-colorize t)
      (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
        (unless eww-disable-colorize
          (funcall orig start end fg)))
      (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
      (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
      (defun eww-disable-color ()
        "ewwで文字色を反映させない"
        (interactive)
        (setq-local eww-disable-colorize t)
        (eww-reload))
      (defun eww-enable-color ()
        "ewwで文字色を反映させる"
        (interactive)
        (setq-local eww-disable-colorize nil)
        (eww-reload))
      ))

;;;
;;; Helm
;;;
(when (require 'helm-config nil t)
  (global-set-key "\C-\M-o" 'helm-mini)
  (global-set-key "\M-x" 'helm-M-x)
  (global-set-key "\C-xb" 'helm-buffers-list)
  (global-set-key "\C-x\C-f" 'helm-find-files)
  (global-set-key "\C-xha" 'helm-apropos)
  (global-set-key "\M-I" 'helm-imenu)
  (global-set-key (kbd "C-x C-d") 'helm-browse-project)
  (global-set-key (kbd "C-q C-y") 'helm-show-kill-ring)
                                        ; (global-set-key (kbd "C-o") 'helm-dabbrev)

  (setq helm-case-fold-search t)
  (setq helm-M-x-fuzzy-match nil)
  (setq helm-dabbrev-cycle-thresold 3)
  (setq helm-buffer-max-length 40)

  (eval-after-load "helm"
    #'(progn
        (helm-mode 1)
        (if (require 'helm-ls-git nil t)
            (progn
              (set-face-foreground 'helm-ff-file "aquamarine1")
              (set-face-foreground 'helm-buffer-file "lime green")
              (set-face-background 'helm-selection-line "gray20")
              (set-face-underline 'helm-selection-line nil)
              (set-face-foreground 'helm-ls-git-conflict-face "#f77")
              (set-face-background 'helm-ls-git-conflict-face "red3")
              (set-face-foreground 'helm-ls-git-untracked-face "plum1"))
          (setq helm-source-ls-git nil))

        (eval-after-load "auto-complete"
	  #'(when (autoload-if-found 'ac-complete-with-helm "ac-helm" nil t)
	      (setq my-ac-helm-trigger-key (kbd "M-l"))
	      (define-key ac-complete-mode-map my-ac-helm-trigger-key 'ac-complete-with-helm)
	      (global-set-key my-ac-helm-trigger-key 'ac-complete-with-helm)
	      (define-key helm-map my-ac-helm-trigger-key 'helm-next-line)))
	(define-key helm-map (kbd "C-h") 'delete-backward-char)
        (define-key helm-map (kbd "C-M-n") 'helm-next-source)
        (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
	(set-face-background 'helm-selection "gray20")
	(set-face-foreground 'helm-selection nil)
	(set-face-underline 'helm-selection nil)
	(set-face-background 'helm-source-header "gray10")
	(set-face-foreground 'helm-source-header "yellowgreen")
	(set-face-underline 'helm-source-header t)
	(set-face-foreground 'helm-match "hotpink1")

        (custom-set-variables
         '(helm-mini-default-sources `(helm-source-buffers-list
                                       helm-source-ls-git
                                       helm-source-recentf
                                       helm-source-findutils
                                       ,(if osxp 'helm-source-mac-spotlight)
                                       )))
	))
  (eval-after-load "helm-files"
    #'(progn
	(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
	(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)))

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

  (when (autoload-if-found 'helm-swoop-from-isearch "helm-swoop" nil t)
    (define-key isearch-mode-map (kbd "M-o") 'helm-swoop-from-isearch)
    )
  )


;;;
;;; emmet-mode
;;;
(when (autoload-if-found 'emmet-mode "emmet-mode" nil t)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (eval-after-load "emmet-mode"
    #'(progn
        (setq emmet-preview-default t)
        (setq emmet-indentation 2)
        (set-face-foreground 'emmet-preview-input "snow")
        (set-face-background 'emmet-preview-input "yellow4")
        (set-face-foreground 'emmet-preview-output "skyblue")
        (set-face-background 'emmet-preview-output "gray30")
        (define-key emmet-mode-keymap (kbd "C-j") nil)
        (define-key emmet-mode-keymap (kbd "C-M-l") 'emmet-expand-line)
        (define-key emmet-mode-keymap (kbd "M-F") 'emmet-next-edit-point)
        (define-key emmet-mode-keymap (kbd "M-B") 'emmet-prev-edit-point)
        (define-key emmet-mode-keymap (kbd "C-c w") 'emmet-wrap-with-markup)
        ))
  )


;;;
;;; popwin
;;;
(when (require 'popwin nil t)
  (popwin-mode 1)
  (setq popwin:popup-window-height .43)
  (push '("\\*Faces\\*" :regexp t :stick t) popwin:special-display-config)
  (push '("\\*eshell\\*" :regexp t :stick t) popwin:special-display-config)
  (push '("\\*eww.*\\*" :regexp t :stick t) popwin:special-display-config)
  )


;;;
;;; company-mode
;;;
;;; elixir-mode では company-mode しか受け付けないようなのでしかたなしに設定する
;;; cf. http://qiita.com/sune2/items/b73037f9e85962f5afb7
;;;
;;; 追記: ac-alchemist なるものができたので elixir-mode において auto-complete が
;;; 使えるようになったが、この設定は残しておく。

(eval-after-load "company"
  #'(progn
      (setq company-idle-delay .3
            company-minimum-prefix-length 3
            company-selection-wrap-around t
            )
      (define-key company-active-map (kbd "M-n") nil)
      (define-key company-active-map (kbd "M-p") nil)
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      (define-key company-active-map (kbd "C-h") nil)

      (defun company--insert-candidate2 (candidate)
        (when (> (length candidate) 0)
          (setq candidate (substring-no-properties candidate))
          (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
              (insert (company-strip-prefix candidate))
            (if (equal company-prefix candidate)
                (company-select-next)
              (delete-region (- (point) (length company-prefix)) (point))
              (insert candidate))
            )))

      (defun company-complete-common2 ()
        (interactive)
        (when (company-manual-begin)
          (if (and (not (cdr company-candidates))
                   (equal company-common (car company-candidates)))
              (company-complete-selection)
            (company--insert-candidate2 company-common))))

      (define-key company-active-map [tab] 'company-complete-common2)
      ))


;;;
;;; auto-complete-mode
;;;
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq ac-auto-start t
        ac-ignore-case t
	ac-delay 0.3
        ac-auto-start 3
	ac-use-menu-map t)
  (eval-after-load "yasnippet"
    #'(setq-default ac-sources
                    (append '(ac-source-yasnippet) ac-sources)))
  )


;;;
;;; yasnippet
;;;
(when (require 'yasnippet nil t)
  (custom-set-variables '(yas-snippet-dirs "~/.emacs.d/snippets"))
  (set-face-background 'yas-field-highlight-face "gray10")
  (set-face-underline 'yas-field-highlight-face t)
  (if (require 'dropdown-list nil t)
      (setq yas-prompt-functions '(yas/dropdown-prompt
				   yas/ido-prompt
				   yas/completing-prompt)))
  (yas-global-mode 1)
  (global-set-key (kbd "C-l") 'yas-expand-from-trigger-key)
  (if (and (fboundp 'helm-mini)
           (autoload-if-found 'helm-yas-complete "helm-c-yasnippet" nil t))
      (progn
	(autoload 'helm-yas-visit-snippet-file "helm-c-yasnippet")
	(global-set-key (kbd "C-q C-l C-l") 'helm-yas-complete)
	(global-set-key (kbd "C-q C-l C-v") 'helm-yas-visit-snippet-file)))
  ;; snippet-mode for *.yasnippet files
  (add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode)))


;;;
;;; expand-region
;;;
(when (autoload-if-found 'er/expand-region "expand-region" nil t)
  (global-set-key (kbd "M-SPC") 'er/expand-region))

;;;
;;; multiple-cursors
;;;
(when (and (autoload-if-found 'mc/mark-all-dwim "multiple-cursors" nil t)
	   (autoload-if-found 'mc/edit-lines "multiple-cursors" nil t))
  (global-set-key (kbd "C-q C-SPC") 'mc/mark-all-dwim)
  (global-set-key (kbd "C-x r t") 'mc/edit-lines)
  )

;;;
;;; migemo
;;;
(setq migemo-command
      (cond
       ((eq (shell-command "which cmigemo") 0) "cmigemo")
       ((eq (shell-command "which migemo") 0) "migemo")
       (t nil)))
(when (and migemo-command (require 'migemo nil t))
  (setq migemo-accept-process-output-timeout-msec 20
	migemo-isearch-enable-p t
	migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"
	migemo-coding-system 'utf-8
	migemo-options '("-q" "--emacs" "-i" "\g"))
  (migemo-init))


;;;
;;; magit
;;;
(when (autoload-if-found 'magit-status "magit" "magit: Show Git status" t)
  (setq magit-auto-revert-mode t)
  (autoload 'magit-diff-unstaged "magit" "magit: Show unstaged diff" t)
  (autoload 'magit-log "magit" "magit: Show Log" t)

  (global-set-key (kbd "C-q g g") 'magit-status)
  (global-set-key (kbd "C-q g d") 'magit-diff-unstaged)
  (global-set-key (kbd "C-q g l") 'magit-log)
  (global-set-key (kbd "C-q g c") 'magit-branch-and-checkout)
  (global-set-key (kbd "C-q g b") 'magit-checkout)
  (global-set-key (kbd "C-q g v") 'vc-revert)

  (eval-after-load "magit"
    #'(progn
        (define-key magit-mode-map (kbd "C-w") ctl-q-map)
        (set-face-background 'magit-section-highlight "gray20")

        (set-face-foreground 'magit-diff-added "#55aa55")
        (set-face-background 'magit-diff-added nil)
        (set-face-foreground 'magit-diff-added-highlight "#00ff00")
        (set-face-background 'magit-diff-added-highlight "gray20")

        (set-face-foreground 'magit-diff-removed "#aa5555")
        (set-face-background 'magit-diff-removed nil)
        (set-face-foreground 'magit-diff-removed-highlight "#ff0000")
        (set-face-background 'magit-diff-removed-highlight "gray20")

        (set-face-foreground 'magit-diff-hunk-heading "gray30")
        ;; (set-face-background 'magit-diff-hunk-heading "gray60")
        (set-face-foreground 'magit-diff-hunk-heading-highlight "#ffffff")
        ;; (set-face-background 'magit-diff-hunk-heading-highlight "gray60")

        (set-face-foreground 'magit-diff-context "gray50")
        (set-face-background 'magit-diff-context nil)
        (set-face-foreground 'magit-diff-context-highlight "white")
        (set-face-background 'magit-diff-context-highlight "gray20")
        ))
  )


;;;
;;; howm
;;;
(when (add-to-load-path-if-found "~/.emacs.d/free/howm")
  (setq howm-menu-lang 'ja)
  (setq howm-process-coding-system 'euc-jp-unix)

  (setq howm-prefix "\C-q,")
  (global-set-key "\C-q,," 'howm-menu)
  (global-set-key "\C-q,c" 'howm-create)
  (global-set-key "\C-q,s" 'howm-list-grep-fixed)

  (mapc
   (lambda (f)
     (autoload f "howm" "Hitori Otegaru Wiki Modoki" t))
   '(howm-menu
     howm-create
     howm-list-grep-fixed
     howm-list-all
     howm-list-recent
     howm-list-grep
     howm-keyword-to-kill-ring))

  (eval-after-load "howm"
    #'(progn
	(define-key howm-mode-map [tab] 'action-lock-goto-next-link)
	(define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))
  ;; 「最近のメモ」一覧時にタイトル表示
  (setq howm-list-recent-title t)
  ;; 全メモ一覧時にタイトル表示
  (setq howm-list-all-title t)
  ;; メニューを 2 時間キャッシュ
  (setq howm-menu-expiry-hours 2)
  ;; メニューを自動更新しない
  (setq howm-refesh-after-save nil)
  ;; 下線を引き直さない
  (setq howm-refresh-afgter-save nil)

  ;; howm の時は auto-fill なしで
  (add-hook 'howm-mode-on-hook (lambda () (auto-fill-mode -1)))

  ;; RET でファイルを開く際, 一覧バッファを消す
  ;; C-u RET なら残る
  (setq howm-view-summary-persistent nil)

  ;; メニューの予定表の表示範囲
  ;; 10 日前から
  (setq howm-menu-schedule-days-before 10)
  ;; 3 日後まで
  (setq howm-menu-schedule-days 3)

  ;; howm のファイル名
  (setq howm-file-name-format "%Y/%m/%Y%m%d-%H%M%S.txt")

  (setq howm-view-grep-parse-line
	"^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
  ;; 検索しないファイルの正規表現
  (setq
   howm-excluded-file-regexp
   "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$\\|git")

  ;; Macの場合、Dropboxにメモを置く
  (if (and (equal system-type 'darwin) (file-directory-p "~/Dropbox/howm"))
      (setq howm-directory "~/Dropbox/howm/"))

  ;; いちいち消すのも面倒なので
  ;; 内容が 0 ならファイルごと削除する
  (if (not (memq 'delete-file-if-no-contents after-save-hook))
      (setq after-save-hook
	    (cons 'delete-file-if-no-contents after-save-hook)))
  (defun delete-file-if-no-contents ()
    (when (and
	   (buffer-file-name (current-buffer))
	   (string-match "\\.howm" (buffer-file-name (current-buffer)))
	   (= (point-min) (point-max)))
      (delete-file
       (buffer-file-name (current-buffer)))))

  ;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
  ;; C-cC-c で保存してバッファをキルする
  (defun my-save-and-kill-buffer ()
    (interactive)
    (when (and
	   (buffer-file-name)
	   (string-match "\\.howm"
			 (buffer-file-name)))
      (save-buffer)
      (kill-buffer nil)))
  (eval-after-load "howm-mode"
    #'(define-key howm-mode-map
        "\C-c\C-c" 'my-save-and-kill-buffer))
  )


;;;
;;; ruby-mode
;;;

;; zshからemacsを起動した時にrspec-modeが上手く動かない場合は ~/.zshenv に
;;   if which rbenv > /dev/null; then eval "$(rbenv init --no-rehash - zsh)" ; fi
;; を加える
;; cf. http://qiita.com/nysalor/items/59060cc16f2d636c24b3

(when (autoload-if-found 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  (eval-after-load "ruby-mode"
    #'(progn
        (defun ruby-interpreter ()
          (let ((shims-ruby (concat (getenv "HOME") "/.rbenv/shims/ruby")))
            (if (file-exists-p shims-ruby)
                shims-ruby
              (string-strip (shell-command-to-string "which ruby")))))
        (defun ruby-mode-insert-braces ()
          (interactive)
          (if (memq 'font-lock-string-face (text-properties-at (point)))
              (progn
                (insert "#{}")
                (backward-char))
            (progn
              (insert "{|| }")
              (backward-char 3))))
        (defun eww-open-ruby-reference ()
          (interactive)
          (let ((ruby-version
                 (replace-regexp-in-string "ruby \\([0-9]+\\.[0-9]+\\.\\).*\n" "\\1"
                                           (shell-command-to-string
                                            (concat (ruby-interpreter) " --version")))))
            (eww (concat
                  "http://docs.ruby-lang.org/ja/"
                  ruby-version
                  (cond ((eq ruby-version "1.8") "7")
                        ((eq ruby-version "1.9") "3")
                        (t "0"))
                  "/doc/"))
            )
          )
        (define-key ruby-mode-map (kbd "C-q m") 'eww-open-ruby-reference)
        (define-key ruby-mode-map (kbd "M-\"") 'ruby-mode-insert-braces)
        (define-key ruby-mode-map (kbd "C-M-q") 'ruby-indent-exp)
	(setq ruby-indent-level 2)
	(setq ruby-indent-tabs-mode nil)
	(when (require 'rinari nil t)
	  (global-rinari-mode t))
	))

  ;; rbenv
  (when (fboundp 'global-rbenv-mode)
    (setq rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode)
    (set-face-foreground 'rbenv-active-ruby-face "green1")
    (set-face-background 'rbenv-active-ruby-face nil)
    (set-face-bold-p 'rbenv-active-ruby-face nil)
    (add-hook 'ruby-mode-hook
              (lambda ()
                (rbenv-use-corresponding)))
    )

  ;; align
  (eval-after-load "align"
    #'(progn
	(add-to-list 'align-rules-list
		     '(ruby-comma-delimiter
		       (regexp . ",\\(\\s-*\\)[^# \t\n]")
		       (repeat . t)
		       (modes  . '(ruby-mode))))
	(add-to-list 'align-rules-list
		     '(ruby-hash-literal
		       (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
		       (repeat . t)
		       (modes  . '(ruby-mode))))
	(add-to-list 'align-rules-list
		     '(ruby-assignment-literal
		       (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
		       (repeat . t)
		       (modes  . '(ruby-mode))))
	(add-to-list 'align-rules-list	;TODO add to rcodetools.el
		     '(ruby-xmpfilter-mark
		       (regexp . "\\(\\s-*\\)# => [^#\t\n]")
		       (repeat . nil)
		       (modes  . '(ruby-mode))))))
  )
;;   robeを使うには
;;     Gemfileに 'pry' と記述しておいて M-x robe-start
;;   Gemfileがないときは、gem install pry pry-docした後
;;     M-x inf-ruby
;;     M-x robe-start
(when (autoload-if-found 'robe-mode "robe"
			 "Robe is a code assistance tool that uses a Ruby REPL subprocess" t)
  (autoload 'ac-robe-setup "ac-robe" "robe auto-complete" nil nil)
  (eval-after-load 'ruby-mode
    #'(add-hook 'ruby-mode-hook 'robe-mode))
  (eval-after-load 'auto-complete
    #'(add-hook 'robe-mode-hook 'ac-robe-setup))
  )
(when (autoload-if-found 'run-ruby "inf-ruby" "Run an inferior Ruby process in a buffer." t)
  (eval-after-load 'auto-complete
    #'(add-to-list 'ac-modes 'inf-ruby-mode))
  (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
  (eval-after-load 'inf-ruby
    #'(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete)))
;; SCSSはRailsを使うときに現れるのでここで一緒に定義する
(add-to-list 'auto-mode-alist '("\\.css\\.scss$" . css-mode))

(when (fboundp 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )


;;;
;;; php-mode
;;;
(when (autoload-if-found 'php-mode "php-mode" "Major mode for PHP files" t)
  (add-hook 'php-mode-hook
            (lambda ()
              (setq-local c-basic-offset 2)))
  )


;;;
;;; Emacs-Lisp-mode
;;;

;; 自動的に elisp リファレンスを mini-buffer に表示
(when (autoload-if-found 'turn-on-eldoc-mode "eldoc" "eldoc" t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode))


;;;
;;; web-mode
;;;
(when (autoload-if-found 'web-mode "web-mode" "web-mode" t)
  ;; 適用する拡張子
  (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

  (eval-after-load "web-mode"
    #'(progn
	(defun web-mode-hook ()
	  "Hooks for Web mode."
	  ;; web-mode 標準のタグ用キーバインド C-c C-t X は tmux の prefix key である C-t と
	  ;; バッティングするため C-c t X に換える
	  (define-key web-mode-map (kbd "C-c t a") 'web-mode-tag-attributes-sort)
	  (define-key web-mode-map (kbd "C-c t b") 'web-mode-tag-beginning)
	  (define-key web-mode-map (kbd "C-c t e") 'web-mode-tag-end)
	  (define-key web-mode-map (kbd "C-c t m") 'web-mode-tag-match)
	  (define-key web-mode-map (kbd "C-c t n") 'web-mode-tag-next)
	  (define-key web-mode-map (kbd "C-c t p") 'web-mode-tag-previous)
	  (define-key web-mode-map (kbd "C-c t s") 'web-mode-tag-select)
	  (define-key web-mode-map (kbd "C-c t c") 'web-mode-element-close)

	  ;; インデント数
	  (setq web-mode-markup-indent-offset 2)
	  (setq web-mode-html-offset   2)
	  (setq web-mode-css-offset    2)
	  (setq web-mode-script-offset 2)
	  (setq web-mode-php-offset    2)
	  (setq web-mode-java-offset   2)
	  (setq web-mode-asp-offset    2))
	(add-hook 'web-mode-hook 'web-mode-hook)
	(set-face-foreground 'web-mode-html-tag-bracket-face "lemonchiffon4")
	))
  )


;;;
;;; haskell-mode
;;;
(eval-after-load "haskell-mode"
  (lambda ()
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'font-lock-mode)
    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
    (add-hook 'haskell-mode-hook 'inf-haskell-mode)
    (defadvice inferior-haskell-load-file (after change-focus-after-load activate)
      "Change focus to GHCi window after C-c C-l command"
      (pop-to-buffer "*haskell*"))
    )
  )


;;;
;;; markdown-mode
;;;
(when (autoload-if-found 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
  (eval-after-load "markdown-mode"
    (lambda ()
      ;; デフォルトでキーシーケンスにC-tが入っているコマンド群はtmuxに当たるので C-t -> t に変更
      (define-key markdown-mode-map "\C-cth" 'markdown-insert-header-dwim)
      (define-key markdown-mode-map "\C-ctH" 'markdown-insert-header-setext-dwim)
      (define-key markdown-mode-map "\C-ct1" 'markdown-insert-header-atx-1)
      (define-key markdown-mode-map "\C-ct2" 'markdown-insert-header-atx-2)
      (define-key markdown-mode-map "\C-ct3" 'markdown-insert-header-atx-3)
      (define-key markdown-mode-map "\C-ct4" 'markdown-insert-header-atx-4)
      (define-key markdown-mode-map "\C-ct5" 'markdown-insert-header-atx-5)
      (define-key markdown-mode-map "\C-ct6" 'markdown-insert-header-atx-6)
      (define-key markdown-mode-map "\C-ct!" 'markdown-insert-header-setext-1)
      (define-key markdown-mode-map "\C-ct@" 'markdown-insert-header-setext-2)
      (define-key markdown-mode-map "\C-ctt" 'markdown-insert-header-setext-1)
      (define-key markdown-mode-map "\C-cts" 'markdown-insert-header-setext-2)

      (progn
        (setq markdown-common-header-face-foreground "SlateBlue4")
        (set-face-foreground 'markdown-header-face-1 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-2 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-3 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-4 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-5 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-6 markdown-common-header-face-foreground)
        )
      )
    )
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  )


;;;
;;; elixir-mode
;;;
(eval-after-load "elixir-mode"
  #'(progn
      (add-hook 'elixir-mode-hook 'alchemist-mode)
      (eval-after-load "alchemist"
        #'(progn
            (add-hook 'alchemist-mode-hook 'ac-alchemist-setup)
            (setq alchemist-key-command-prefix (kbd "C-c a")) ; これがないとiexの起動に失敗する
            (set-face-foreground 'elixir-atom-face "Gold3")
            (define-key alchemist-mode-map (kbd "C-x C-e") 'alchemist-iex-send-last-sexp)

            (defun my-alchemist-iex-electric-send-thing (uarg)
              "Sends the code fragment to the inferior IEx process.
If universal argument (C-u) is given, jump to the buffer.
when region is active, sends the marked region.
Otherwise sends the current line."
              (interactive "P")
              (cond
               ;; regionがアクティブかつC-uが押されている
               ((and uarg (use-region-p))
                (alchemist-iex-send-region-and-go))
               ;; regionがアクティブ
               ((and (not uarg) (use-region-p))
                (alchemist-iex-send-region (point) (mark)))
               ;; regionなし、かつC-uが押されている
               ((and uarg (not (use-region-p)))
                (alchemist-iex-send-current-line-and-go))
               ;; なんにもなし
               ((and (not uarg) (not (use-region-p)))
                (alchemist-iex-send-current-line))))

            (define-key alchemist-mode-map (kbd "C-M-x") 'my-alchemist-iex-electric-send-thing)))))


;;;
;;; volatile-highlights
;;;
(when (require 'volatile-highlights nil t)
  (volatile-highlights-mode t)
  (set-face-background 'vhl/default-face "thistle4")
  (set-face-foreground 'vhl/default-face "olivedrab1")
  )


;;;
;;; smooth-scroll
;;;
(when (require 'smooth-scroll nil t)
  (setq smooth-scroll/vscroll-step-size 3)
  (smooth-scroll-mode t))


;;;
;;; emacs server
;;;
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))




;;;
;;; hiwin
;;;

;; (when (require 'hiwin nil t)
;;   (hiwin-activate)
;;   (set-face-background 'hiwin-face "gray15"))

;;;
;;; smartrep
;;;
(when (require 'smartrep nil t)
  (setq smartrep-mode-line-active-bg "DarkSlateBlue")
  (setq smartrep-mode-line-string-activated "*")
  (smartrep-define-key
      global-map "C-x" '(("o" . (lambda () (other-window 1)))
                         ("p" . (lambda () (other-window -1)))))
  (smartrep-define-key
      global-map "C-q" '(("C-p" . (lambda ()
                                    (scroll-down-1)
                                    (previous-line 1)))
                         ("C-n" . (lambda ()
                                    (scroll-up-1)
                                    (next-line 1)))))
  (smartrep-define-key
      global-map "C-q" '(("n" . (lambda () (scroll-other-window)))
                         ("p" . (lambda () (scroll-other-window-down)))
                         ("N" . (lambda () (scroll-other-window 1)))
                         ("P" . (lambda () (scroll-other-window-down 1)))
                         ("<" . (lambda () (beginning-of-buffer-other-window 0)))
                         (">" . (lambda () (end-of-buffer-other-window 0)))
                         ))
  )

;;;
;;; elscreen
;;;
(when (require 'elscreen nil t)
  (setq elscreen-prefix-key (kbd "C-w C-w"))
  (elscreen-start)
  (global-set-key (kbd "C-w C-w C-w") 'elscreen-toggle)
  (global-set-key (kbd "C-w C-w l") 'helm-elscreen)
  ;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil)

  (set-face-foreground 'elscreen-tab-other-screen-face "#999")
  (set-face-background 'elscreen-tab-other-screen-face "#444")

  (set-face-foreground 'elscreen-tab-current-screen-face "#fff")
  (set-face-background 'elscreen-tab-current-screen-face "#666")
  )

;;;
;;; foreign-regexp
;;;
(when (require 'foreign-regexp nil t)
  (custom-set-variables
   '(foreign-regexp/regexp-type 'ruby)
   '(reb-re-syntax 'foreign-regexp))
  )


;;;
;;; yatex
;;;
(load-safe "~/.emacs.d/my-yatex")

;;;
;;; smart-newline-mode
;;;
(when (autoload-if-found 'smart-newline-mode "smart-newline" t)
  (add-hook 'ruby-mode-hook
            (lambda () (smart-newline-mode t)))
  )


;;;
;;; smart-compile
;;;
(when (autoload-if-found 'smart-compile "smart-compile" t)
  (eval-after-load "ruby-mode"
    #'(progn
        (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
        (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m")))))


;;;
;;; easy-kill
;;;
(when (require 'easy-kill nil t)
  (global-set-key (kbd "M-w") 'easy-kill))


;;;
;;; faces
;;;
(set-face-foreground 'mode-line "chartreuse1")
(set-face-background 'mode-line "gray40")
(set-face-underline 'mode-line nil)
(set-face-foreground 'mode-line-inactive "gray40")
(set-face-background 'mode-line-inactive "gray10")
(set-face-foreground 'font-lock-comment-face "gray60")
(set-face-foreground 'highlight nil)
(set-face-background 'highlight "palegreen4")
(set-face-background 'region "LightSteelBlue4")
(set-face-foreground 'font-lock-string-face "tomato1")
(set-face-foreground font-lock-builtin-face "CornflowerBlue")
(set-face-foreground font-lock-keyword-face "DeepSkyBlue")
(set-face-bold font-lock-keyword-face t)
(set-face-foreground 'minibuffer-prompt "SlateBlue")
(set-face-foreground 'font-lock-constant-face "gold3")
(set-face-foreground 'default "linen")
(set-face-foreground 'match "black")
(set-face-background 'match "yellow1")
(set-face-foreground 'show-paren-match "white")
(set-face-background 'show-paren-match "limegreen")
(set-face-foreground 'font-lock-type-face "LimeGreen")
(set-face-background 'secondary-selection "MediumPurple4")
