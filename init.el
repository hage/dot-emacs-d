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

;; 機種判別
(setq osxp (equal system-type 'darwin))	; osx環境であるかどうか


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
  (when (require 'cask nil t)
    (cask-initialize)
    (when (require 'pallet nil t)
      (pallet--on))
    ))

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
(setq-default dabbrev-case-fold-search t) ; caseの区別なく探してきて、caseを保持したまま補完
(setq kill-whole-line t)		  ; C-k(kill-line) で行末の改行も含めて kill する
(setq comment-fill-column 1000)		  ; 行末コメントの位置が揃うように
(setq completion-ignore-case t)		  ; ファイルの補完をするときに case の区別をしない
(setq auto-coding-functions nil)	  ; セーブ時に勝手に文字コードを変更させない
(modify-syntax-entry ?。 ".")		  ; 句点を単語境界に
(modify-syntax-entry ?、 ".")		  ; 読点を単語境界に
(setq indent-tabs-mode nil)		  ; インデントにTABを使わない

;; hl-line-modeを有効に
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)
  (set-face-background 'hl-line "gray10"))

;; 本当に終わってもいいの? と聞くようにする
(add-hook 'kill-emacs-query-functions
	  (lambda ()
	     (y-or-n-p "Really quit Emacs? ")))


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


;;;
;;; isearch
;;;
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)


;;;
;;; Helm
;;;
(when (require 'helm-config nil t)
  (global-set-key "\C-\M-o" 'helm-mini)
  (global-set-key "\M-x" 'helm-M-x)
  (global-set-key "\C-xb" 'helm-buffers-list)
  (global-set-key "\C-x\C-f" 'helm-find-files)
  (global-set-key "\C-xha" 'helm-apropos)

  (if (not (require 'helm-ls-git nil t))
      (setq helm-source-ls-git nil))
  (custom-set-variables
   '(helm-mini-default-sources `(helm-source-buffers-list
                                 helm-source-recentf
				 helm-source-ls-git
				 helm-source-findutils
				 ,(if osxp helm-source-mac-spotlight helm-source-locate)
				 )))
  (eval-after-load "helm"
    #'(progn
	(eval-after-load "auto-complete"
	  #'(when (autoload-if-found 'ac-complete-with-helm "ac-helm" nil t)
	      (setq my-ac-helm-trigger-key (kbd "M-l"))
	      (define-key ac-complete-mode-map my-ac-helm-trigger-key 'ac-complete-with-helm)
	      (global-set-key my-ac-helm-trigger-key 'ac-complete-with-helm)
	      (define-key helm-map my-ac-helm-trigger-key 'helm-next-line)))
	(define-key helm-map (kbd "C-h") 'delete-backward-char)
	(set-face-background 'helm-selection "gray40")
	(set-face-foreground 'helm-selection nil)
	(set-face-underline 'helm-selection nil)
	(set-face-background 'helm-source-header "gray10")
	(set-face-foreground 'helm-source-header "yellowgreen")
	(set-face-underline 'helm-source-header t)
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
  )


;;;
;;; zencoding-mode
;;;
(when (load "zencoding-mode-autoloads" t)
  (defvar zencoding-mode-keymap nil
    "Keymap for zencode minor mode.")
  (setq zencoding-mode-keymap (make-sparse-keymap))
  (define-key zencoding-mode-keymap (kbd "C-M-l") 'zencoding-expand-line)

  (add-hook 'web-mode-hook
	    (lambda ()
	      (zencoding-mode t)))
  (eval-after-load "zencoding-mode"
    #'(setq zencoding-indentation 2))
  )


;;;
;;; popwin
;;;
(when (require 'popwin nil t)
  (popwin-mode 1)

  (push '("\\*magit-.*\\*" :regexp t) popwin:special-display-config)

  (when (featurep 'helm-config)
    (setq helm-full-frame nil)
    (push '("*compilatoin*" :noselect t) popwin:special-display-config)
    (push '("helm" :regexp t) popwin:special-display-config))

  ;; フレームのサイズに応じてpopwinの出現位置を決める
  (defun popwin-auto-set-popup-window-position ()
    (interactive)
    (let ((w (frame-width))
	  (h (frame-height)))
      (if (and (< 200 w)				      ; フレームの幅が200桁より大きくて
	       (< h w))					      ; 横長の時に
	  (progn (setq popwin:popup-window-position 'right)   ; 右へ出す
		 (setq popwin:popup-window-width (/ w 2)))
	(setq popwin:popup-window-position 'bottom)))) ; そうじゃないときは下へ出す

  ;; popwin表示時にフレームサイズに応じた表示位置にする
  (defadvice  popwin:display-buffer (before popwin-auto-window-position activate)
    (popwin-auto-set-popup-window-position))
  )


;;;
;;; auto-complete
;;;
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq ac-auto-start t
        ac-ignore-case t
	ac-delay 0.3
        ac-auto-start 3
	ac-use-menu-map t)
  )


;;;
;;; yasnippet
;;;
(when (require 'yasnippet nil t)
  (custom-set-variables '(yas-snippet-dirs "~/.emacs.d/snippets"))
  (if (require 'dropdown-list nil t)
      (setq yas-prompt-functions '(yas/dropdown-prompt
				   yas/ido-prompt
				   yas/completing-prompt)))
  (yas-global-mode 1)
  (global-set-key "\C-l" 'yas-expand-from-trigger-key)

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
(when (autoload-if-found 'mc/mark-all-like-this "multiple-cursors" nil t)
  (global-set-key (kbd "M-RET") 'mc/mark-all-like-this)
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
  (autoload 'magit-diff-unstaged "magit" "magit: Show unstaged diff" t)
  (autoload 'magit-log "magit" "magit: Show Log" t)

  (global-set-key "\C-qgg" 'magit-status)
  (global-set-key "\C-qgd" 'magit-diff-unstaged)
  (global-set-key "\C-qgl" 'magit-log)
  (eval-after-load "magit"
    #'(progn
	(set-face-foreground 'magit-diff-add "green")
	(set-face-background 'magit-diff-add "gray20")
	(set-face-foreground 'magit-diff-del "red")
	(set-face-background 'magit-diff-del "gray20")
	(set-face-background 'magit-item-highlight "gray20")
	(set-face-foreground 'magit-diff-hunk-header "royalblue4")
	(set-face-foreground 'magit-log-head-label-bisect-bad "yellow")
	(set-face-foreground 'magit-log-head-label-bisect-skip "gray30")
	(set-face-foreground 'magit-branch "skyblue4")))
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
(when (autoload-if-found 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  (eval-after-load "ruby-mode"
    #'(progn
	(setq ruby-indent-level 2)
	(setq ruby-indent-tabs-mode nil)
	(when (require 'rinari nil t)
	  (global-rinari-mode t))
	))
  )
(when (autoload-if-found 'run-ruby "inf-ruby" "Run an inferior Ruby process in a buffer." t)
  (eval-after-load 'auto-complete
    #'(add-to-list 'ac-modes 'inf-ruby-mode))
  (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
  (eval-after-load 'inf-ruby
    #'(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete)))


;;;
;;; php-mode
;;;
(autoload-if-found 'php-mode "php-mode" "Major mode for PHP files" t)


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

	  ;; インデント数
	  (setq web-mode-html-offset   2)
	  (setq web-mode-css-offset    2)
	  (setq web-mode-script-offset 2)
	  (setq web-mode-php-offset    2)
	  (setq web-mode-java-offset   2)
	  (setq web-mode-asp-offset    2))
	(add-hook 'web-mode-hook 'web-mode-hook)))
  )


;;;
;;; e2wm
;;;
(when (autoload-if-found 'e2wm:start-management "e2wm" nil t)
  (global-set-key "\C-www" 'e2wm:start-management)
  (setq e2wm:prefix-key (kbd "C-w w"))
  (if (<= 24 emacs-major-version)
      (eval-after-load 'e2wm
	#'(setq e2wm:c-document-buffer-p	;
	    (lambda (buf)
	      (string-match "\\*\\(Help\\|info\\|w3m\\|WoMan\\|eww\\)" (buffer-name buf))))))
  )

;;;
;;; faces
;;;
(set-face-foreground 'mode-line "olivedrab3")
(set-face-background 'mode-line "gray40")
(set-face-foreground 'mode-line-inactive "gray30")
(set-face-background 'mode-line-inactive "gray10")
(set-face-foreground 'font-lock-comment-face "gray50")
(set-face-foreground 'highlight nil)
(set-face-background 'highlight "palegreen4")
(set-face-background 'region "navy")
