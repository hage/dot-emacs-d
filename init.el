;;; init.el --- initialize
;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

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

;;; tmux 内にいる && Emacs がインタラクティブに起動 && "Emacs" が window-name にないとき
;;; window-name を設定する。
;;; また、Emacs 終了時に window-name を起動時のものに戻す。
(defvar my-saved-tmux-window-name nil)
(let ((case-fold-search nil)            ; case-sensitive
      (tmux-title-of-emacs "Emacs"))
    (when (and (getenv "TMUX")
               (not noninteractive)
               (not (string-match-p
                     (concat  "^[0-9]+: " tmux-title-of-emacs)
                     (shell-command-to-string "tmux lsw"))))
      (setq my-saved-tmux-window-name
            (let ((line (shell-command-to-string "tmux lsw|grep '(active)'")))
              (string-match "[0-9]+: \\(.*\\)\\*" line)
              (match-string 1 line)))
      (shell-command (format "tmux rename-window '%s'" tmux-title-of-emacs))
      (add-hook 'kill-emacs-hook
                (lambda ()
                  (when my-saved-tmux-window-name
                    (shell-command (format "tmux rename-window '%s'" my-saved-tmux-window-name)))))))


;; 非常に重要な設定
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; パッケージでインストールしたとき .elc がなんか変なエラーを出すようになるのでそれを消す
(let ((file ".cask/25.3/elpa/php-eldoc-20140202.1141/php-eldoc.elc"))
  (when (file-exists-p file)
    (delete-file file)))


;; 文字コード
(set-language-environment 'japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;;;
;;; ユーティリティ関数とマクロ
;;;
(require 'cl)
(defmacro exec-if-bound (sexplist)
  "関数が存在する時だけ SEXPLIST を実行し、t を返す.  そうでないときは nil を返す."
  `(if (fboundp (car ',sexplist))
       (progn ,sexplist
              t)
     nil))

(defmacro add-hook-if-bound (hook func)
  "(fboundp func) が t のとき hook に func をフックする"
  `(if (fboundp ,func)
       (add-hook ,hook ,func)))

(defun add-to-load-path-if-found (path)
  "PATH が存在するときだけ `load-path' に加える."
  (let ((epath (expand-file-name path)))
    (if (file-exists-p epath)
        (progn
          (setq load-path (cons epath load-path))
          t)
      nil)))

(defun autoload-if-found (func file &optional docstring interactive type)
  "FUNC が評価されたとき FILE を読み込むようにする.
但しそれは FILE が 'load-path' 上に存在するときのみである.
DOCSTRING INTERACTIVE TYPE は 'autoload' に準じる."
  (let ((file-exist (locate-library file)))
    (and file-exist
         (autoload func file docstring interactive type))
    file-exist))

(defun add-load-path-recurcive-if-found (my-elisp-directory)
  "MY-ELISP-DIRECTORY 以下を再帰的に 'load-path' に加える."
  (interactive)
  (dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
                 (list dir (format "%s%d" dir emacs-major-version))))
    (when (and (stringp dir) (file-directory-p dir))
      (let ((default-directory dir))
        (setq load-path (cons default-directory load-path))
        (normal-top-level-add-subdirs-to-load-path)))))

(defun load-safe (loadlib)
  "安全な load。 LOADLIB を読み込むが読み込みに失敗してもそこで止まらない."
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))

(defmacro global-set-key-if-bound (key-bind fun)
  "関数が存在したら KEY-BIND に interactive 関数 FUN を割り当てる."
  `(when (fboundp ,fun)
     (global-set-key ,key-bind ,fun))
  )

(defun string-strip (str)
  "STR から文字列頭と末尾のホワイトスペース及び改行文字を削除する."
  (replace-regexp-in-string "^[ \n]*\\(.*\\)[ \n]*" "\\1" str))

(add-load-path-recurcive-if-found "~/.emacs.d/free")

(defun string-currentline ()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun inside-string-p (&optional pos)
  "POS のある位置が文字列であるなら nil 以外を返す. POS が与えられなかったら (point) から得る."
  (save-excursion
    (nth 3 (syntax-ppss (or pos (point))))))

(defun* git-toplevel-dir (&optional (path (buffer-file-name)))
  "Returns git root dir of specified PATH or current buffer.
returns nil when;
* PATH was not exist
* buffer does not have file name"
  (if (and path (file-exists-p path))
      (let ((result (string-strip
                     (shell-command-to-string (format  "cd %s && git rev-parse --show-toplevel" default-directory)))))
        result)
    nil)
  )

;; 機種判別
(setq osxp (equal system-type 'darwin))	; osx環境であるかどうか

;; east-asian-ambiguous
(require 'east-asian-ambiguous nil t)

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
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq package-hidden-regexps '("\\(evil\\|-theme\\)"))
  (package-initialize)
  (when (require 'cask nil t)
    (cask-initialize)
    (when (require 'pallet nil t)
      (pallet--on))
    ))

;;;
;;; いろいろ設定
;;;

(setq garbage-collection-messages nil)    ; ガベージコレクションのメッセージ

;; mode-line
(line-number-mode t)			; 行番号を表示
(setq column-number-mode t)             ; 列番号を表示する

(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

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
                ))
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode . "")
    (paredit-mode . " ()e")
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
    (ruby-test-mode . " rtest")
    (editorconfig-mode . " EdC")
    (helm-migemo-mode . "")             ; Hmio
    (auto-revert-mode . "")             ; ARev
    (auto-highlight-symbol-mode . "")   ; HS
    (which-key-mode . "")               ; WK
    (smartparens-mode . "")             ; SP
    (auto-complete-mode . "")           ; AC

    ;; Major modes
    (lisp-interaction-mode . "iLisp")
    (python-mode . "Py")
    (ruby-mode   . "Ruby")
    (emacs-lisp-mode . "Elisp")
    (elixir-mode . "Elx")
    (js2-mode . "Js2")
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

;;; keycast-mode
(eval-after-load 'keycast
  #'(progn
      (setq keycast-remove-tail-elements t)
      (setq keycast-insert-after 'mode-line-misc-info)
      (setq keycast-separator-width 0)
      (setq keycast-window-predicate 'keycast-bottom-right-window-p)
      (set-face-foreground 'keycast-command "white")))
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (exec-if-bound (keycast-mode 1))))

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

;; バックアップ・自動保存
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

;; show-paren-mode
(when (functionp 'show-paren-mode)
  (progn
    (setq show-paren-delay 0.2)
    (setq show-paren-style 'expression)
    (set-face-foreground 'show-paren-match nil)
    (set-face-background 'show-paren-match "#112909")
    (set-face-bold 'show-paren-match t)
    (set-face-foreground 'show-paren-mismatch nil)
    (set-face-background 'show-paren-mismatch "#551109")
    (show-paren-mode t)))

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
(setq recenter-positions '(top middle bottom)) ; move-to-window-line-top-bottomの順番
(setq-default smerge-command-prefix (kbd "C-q C-m"))
(setq ring-bell-function (lambda () (princ "[RING] "))) ; 控えめな ring
(setq auto-revert-check-vc-info t)      ; 変化があったときに自動的に revert する
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(setq force-load-messages t)

;; hl-line-modeを有効に
(when (and
       (not (equal (getenv "TERM_PROGRAM") "iTerm.app")) ; iTermのときはそちらにある同等機能を使うからこちらは無効に
       (require 'hl-line nil t))
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

;; savehist
(defvar savehist-additional-variables '(kill-ring))           ; 追加で保存する履歴
(defvar savehist-ignored-variables '(magit-revision-history)) ; 保存しない履歴
(savehist-mode 1)                                             ; minibuffer の履歴を保存

;; recentf
(eval-after-load 'recentf
  #'(progn
      (setq recentf-max-saved-items 4096)
      (setq recentf-auto-cleanup 3600)
      (setq recentf-exclude '(".recentf"))
      (run-with-idle-timer 60 t (lambda () (shut-up (recentf-save-list))))
      (require 'recentf-ext nil t)
      ))


;; 本当に終わってもいいの? と聞くようにする
(add-hook 'kill-emacs-query-functions
	  (lambda ()
            (y-or-n-p "Really quit Emacs? ")))

;; ファイルのセーブ前にそのバッファの末尾スペースを取り除く
(setq-default enable-delete-trailing-whitespace-before-save t)
(add-hook 'before-save-hook
          (lambda ()
            (when enable-delete-trailing-whitespace-before-save
              (delete-trailing-whitespace))))

;; vcを起動しないようにする
(setq vc-handled-backends nil)
;; vcが当てた不要なhookを外す
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;; tramp -- zshだとハングアップすることが多いため
(eval-after-load 'tramp #'(setenv "SHELL" "/bin/bash"))


;;; キー・バインドの変更、新規割当
;;;
(global-unset-key "\C-q")
(global-unset-key "\C-w")
(global-set-key "\C-q\C-q" 'quoted-insert)
(global-set-key "\C-q\C-j" 'join-line)
(global-set-key (kbd "M-L") 'recenter-top-bottom)
(global-set-key "\C-qk" 'kill-region)
(global-set-key "\C-xh" 'help-command)
(global-set-key "\C-o" 'dabbrev-expand)
(global-set-key "\C-q\C-a" 'mark-whole-buffer)
(global-set-key "\M-&" 'replace-regexp)
(global-set-key "\M-_" 'next-error)
(global-set-key (kbd "M-s f") 'find-function)
(global-set-key "\M-~" 'call-last-kbd-macro)
(global-set-key "\C-q\C-r" 'revert-buffer)
(global-set-key "\M-\C-_" 'indent-region)
(global-set-key "\M-H" 'backward-kill-word)
(global-set-key "\M-/" 'xref-find-definitions-other-window)
(global-set-key (kbd "M-?") 'find-tag)
(global-set-key "\C-xj" 'goto-line)
(global-set-key "\C-j" 'newline-and-indent)
(global-set-key (kbd "C-q a") 'align)
(global-set-key (kbd "C-w C-SPC") 'mark-sexp)
(global-set-key (kbd "C-q DEL") 'just-one-space)
(global-unset-key (kbd "C-x C-h"))

(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-F") 'forward-word)
(global-set-key (kbd "M-B") 'backward-to-word)

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

;; バッファのファイル名をリネーム
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FMove to: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
(global-set-key (kbd "C-x w") 'rename-file-and-buffer)


;; 閉じカッコの上にカーソルがあった時はブロックを開いて、そうじゃないときは newline-and-indent
(defun my-open-block-or-newline-and-indent ()
  (interactive)
  (let ((ch (char-to-string (following-char))))
    (if (string-match "[])}>]" ch)
        (progn
          (newline-and-indent)
          (forward-line -1)
          (move-end-of-line 1)
          (newline-and-indent))
      (newline-and-indent))
    ))
(global-set-key (kbd "C-j") 'my-open-block-or-newline-and-indent)

(eval-after-load "nginx-mode"
  #'(progn
      (add-hook 'nginx-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-j") 'my-open-block-or-newline-and-indent)))))


;; 行末に移動して
;;   セミコロンだったら → そのまま改行
;;   コメントだったら → indent-new-comment-line
;;   どれでもないとき → セミコロンをつけて改行
(defun my-semicolon-or-new-comment-ine ()
  (interactive)
  (move-end-of-line 1)
  (if (memq 'font-lock-comment-face (text-properties-at (point)))
      (indent-new-comment-line)
    (progn
      (if (not (string-match ";[ 	]*$" (string-currentline)))
          (insert ";"))
      (newline-and-indent))))

;; Finder でカレントディレクトリを開く // M-g o にキーバインド
(defun my-open-current-directory-with-finder ()
  "Open current directory with Finder (macOS)."
  (interactive)
  (cond
   (osxp
    (shell-command (concat "open " default-directory))
    (message (concat "Open directory \"" default-directory "\" with Finder.")))
   (t
    (dired default-directory))))
(global-set-key (kbd "M-g o") 'my-open-current-directory-with-finder)

;; yank 後にインデント
(defun my-yank-and-indent-it ()
  "Yank and indentat it."
  (interactive)
  (yank)
  (save-excursion
    (exchange-point-and-mark)
    (indent-region (point) (mark))))
(global-set-key (kbd "C-M-y") 'yank)                ; yank を C-M-y に移して
(global-set-key (kbd "C-y") 'my-yank-and-indent-it) ; 普通の yank に割り当てられているキーをこれに

(with-eval-after-load 'make-mode
    (define-key makefile-mode-map (kbd "C-y") 'yank)
 )

;;;
;;; cde用 -- カレントバッファのディレクトリを返す
;;;
(defun return-current-working-directory-to-shell ()
  (expand-file-name
   (with-current-buffer
       (if (featurep 'elscreen)
           (let* ((frame-confs (elscreen-get-frame-confs (selected-frame)))
                  (num (nth 1 (assoc 'screen-history frame-confs)))
                  (cur-window-conf
                   (assoc 'window-configuration
                          (assoc num (assoc 'screen-property frame-confs))))
                  (marker (nth 2 cur-window-conf)))
             (marker-buffer marker))
         (nth 1
              (assoc 'buffer-list
                     (nth 1 (nth 1 (current-frame-configuration))))))
     default-directory)))


;;;
;;; transpose-frame
;;;
(global-set-key-if-bound (kbd "C-w '") 'transpose-frame)
(global-set-key-if-bound (kbd "C-w \"") 'flop-frame)


;;;
;;; emacs-ja.info
;;;
;;;   https://ayatakesi.github.io
;;;   http://emacs.rubikitch.com/emacs245-manual-ja/
;;;
;;; マニュアルの生成 (ダウンロード) は
;;;   cd ~/.emacs.d
;;;   make info-ja
;;;
(defvar emacs-info-ja-path "~/.emacs.d/info/emacs-ja.info")
(when (file-exists-p emacs-info-ja-path)
  (add-to-list 'Info-directory-list (file-name-directory emacs-info-ja-path))
  (defun Info-find-node--info-ja (orig-fn filename &rest args)
    (apply orig-fn
           (pcase filename
             ("emacs" "emacs-ja")
             (t filename))
           args))
  (advice-add 'Info-find-node :around 'Info-find-node--info-ja))


;;;
;;; auto-insert-mode
;;;
;;;     cf. https://sites.google.com/site/ytakenaka/ja/emacs/autoinsert
(setq auto-insert-directory "~/.emacs.d/template")
(auto-insert-mode)
(defvar template-replacements-alists
  '(("%file%"     . (lambda()(file-name-nondirectory (buffer-file-name))))
    ("%filebody%" . (lambda ()
                      (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%name%"     . user-full-name)
    ("%mail%"     . (lambda()(identity user-mail-address)))
    ("%cyear%"    . (lambda()(substring (current-time-string) -4)))
    ; ("%license%"  . (lambda()(read-from-minibuffer "License: ")))
    ; ("%bdesc%"    . (lambda()(read-from-minibuffer "Brief dscription: ")))
    ))

(defun my-file-body-name (file-name)
  (substring file-name 0 (position 46 file-name)))

(defmacro defreplace (name replace-string)
  `(defun ,name (str)
     (goto-char (point-min))
     (replace-string ,replace-string str)))

(defreplace my-template-exec "%exec%")
(defreplace my-template-package "%package%")

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
	    (progn
	      (goto-char (point-min))
	      (replace-string (car c) (funcall (cdr c)) nil)))
	template-replacements-alists)
  (goto-char (point-max))
  (message "done."))


;;;
;;; diff-mode
;;;
(eval-after-load 'diff-mode
  #'(progn
      (set-face-foreground 'diff-added "#ddf")
      (set-face-background 'diff-added "#001")
      (set-face-foreground 'diff-indicator-added "#fff")
      (set-face-background 'diff-indicator-added "#000")
      (set-face-bold 'diff-indicator-added t)

      (set-face-foreground 'diff-removed "#fdd")
      (set-face-background 'diff-removed "#100")
      (set-face-foreground 'diff-indicator-removed "#fff")
      (set-face-background 'diff-indicator-removed "#000")
      (set-face-bold 'diff-indicator-removed t)

      (set-face-foreground 'diff-context "gray60")))
(when (fboundp 'diff-mode)
  (add-to-list 'auto-mode-alist '("\\.diff$" . diff-mode)))

;;;
;;; smartparens
;;;
(when (require 'smartparens-config nil t)
  (set-face-background 'sp-wrap-overlay-face "blue4")
  (set-face-background 'sp-pair-overlay-face "black")
  (set-face-underline 'sp-pair-overlay-face t)
  (smartparens-global-mode))


;;;
;;; isearch
;;;
(set-face-foreground 'isearch "#fff")
(set-face-background 'isearch "tomato")


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
;;; duplicate-thing
;;;
(when (autoload-if-found 'duplicate-thing "duplicate-thing" nil t)
  (global-set-key (kbd "M-c") 'duplicate-thing))


;;;
;;; modeline-git-branch
;;;
(when (and (add-to-load-path-if-found "~/.emacs.d/develop/modeline-git-branch")
           (require 'modeline-git-branch nil t))
  (setq modeline-git-branch-wait-time 2)
  (modeline-git-branch-mode 1))


;;;
;;; eww
;;;
(eval-after-load "eww"
  #'(progn
      ;; ewwのサーチエンジンをgoogleに
      (setq eww-search-prefix "https://www.google.co.jp/search?q=")

      ;; 一般の web ブラウザで開き直す
      (define-key eww-mode-map (kbd "C-c C-v") 'eww-browse-with-external-browser)

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
  (global-set-key (kbd "M-O") 'helm-resume)
  (global-set-key "\M-x" 'helm-M-x)
  (global-set-key "\C-xb" 'helm-buffers-list)
  (global-set-key "\C-x\C-f" 'helm-find-files)
  (global-set-key "\C-xha" 'helm-apropos)
  (global-set-key "\M-I" 'helm-semantic-or-imenu)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-o") 'helm-dabbrev)
  (global-set-key (kbd "M-s a") 'helm-do-grep-ag)
  (defvar helm-dabbrev-cycle-threshold 2)
  (defvar helm-case-fold-search t)
  (defvar helm-M-x-fuzzy-match nil)
  (defvar helm-M-x-always-save-history t)
  (defvar helm-dabbrev-cycle-thresold 3)
  (defvar helm-buffer-max-length 40)
  (defvar helm-autoresize-min-height 25)

  ;; helm-mini をフレーム下部に横いっぱいに広げて表示する
  ;;   https://github.com/emacs-helm/helm/issues/2039
  (defvar helm-always-two-windows nil)
  (defvar helm-display-buffer-default-height 23)
  (defvar helm-default-display-buffer-functions '(display-buffer-in-side-window))

  (semantic-mode 1)

  ;; helm-projectile
  (global-set-key (kbd "C-x C-g")
                  (if (fboundp 'helm-projectile) 'helm-projectile 'helm-browse-project))
  (global-set-key-if-bound (kbd "C-w C-o") 'helm-projectile-find-file-dwim)

  ;; autoload helm after startup
  (run-with-idle-timer 2 nil (lambda ()
                               (message "loading helm...")
                               (require 'helm)
                               (require 'helm-buffers)
                               (message "loading helm... done.")))

  (eval-after-load "helm"
    #'(progn
        (helm-mode 1)
        (helm-migemo-mode 1)
        (global-set-key-if-bound (kbd "M-s g") 'helm-git-grep)
        (setq helm-window-prefer-horizontal-split t) ; helm-buffer-switch-other-window で縦分割する
        (if (require 'helm-ls-git nil t)
            (progn
              (set-face-foreground 'helm-ff-file "aquamarine1")
              (set-face-foreground 'helm-buffer-file "limegreen")
              (set-face-background 'helm-selection-line "gray20")
              (set-face-underline 'helm-selection-line nil)

              (set-face-foreground 'helm-header "#aaa")
              (set-face-background 'helm-header "#111")
              (set-face-underline 'helm-header nil)
              (set-face-background 'helm-source-header "#000")

              (set-face-foreground 'helm-ls-git-conflict-face "#f77")
              (set-face-background 'helm-ls-git-conflict-face "red3")
              (set-face-foreground 'helm-ls-git-untracked-face "plum1")

              (set-face-foreground 'helm-ff-directory "yellow")
              (set-face-background 'helm-ff-directory nil)
              (set-face-underline 'helm-ff-directory t)

              (set-face-foreground 'helm-ff-dotted-directory "yellow")
              (set-face-background 'helm-ff-dotted-directory nil)
              (set-face-underline 'helm-ff-dotted-directory t)

              (set-face-foreground 'helm-ff-dotted-symlink-directory "#f55")
              (set-face-background 'helm-ff-dotted-symlink-directory "#225")

              (set-face-foreground 'helm-visible-mark "#fff")
              (set-face-background 'helm-visible-mark "palegreen4")
              )
          (setq helm-source-ls-git nil))

        (eval-after-load "auto-complete"
	  #'(when (autoload-if-found 'ac-complete-with-helm "ac-helm" nil t)
	      (setq my-ac-helm-trigger-key (kbd "M-l"))
	      (define-key ac-complete-mode-map my-ac-helm-trigger-key 'ac-complete-with-helm)
	      (global-set-key my-ac-helm-trigger-key 'ac-complete-with-helm)
	      (define-key helm-map my-ac-helm-trigger-key 'helm-next-line)))

        (set-face-background 'helm-selection "gray40")
	(set-face-foreground 'helm-selection nil)
	(set-face-underline 'helm-selection nil)
	(set-face-background 'helm-source-header "gray10")
	(set-face-foreground 'helm-source-header "yellowgreen")
	(set-face-underline 'helm-source-header t)
	(set-face-foreground 'helm-match "hotpink1")
        (setq helm-locate-command
              (case system-type
                ('gnu/linux "locate -i -r %s")
                ('berkeley-unix "locate -i %s")
                ('windows-nt "es %s")
                ('darwin "mdfind -name %s %s")
                (t "locate %s"))
              )
        (setq helm-mini-default-sources `(helm-source-buffers-list
                                          helm-source-ls-git-status
                                          helm-source-ls-git
                                          helm-source-recentf
                                          helm-source-locate
                                          ))

        (define-key helm-map (kbd "C-M-n") 'helm-next-source)
        (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
        (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
        (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
        (define-key helm-map (kbd "C-M-i") 'helm-select-action)

        (let ((keyb (kbd "C-o")))
          (define-key helm-buffer-map     keyb 'helm-buffer-switch-other-window)
          (define-key helm-find-files-map keyb 'helm-ff-run-switch-other-window)
          (define-key helm-etags-map      keyb 'helm-etags-run-switch-other-window))


	))
  (eval-after-load "helm-files"
    #'(progn
	(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)))

  ; mode に関係なく dbbrev する
  (setq helm-dabbrev-related-buffer-fn
        (lambda (start-buffer) t))

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
    (setq my-helm-swoop-trigger-key (kbd "C-s"))
    (eval-after-load "helm-swoop"
      (lambda ()
        (set-face-foreground 'helm-swoop-target-word-face "#ffffff")
        (set-face-background 'helm-swoop-target-word-face "#050")
        (set-face-bold 'helm-swoop-target-word-face t)

        (set-face-foreground 'helm-swoop-target-line-face nil)
        (set-face-background 'helm-swoop-target-line-face "#030")
        (set-face-underline 'helm-swoop-target-line-face nil)

        (define-key helm-map my-helm-swoop-trigger-key 'previous-history-element))
      )
    (global-set-key my-helm-swoop-trigger-key 'helm-swoop)
    (setq helm-swoop-pre-input-function (lambda () "")) ; helm 起動時に検索入力欄を空にする
    (define-key isearch-mode-map (kbd "M-o") 'helm-swoop-from-isearch)
    (global-set-key (kbd "M-s s") 'isearch-forward))
  (global-set-key-if-bound (kbd "M-s M-a") #'helm-ag)
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
;;; https://github.com/m2ym/popwin-el
;;;
(when (require 'popwin nil t)
  (popwin-mode 1)
  (global-set-key (kbd "C-w C-p") popwin:keymap)
  (setq popwin:adjust-other-windows t)
  (setq popwin:popup-window-height .43)
  (push '("\\*Faces\\*" :regexp t :stick t) popwin:special-display-config)
  (push '("\\*eww.*\\*" :regexp t :stick t :position bottom :height .6) popwin:special-display-config)
  (push '("*Backtrace*") popwin:special-display-config)
  (push '("*compilation*" :height .4 :position bottom :stick t) popwin:special-display-config)
  (push '("*pry*" :height .5 :width .5 :stick t) popwin:special-display-config)
  (push '("*rake*") popwin:special-display-config)
  (push '("*Diff*") popwin:special-display-config)
  (push '("\\*alchemist .*\\*" :regexp t :stick t) popwin:special-display-config)
  (push '("*xref*") popwin:special-display-config)
  (push '("*robe-doc*" :stick t :dedicated t :width .5 :height .5) popwin:special-display-config)
  (push '("*Messages*" :position bottom :dedicated t :height .3) popwin:special-display-config)
  (push '("\\*Man .*" :regexp t :position right :stick t :width .5) popwin:special-display-config)
  (push '("*Colors*" :position right :stick t :width .5) popwin:special-display-config)
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
  (setq-default ac-sources (push 'ac-source-yasnippet ac-sources))
  (setq ac-ignore-case t
	ac-delay 0.1
        ac-auto-start 3
        ac-use-menu-map t)

  ;; ac-etags
  (setq ac-etags-requires 3)
  (eval-after-load "etags"
    '(progn
       (ac-etags-setup)))
  (defun my/prog-mode-common-hook ()
    (add-to-list 'ac-sources 'ac-source-etags))
  (add-hook 'c-mode-common-hook 'my/prog-mode-common-hook)
  (add-hook 'ruby-mode-hook 'my/prog-mode-common-hook)

  (push 'text-mode ac-modes)
  (push 'markdown-mode ac-modes)
  (push 'yaml-mode ac-modes)
  (push 'markdown-mode ac-modes)
  (push 'gfm-mode ac-modes)
  (push 'elixir-mode ac-modes)
  (push 'alchemist-iex-mode ac-modes)

  (eval-after-load 'auto-complete
    #'(progn
        (define-key ac-menu-map (kbd "RET") nil)))
  )


;;;
;;; yasnippet
;;;
;; auto-complete が require してくれているっぽいので、
;; auto-complete を使わなくなったら自前で require する必要があるかもしれない
(eval-after-load 'yasnippet
  (lambda ()
    (global-set-key (kbd "C-l") 'yas-expand-from-trigger-key)

    (set-face-foreground 'yas-field-highlight-face "#fff")
    (set-face-background 'yas-field-highlight-face "#509")

    (yas-global-mode)
    (if (and (fboundp 'helm-mini)
             (autoload-if-found 'helm-yas-complete "helm-c-yasnippet" nil t))
        (progn
          (autoload 'helm-yas-visit-snippet-file "helm-c-yasnippet")
          (global-set-key (kbd "C-q C-l C-l") 'helm-yas-complete)
          (global-set-key (kbd "C-q C-l C-v") 'helm-yas-visit-snippet-file)))))


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

  (eval-after-load "multiple-cursors"
    (lambda ()
      ;; cf. https://qiita.com/ShingoFukuyama/items/3ad7e24cb2d8f55b4cc5
      ;; insert specific serial number
      (defvar my/mc/insert-numbers-hist nil)
      (defvar my/mc/insert-numbers-inc 1)
      (defvar my/mc/insert-numbers-pad "%01d")

      (defun my/mc/insert-numbers (start inc pad)
        "Insert increasing numbers for each cursor specifically."
        (interactive
         (list (read-number "Start from: " 0)
               (read-number "Increment by: " 1)
               (read-string "Padding (%01d): " nil my/mc/insert-numbers-hist "%01d")))
        (setq mc--insert-numbers-number start)
        (setq my/mc/insert-numbers-inc inc)
        (setq my/mc/insert-numbers-pad pad)
        (mc/for-each-cursor-ordered
         (mc/execute-command-for-fake-cursor
          'my/mc--insert-number-and-increase
          cursor)))
      (defun my/mc--insert-number-and-increase ()
        (interactive)
        (insert (format my/mc/insert-numbers-pad mc--insert-numbers-number))
        (setq mc--insert-numbers-number (+ mc--insert-numbers-number my/mc/insert-numbers-inc)))))
  )

;;;
;;; magit
;;;
(when (autoload-if-found 'magit-status "magit" "magit: Show Git status" t)
  (setq magit-revert-buffers t)

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
        (setq magit-merge-arguments '("--no-ff"))

        (define-key magit-mode-map (kbd "C-w") ctl-q-map)
        (set-face-background 'magit-section-highlight "gray20")

        (set-face-foreground 'magit-diff-added "#22aa22")
        (set-face-background 'magit-diff-added "black")
        (set-face-foreground 'magit-diff-added-highlight "#44ff44")
        (set-face-background 'magit-diff-added-highlight "#112")

        (set-face-foreground 'magit-diff-removed "#aa2222")
        (set-face-background 'magit-diff-removed nil)
        (set-face-foreground 'magit-diff-removed-highlight "#ff4444")
        (set-face-background 'magit-diff-removed-highlight "#112")

        (set-face-foreground 'magit-diff-hunk-heading "gray60")
        (set-face-background 'magit-diff-hunk-heading "gray30")
        (set-face-foreground 'magit-diff-hunk-heading-highlight "#88ffff")
        (set-face-background 'magit-diff-hunk-heading-highlight "gray30")
        (set-face-bold-p 'magit-diff-hunk-heading-highlight t)

        (set-face-foreground 'magit-diff-context "gray50")
        (set-face-background 'magit-diff-context nil)
        (set-face-foreground 'magit-diff-context-highlight "white")
        (set-face-background 'magit-diff-context-highlight "#112")

        (set-face-foreground 'magit-branch-local "skyblue")
        (set-face-foreground 'magit-branch-remote "green")
        ))
  )


;;;
;;; org-mode
;;;
(when (featurep 'org)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (set-face-foreground 'org-document-title "skyblue3")
  (set-face-foreground 'org-document-info "skyblue3")
  )


;;;
;;; howm
;;;
; (defun autoload-if-found (func file &optional docstring interactive type)

(autoload-if-found 'howm-menu "howm" nil t)
(autoload-if-found 'howm-create "howm" nil t)
(autoload-if-found 'howm-list-grep-fixed "howm" nil t)

(global-set-key-if-bound "\C-q,," 'howm-menu)
(global-set-key-if-bound "\C-q,c" 'howm-create)
(global-set-key-if-bound "\C-q,s" 'howm-list-grep-fixed)


(eval-after-load "howm"
  #'(progn
      (setq howm-menu-lang 'ja)
      (setq howm-process-coding-system 'euc-jp-unix)

      (setq howm-prefix "\C-q,")

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
      (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
      (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)

      ;; 新しくメモを作る時は、先頭の「=タイトル」だけ挿入。
      (setq howm-template "= %title%cursor")
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
      (setq howm-file-name-format
            (concat "%Y/%m/%Y%m%d-%H%M%S." (if (functionp 'markdown-mode)
                                               "md"
                                             "howm")))

      (setq howm-view-grep-parse-line
            "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
      ;; 検索しないファイルの正規表現
      (setq
       howm-excluded-file-regexp
       "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$\\|git")

      ;; howmの置き場所
      (setq howm-directory "~/Documents/howm/")

      ;; カレントバッファがhowm管理下にあるかどうか判定する
      (defun my-howm-current-buffer-under-controlled-p ()
        (and (buffer-file-name (current-buffer))
             (string-match "/howm/" (buffer-file-name (current-buffer)))))

      ;; いちいち消すのも面倒なので
      ;; 内容が 0 ならファイルごと削除する
      (if (not (memq 'my-howm-delete-file-if-no-contents after-save-hook))
          (setq after-save-hook
                (cons 'my-howm-delete-file-if-no-contents after-save-hook)))
      ;; howmディレクトリ以下のファイルをセーブしたとき内容がなければ削除する
      (defun my-howm-delete-file-if-no-contents ()
        (when (and (my-howm-current-buffer-under-controlled-p)
                   (= (point-min) (point-max)))
          (delete-file
           (buffer-file-name (current-buffer)))))

      ;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
      ;; C-cC-c で保存してバッファをキルする
      (defun my-save-and-kill-buffer ()
        (interactive)
        (when (my-howm-current-buffer-under-controlled-p)
          (save-buffer)
          (kill-buffer nil)))
      (eval-after-load "howm-mode"
        #'(define-key howm-mode-map
            "\C-c\C-c" 'my-save-and-kill-buffer))
      ))


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
        (modify-syntax-entry ?@ "w" ruby-mode-syntax-table)
        (defun ruby-interpreter ()
          (let ((shims-ruby (concat (getenv "HOME") "/.rbenv/shims/ruby")))
            (if (file-exists-p shims-ruby)
                shims-ruby
              (string-strip (shell-command-to-string "which ruby")))))
        (defun ruby-mode-insert-braces ()
          (interactive)
          (if (inside-string-p)
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
        (define-key ruby-mode-map (kbd "C-c C-i") 'run-ruby)
        (define-key ruby-mode-map (kbd "C-c C-v") 'ruby-send-buffer)
	(setq ruby-indent-level 2)
	(setq ruby-indent-tabs-mode nil)
        (when (functionp 'ruby-test-mode)
          (add-hook 'ruby-mode-hook 'ruby-test-mode))
        (eval-after-load "ruby-test-mode"
          #'(progn
              (define-key ruby-mode-map (kbd "C-c C-_") 'ruby-test-run)
              (define-key ruby-mode-map (kbd "C-c C-/") 'ruby-test-run-at-point)))
	(when (require 'rinari nil t)
	  (global-rinari-mode t))

        (eval-after-load "ruby-tools"
          #'(progn
              (define-key ruby-tools-mode-map (kbd "C-q :") 'ruby-tools-to-symbol)
              (define-key ruby-tools-mode-map (kbd "C-q ;") 'ruby-tools-clear-string)
              (define-key ruby-tools-mode-map (kbd "C-q '") 'ruby-tools-to-single-quote-string)
              (define-key ruby-tools-mode-map (kbd "C-q \"") 'ruby-tools-to-double-quote-string)
              (define-key ruby-tools-mode-map (kbd "#") nil)))
	))

  ;; rbenv
  (when (fboundp 'global-rbenv-mode)
    (setq rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode)
    (set-face-foreground 'rbenv-active-ruby-face "green1")
    (set-face-background 'rbenv-active-ruby-face nil)
    (set-face-bold 'rbenv-active-ruby-face nil)
    (add-hook 'ruby-mode-hook
              (lambda ()
                (setq rbenv-show-active-ruby-in-modeline t)
                (rbenv-use-corresponding)))
    )

  ;; align
  (eval-after-load "align"
    #'(progn
        (defun align-rules-setup ()
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
		         (modes  . '(ruby-mode))))
          (add-to-list 'align-rules-list
                       '(ruby-hash-literal2
                         (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
                         (repeat . t)
                         (modes  . '(ruby-mode))))
          (add-to-list 'align-rules-list
		       '(elixir-comma-delimiter
		         (regexp . ",\\(\\s-*\\)[^# \t\n]")
		         (repeat . t)
		         (modes  . '(elixir-mode))))
          (add-to-list 'align-rules-list
		       '(elixir-colon-delimiter
		         (regexp . ":\\(\\s-*\\)[^# \t\n]")
		         (repeat . t)
		         (modes  . '(elixir-mode))))
          (add-to-list 'align-rules-list
                       '(php-hash
                         (regexp . "\\( *\\) =>")
                         (repeat . nil)
                         (modes . '(php-mode))))
          (add-to-list 'align-rules-list
                       '(php-assignment-literal
		         (regexp . "\\(\\s-*\\)=\\s-*[^ \t\n]")
		         (repeat . t)
		         (modes  . '(php-mode)))))
        (defun align-reset-rules ()
          (interactive)
          (setq align-rules-list nil)
          (align-rules-setup))
        (align-rules-setup)))
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
    #'(progn
        (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
        (autoload 'inf-ruby-minor-mode "inf-ruby" "" t)
        (add-hook 'ruby-mode-hook
                  (lambda ()
                    (robe-mode)
                    (robe-ac-setup)
                    (inf-ruby-minor-mode)))
        (eval-after-load 'robe
          #'(progn
              (define-key ruby-mode-map (kbd "C-c C-a") 'robe-ask)))
        (eval-after-load 'inf-ruby
          #'(progn
              (define-key ruby-mode-map (kbd "C-c C-i") 'inf-ruby-console-auto)
              (defun my-ruby-send-thing-dwim (uarg)
                "Sends the code fragment to the inferior Ruby process.
If universal argument (C-u) is given, jump to the inf-ruby buffer.
when region is active, sends the marked region.
Otherwise sends the whole buffer."
                (interactive "P")
                (cond
                 ;; regionがアクティブかつC-uが押されている
                 ((and uarg (use-region-p))
                  (ruby-send-region-and-go (point) (mark)))
                 ;; regionがアクティブ
                 ((and (not uarg) (use-region-p))
                  (ruby-send-region (point) (mark)))
                 ;; regionなし、かつC-uが押されている
                 ((and uarg (not (use-region-p)))
                  (ruby-send-region-and-go (point-min) (point-max)))
                 ;; なんにもなし
                 ((and (not uarg) (not (use-region-p)))
                  (ruby-send-region (point-min) (point-max)))))

              (define-key inf-ruby-minor-mode-map
                (kbd "C-c C-r") 'my-ruby-send-thing-dwim)))))

  (eval-after-load 'auto-complete
    #'(add-hook 'robe-mode-hook 'ac-robe-setup))
  )
(when (autoload-if-found 'run-ruby "inf-ruby" "Run an inferior Ruby process in a buffer." t)
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  ;; riなどのエスケープシーケンスを処理し、色付けする
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
  (eval-after-load 'auto-complete
    #'(add-to-list 'ac-modes 'inf-ruby-mode))
  (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
  (eval-after-load 'inf-ruby
    #'(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete)))

;; realgud:byebug
(eval-after-load 'realgud-byebug
  #'(progn
      (setq realgud:byebug-command-name "bundle exec byebug")))

;; SCSSはRailsを使うときに現れるのでここで一緒に定義する
(add-to-list 'auto-mode-alist '("\\.css\\.scss$" . css-mode))

(when (fboundp 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (when (fboundp 'ansible)
    (add-hook 'yaml-mode-hook 'ansible))
  )

(when (fboundp 'jinja2-mode)
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode)))

;;;
;;; php-mode
;;;
(when (autoload-if-found 'php-mode "php-mode" "Major mode for PHP files" t)
  (eval-after-load 'php-mode
    #'(define-key php-mode-map (kbd "M-.") 'php-show-arglist))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
  (setq php-search-url "https://secure.php.net/search.php?show=quickref&pattern=")
  (define-auto-insert ".*Test\\.php$" ["php-phpunit.php" my-template])
  (add-hook 'php-mode-hook
            (lambda ()
              (setq-local c-basic-offset 2)
              ; (php-eldoc-enable)
              (when (require 'ac-php nil t)
                (push 'ac-source-php ac-sources))
              (local-set-key (kbd "C-M-j") 'my-semicolon-or-new-comment-ine))))


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
  (add-to-list 'auto-mode-alist '("\\.html.eex?$" . web-mode))

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

          (define-key web-mode-map (kbd "M-RET") 'web-mode-element-content-select)
          (define-key web-mode-map (kbd "C-M-u") 'web-mode-element-parent)
          (define-key web-mode-map (kbd "C-M-d") 'web-mode-element-child)
          (define-key web-mode-map (kbd "M-]") 'web-mode-navigate)
          (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-buffer)

          ;; ターミナルではタグを自動的に閉じる機能が働かないようになっているので強制的に有効にする
          ;; cf. https://qiita.com/hayamiz/items/130727c09230fab0c097
          (setq web-mode-auto-close-style 1)
          (setq web-mode-enable-auto-closing t)
          (setq web-mode-enable-auto-pairing t)
          (setq web-mode-enable-auto-quoting t)

	  ;; インデント数
          (setq indent-tabs-mode nil)
          (setq tab-width 2)
          (setq web-mode-markup-indent-offset 2)
	  (setq web-mode-html-offset   2)
	  (setq web-mode-css-offset    2)
	  (setq web-mode-script-offset 2)
	  (setq web-mode-php-offset    2)
	  (setq web-mode-java-offset   2)
	  (setq web-mode-asp-offset    2))
	(add-hook 'web-mode-hook 'web-mode-hook)
	(set-face-foreground 'web-mode-html-tag-bracket-face "lemonchiffon4")
	(set-face-foreground 'web-mode-html-tag-face "OliveDrab3")

        (eval-after-load 'smartrep
          #'(progn
              (smartrep-define-key web-mode-map
                  "C-c e" '(("u" . (lambda () (web-mode-element-parent)))
                            ("d" . (lambda () (web-mode-element-child)))
                            ("n" . (lambda () (web-mode-element-sibling-next)))
                            ("p" . (lambda () (web-mode-element-sibling-previous)))
                            ("N" . (lambda () (web-mode-element-next)))
                            ("P" . (lambda () (web-mode-element-previous)))
                            ("t" . (lambda () (web-mode-element-transpose)))
                            ("k" . (lambda () (web-mode-element-kill)))
                            ("V" . (lambda () (web-mode-element-vanish)))
                            ("R" . (lambda () (web-mode-element-rename)))
                            ("S" . (lambda () (web-mode-element-select)))
                            ("s" . (lambda () (web-mode-element-content-select)))))))
        ;; convinience functions
        (defun my-web-mode-markup-paragraph-current-line ()
          (interactive)
          (back-to-indentation)
          (insert "<p>")
          (move-end-of-line 1)
          (insert "</p>")
          (forward-char))
        (define-key web-mode-map (kbd "M-P") 'my-web-mode-markup-paragraph-current-line)
	))
  )

;;; tagedit は不安定すぎるので Cask から消すことによって一度外す
;;; 以下のコードは残す
(when (autoload-if-found 'tagedit-mode "tagedit" "tagedit" t)
  (eval-after-load 'web-mode
    #'(progn
        (add-hook 'web-mode-hook
                  (lambda ()
                    (tagedit-mode)
                    (tagedit-add-experimental-features)
                    (define-key tagedit-mode-map (kbd "<") nil)
                    (define-key tagedit-mode-map (kbd ">") nil)
                    (define-key tagedit-mode-map (kbd ".") nil)
                    (define-key tagedit-mode-map (kbd "#") nil)))))

  (eval-after-load 'tagedit
    #'(progn
        (define-key tagedit-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
        (define-key tagedit-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
        (define-key tagedit-mode-map (kbd "M-r") 'tagedit-raise-tag)
        (define-key tagedit-mode-map (kbd "C-M-s") 'tagedit-splice-tag)
        (define-key tagedit-mode-map (kbd "C-k") 'tagedit-kill)
        (define-key tagedit-mode-map (kbd "C-w C-k") 'tagedit-kill-attribute)
        (eval-after-load 'smartrep
          #'(progn
              (smartrep-define-key
                  tagedit-mode-map "C-q" '(("9" . 'tagedit-forward-barf-tag)
                                           ("0" . 'tagedit-forward-slurp-tag))))))))



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

      (setq my-another-markdown-previewer (if osxp "/Applications/Markn.app/Contents/MacOS/Electron"
                                    nil))

      (setq my-markdown-previewer
            (if (and my-another-markdown-previewer (file-exists-p my-another-markdown-previewer))
                'my-markdown-previw
              'markdown-preview))

      (defun my-markdown-preview ()
        (interactive)
        "Preview a markdown file using Markn.app"
        ;;(async-shell-command (concat my-another-markdown-previewer " " buffer-file-name))
        (start-process "Markn.app" "*Markn*" my-another-markdown-previewer buffer-file-name)
        )

      (add-hook 'markdown-mode-hook
                '(lambda ()
                   (outline-minor-mode t)  ; markdown-mode で outline-minor-mode を有効にする
                   (define-key markdown-mode-map (kbd "C-c C-v") 'my-markdown-preview)
                   ))

      (progn
        (setq markdown-common-header-face-foreground "CornflowerBlue")
        (set-face-foreground 'markdown-header-face-1 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-2 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-3 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-4 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-5 markdown-common-header-face-foreground)
        (set-face-foreground 'markdown-header-face-6 markdown-common-header-face-foreground)

        (setq markdown-common-delimiter-face-foreground "moccasin")
        (set-face-foreground 'markdown-header-delimiter-face markdown-common-delimiter-face-foreground)
        (set-face-foreground 'markdown-list-face markdown-common-delimiter-face-foreground)
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
      (define-key elixir-mode-map (kbd "C-M-a") 'elixir-beginning-of-defun)
      (define-key elixir-mode-map (kbd "C-M-e") 'elixir-end-of-defun)
      ;; 改行して行頭に |> をつける
      (defun my-elixir-newline-and-insert-pipe ()
        (interactive)
        (move-end-of-line 1)
        (newline-and-indent)
        (insert "|> "))
      (define-key elixir-mode-map (kbd "M-J") 'my-elixir-newline-and-insert-pipe)

      (defun my-elixir-insert-arrow-and-newline ()
        (interactive)
        (move-end-of-line 1)
        (just-one-space)
        (insert "->")
        (newline-and-indent))
      (define-key elixir-mode-map (kbd "M-L") 'my-elixir-insert-arrow-and-newline)
      (define-key elixir-mode-map (kbd "M-u") 'string-inflection-ruby-style-cycle)

      (defun my-elixir-string-embed-expression ()
        (interactive)
        (if (inside-string-p)
            (progn
              (insert "#{}")
              (backward-char))
          (progn
            (insert "#"))))
      (define-key elixir-mode-map (kbd "#") 'my-elixir-string-embed-expression)


      (when (featurep 'smartparens)
        (sp-with-modes '(elixir-mode)
          (sp-local-pair "fn" "end"
                         :when '(("SPC" "RET"))
                         :actions '(insert navigate))
          (sp-local-pair "do" "end"
                         :when '(("SPC" "RET"))
                         :post-handlers '(sp-ruby-def-post-handler)
                         :actions '(insert navigate))))

      (add-hook-if-bound 'elixir-mode-hook 'alchemist-mode)
      (add-hook-if-bound 'elixir-mode-hook 'auto-highlight-symbol-mode)

      (eval-after-load "alchemist"
        #'(progn

            ;; C-c C-f でカーソル下の関数などのドキュメントを探し、
            ;; C-u をつけるとドキュメントの目次を表示する
            (defun my-alchemist-help-search (uarg)
              "Search Document by alchemist-help."
              (interactive "P")
              (if uarg
                  (alchemist-help)
                (alchemist-help-search-at-point)))
            (define-key alchemist-mode-map (kbd "C-c C-f") 'my-alchemist-help-search)

            (defun my-elixir-and-alchemist-iex-setup ()
              (ac-alchemist-setup))
            (add-hook 'alchemist-iex-mode-hook 'my-elixir-and-alchemist-iex-setup)
            (add-hook 'alchemist-mode-hook 'my-elixir-and-alchemist-iex-setup)

            (setq alchemist-key-command-prefix (kbd "C-c a")) ; これがないとiexの起動に失敗する

            (set-face-foreground 'elixir-atom-face "Gold3")

            (set-face-foreground 'elixir-attribute-face "royalblue1")
            (set-face-bold-p 'elixir-attribute-face t)

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

            (define-key alchemist-mode-map (kbd "C-M-x") 'my-alchemist-iex-electric-send-thing)

            (defun my-alchemist-iex-send-buffer (uarg)
              "Sends the code of current buffer to the inferior IEx process.
If universal argument (C-u) is given, jump to the buffer."
              (interactive "P")
              (alchemist-iex-send-region (point-min) (point-max))
              (if uarg (pop-to-buffer (process-buffer (alchemist-iex-process))))
              )
            (define-key alchemist-mode-map (kbd "C-c C-c") 'my-alchemist-iex-send-buffer)))))


;;;
;;; js2-mode
;;;
(when (functionp 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (eval-after-load "js2-mode"
    #'(progn
        (eval-after-load 'align
          #'(progn
              (add-to-list 'align-rules-list
                           '(javascript-object-notation
                             (regexp   . ":\\(\\s-*\\)") ; 末尾に \\(\\s-*\\)
                             (tab-stop . t)              ; タブ位置でそろえる
                             (modes    . '(js2-mode))))))
        (setq js2-strict-missing-semi-warning nil)
        (eval-after-load "auto-highlight-symbol"
          #'(progn
              (push 'js2-mode ahs-modes)))
        (add-hook 'js2-mode-hook
                  (lambda ()
                    (setq js2-basic-offset 2)
                    (setq tab-width 2)
                    (setq js-indent-level 2)
                    (define-key js2-mode-map (kbd "M-j") 'move-end-of-line-and-newline-and-indent)
                    (define-key js2-mode-map (kbd "M-J") 'js2-line-break)
                    (define-key js2-mode-map (kbd "C-M-j") 'my-semicolon-or-new-comment-ine)
                    ))
        ))
  )

(when (functionp 'json-mode)
  (setq json-reformat:indent-width 2)
  )
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
  (when (functionp 'mc/mark-next-like-this)
    (smartrep-define-key
        global-map "C-w" '(("w" . 'mc/mark-next-like-this)
                           ("n" . 'mc/mark-next-like-this)
                           ("p" . 'mc/mark-previous-like-this)
                           ))
    )
  (when (functionp 'goto-last-change)
    (smartrep-define-key
        global-map "C-q" '(("C-b" . 'goto-last-change)
                           ("C-f" . 'goto-last-change-reverse)
                           ))
    )
  )


;;;
;;; elscreen
;;;
(when (require 'elscreen nil t)
  (global-unset-key (kbd "M-t"))
  (setq elscreen-prefix-key (kbd "M-t"))
  (elscreen-start)
  (global-set-key (kbd "M-t M-t") 'elscreen-toggle)
  (global-set-key (kbd "M-t l") 'helm-elscreen)
  ;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil)

  (set-face-foreground 'elscreen-tab-other-screen-face "gray60")
  (set-face-background 'elscreen-tab-other-screen-face "gray20")

  (set-face-foreground 'elscreen-tab-current-screen-face "#af0")
  (set-face-background 'elscreen-tab-current-screen-face "#777")
  (set-face-bold 'elscreen-tab-current-screen-face t)

  (when (require 'elscreen-separate-buffer-list nil t)
    (elscreen-separate-buffer-list-mode))
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

  ;; multiple-cursor-mode のときは smart-newline が邪魔をしてうまく動かないので切る
  (eval-after-load "multiple-cursors"
    #'(progn
        (defun multiple-cursors-mode-hook-for-smart-newline ()
          (smart-newline-mode 0))
        (add-hook 'multiple-cursors-mode-hook 'multiple-cursors-mode-hook-for-smart-newline)))
  )


;;;
;;; smart-compile
;;;
(when (autoload-if-found 'smart-compile "smart-compile" t)
  (eval-after-load "ruby-mode"
    #'(progn
        (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
        (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))))
  (with-eval-after-load 'php-mode
    (add-hook 'php-mode-hook
              (lambda ()
                (setq-local smart-compile-option-string "test")))
    (define-key php-mode-map (kbd "C-c c") 'smart-compile)
    (define-key php-mode-map (kbd "C-c C-c") (kbd "C-c c C-m")))
  )


(when (autoload-if-found 'ansi-color-apply-on-region "ansi-color")
  (add-hook 'compilation-filter-hook
            '(lambda ()
               (ansi-color-apply-on-region (point-min) (point-max)))))

;;;
;;; easy-kill
;;;
(when (require 'easy-kill nil t)
  (global-set-key (kbd "M-w") 'easy-kill))


;;;
;;; which-key
;;;
(when (require 'which-key nil t)
  (which-key-setup-side-window-right)
  (setq which-key-side-window-max-width 0.5)
  (setq which-key-max-display-columns 1)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-max-description-length 70)
  (which-key-mode 1))


;;;
;;; persistent-scratch-buffer
;;;
(when (fboundp 'persistent-scratch-setup-default)
  (persistent-scratch-setup-default))


;;;
;;; auto-highlight-symbol
;;;
(when (require 'auto-highlight-symbol nil t)
  (global-auto-highlight-symbol-mode t)
  (eval-after-load 'smartrep
    #'(progn
        (smartrep-define-key
            global-map "C-w" '(("f" . 'ahs-forward)
                               ("b" . 'ahs-backward)
                               ("B" . 'ahs-back-to-start)))))
  (setq ahs-exclude
        '(
          (ruby-mode . "\\_<\\(end\\|def\\|class\\)\\_>")
          ))
  )


;;;
;;; pbcopy
;;;
(when (and osxp (require 'pbcopy nil t))
  (turn-on-pbcopy))


;;;
;;; paredit
;;;
(eval-after-load 'paredit
  #'(progn
      (define-key paredit-mode-map (kbd "M-s") nil)
      (define-key paredit-mode-map (kbd "C-M-s") 'paredit-splice-sexp)
      (eval-after-load 'smartrep
        #'(progn
            (smartrep-define-key
                global-map "C-w" '(("l" . (lambda () (paredit-forward-slurp-sexp)))
                                   ("h" . (lambda () (paredit-forward-barf-sexp)))
                                   ("L" . (lambda () (paredit-backward-barf-sexp)))
                                   ("H" . (lambda () (paredit-backward-slurp-sexp)))
                                   ("k" . (lambda () (paredit-splice-sexp-killing-backward)))
                                   ("j" . (lambda () (paredit-splice-sexp-killing-forward)))))))))





(when (fboundp 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))


;;;
;;; csv-mode
;;;
(eval-after-load 'csv-mode
  #'(progn
      (define-key csv-mode-map (kbd "C-M-f") 'csv-forward-field)
      (define-key csv-mode-map (kbd "C-M-b") 'csv-backward-field)
      ))


;;;
;;; plogic
;;;
(when (fboundp 'css-mode)
  (add-to-list 'auto-mode-alist '("\\.plogic\\'" . css-mode)
               (setq css-indent-offset 2)))


;;;
;;; eshell
;;;
(global-set-key-if-bound (kbd "C-w C-w") 'eshell)
(when (featurep 'popwin)
  (push '("*eshell*" :height 0.5 :stick t) popwin:special-display-config)

  (defun eshell-pop (universal-argument)
    "open eshell window using popwin-elf"
    (interactive "P")
    (let* ((eshell-buffer-name "*eshell*")
	   (eshell-buffer (get-buffer eshell-buffer-name))
	   (file-name (buffer-file-name (current-buffer)))
	   (current-directory (with-current-buffer (current-buffer) default-directory)))
      (if eshell-buffer
	  (popwin:display-buffer eshell-buffer)
	(eshell))
      (when (and universal-argument file-name)
	(eshell-kill-input)
	(insert (concat "cd " current-directory))
	(eshell-send-input)
	(end-of-buffer))))
  (global-set-key-if-bound (kbd "C-w C-w") 'eshell-pop))


;;;
;;; ace-search
;;;
(when (global-set-key-if-bound (kbd "M-C-r") 'ace-jump-mode)
  (eval-after-load 'ace-jump-mode
    (lambda ()
      (set-face-foreground 'ace-jump-face-foreground "#fff")
      (set-face-background 'ace-jump-face-foreground "#000"))))

(when (global-set-key-if-bound (kbd "M-o") 'ace-window)
  (eval-after-load 'ace-window
    (lambda ()
      (set-face-foreground 'aw-leading-char-face "#fff")
      (set-face-background 'aw-leading-char-face "#950"))))


;;;
;;; flycheck
;;;
(when (fboundp #'global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (when (fboundp #'flycheck-pos-tip-mode) ; emacs -nw ではダメっぽいけど将来のために残しておく
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode)))

  ;; elisp でコメントのチェックをしない
  ;; cf. https://stackoverflow.com/questions/15552349/hw-to-disable-flycheck-warning-while-editing-emacs-lisp-scripts
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (defun register-flycheck-disable-mode (mode)
    "flycheck-mode を無効にするモードを登録する"
    (when (fboundp mode)
      (add-hook mode
                (lambda ()
                  (flycheck-mode 0)))))
  (register-flycheck-disable-mode 'restclient-mode)
  (register-flycheck-disable-mode 'html-mode))



;;;
;;; string-inflection
;;;
(if (fboundp #'string-inflection-all-cycle)
    (progn
      (defun my-string-inflection-cycle-auto ()
        "switching by major-mode"
        (interactive)
        (cond
         ;; for emacs-lisp-mode
         ((eq major-mode 'emacs-lisp-mode)
          (string-inflection-all-cycle))
         ;; for java
         ((eq major-mode 'java-mode)
          (string-inflection-java-style-cycle))
         (t
          ;; default
          (string-inflection-ruby-style-cycle))))
      (global-set-key (kbd "M-u") #'my-string-inflection-cycle-auto))

  ;; string-inflection がないときは代用の関数を使う
  (progn
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
    (global-set-key "\M-u" #'changecase-word)))


;;;
;;; apache-mode
;;;
(when (autoload-if-found 'apache-mode "apache-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
  (add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
  (add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
  (add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
  (add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode)))


;;;
;;; mozc-temp
;;;
(when (fboundp 'mozc-mode)
  (setq default-input-method "japanese-mozc"))
(when (and (setq mozc-helper-program-name (executable-find "mozc_emacs_helper"))
           (fboundp 'mozc-temp-convert))
  (setq mozc-candidate-style
        (if (require 'mozc-popup nil t) 'popup 'echo-area))
  (setq mozc-temp-remove-pre-space nil)
  (global-set-key (kbd "M-M") 'mozc-temp-convert-dwim))


;;;
;;; editorconfig
;;;
(when (fboundp 'editorconfig-mode)
  (editorconfig-mode 1))


;;;
;;; helm-qiita
;;;   インストール後初回起動時には touch ~/.emacs.d/helm-qiita すること。しばらくしたら使えるようになる
(when (and (featurep 'helm) (featurep 'helm-qiita))
  ; (setq helm-qiita-debug-mode t) ;; 何かあったときに使える https://twitter.com/masutaka/status/899204475683864580
  (setq helm-qiita-username "__hage")
  ; (setq helm-qiita-organization "Your Organization") ;; optional. For example, "feedforce"
  (setq helm-qiita-access-token "a85219b05c24563583640daeea5e2c6c1465a0bc") ;; See https://qiita.com/settings/applications
  (helm-qiita-initialize)
  )


;;;
;;; google-this
;;;
(when (fboundp 'google-this-search)
  (setq google-this-location-suffix "co.jp")
  (global-set-key (kbd "M-s M-g") 'google-this-search)
  (global-set-key (kbd "M-s M-l") 'google-this-lucky-search))


;;;
;;; urlenc
;;;
(when (featurep 'urlenc)
  (global-set-key (kbd "C-q e u") 'urlenc:encode-region)
  (global-set-key (kbd "C-q d u") 'urlenc:decode-region))


;;;
;;; Docker
;;;
(when (fboundp 'docker-compose-mode)
  (add-to-list 'auto-mode-alist '("docker-compose\\.yml\\'" . docker-compose-mode)))

(eval-after-load 'helm-files
  #'(progn
      (require 'docker-tramp-compat nil t)))


;;;
;;; dimmer
;;;
(when (fboundp 'dimmer-mode)
  (setq dimmer-fraction 0.3)
  (setq dimmer-exclusion-regexp "\\(^\\*helm\\)")
  (dimmer-mode t))

;;;
;;; zoom-window
;;;
(when (fboundp 'zoom-window-zoom)
  (global-set-key (kbd "C-w 1") 'zoom-window-zoom)
  (setq zoom-window-mode-line-color "#050")
  (eval-after-load "zomm-window"
    #'(progn
        (when (featurep 'elscreen)
          (setq zoom-window-use-elscreen t))
        (zoom-window-setup))))


;;;
;;; direx
;;;
(when (fboundp 'direx-project:jump-to-project-root-other-window)
  (autoload-if-found 'direx-project:find-project-root-noselect "direx-project")

  (defun* my-direx-project:find-root (&optional (dir (or buffer-file-name default-directory)))
    "find project root directory of DIR,
guessing a default from current buffer file name or default directory. "
    (direx-project:find-project-root-noselect dir))

  (defun my-direx:dwim (uarg)
    (interactive "P")
    (if (and (my-direx-project:find-root) (not uarg))
        (direx-project:jump-to-project-root-other-window)
      (direx:jump-to-directory-other-window)))
  (global-set-key (kbd "C-x C-d") #' my-direx:dwim))


;;;
;;; showcss-mode
;;;
(when (fboundp 'showcss-mode)
  (eval-after-load "show-css"
    #'(progn
        (set-face-background 'showcss/region-face "gray5")
        (set-face-background 'showcss/header-filepath-face nil)
        (set-face-foreground 'showcss/header-filepath-face "gray60")
        (set-face-background 'showcss/source-region-face "gray20")
        (set-face-foreground 'showcss/source-region-face "gray80")))

  (defun sm/toggle-showcss()
    "Toggle showcss-mode"
    (interactive)
    (if (derived-mode-p
         'html-mode
         'nxml-mode
         'nxhtml-mode
         'web-mode
         'handlebars-mode)
        (showcss-mode 'toggle)
      (message "Not in an html mode")))
  (global-set-key (kbd "C-c C-k") 'sm/toggle-showcss))

;;;
;;; faces
;;;
(if (not window-system)
    (setq frame-background-mode 'dark))
(set-face-foreground 'mode-line "chartreuse1")
(set-face-background 'mode-line "#607360")
(set-face-underline 'mode-line nil)
(set-face-foreground 'mode-line-inactive "gray10")
(set-face-background 'mode-line-inactive "gray50")
(set-face-foreground 'font-lock-comment-face "#a0a3b5")
(set-face-foreground 'highlight nil)
(set-face-background 'highlight "palegreen4")
(set-face-background 'region "LightSteelBlue4")
(set-face-foreground 'font-lock-string-face "mediumpurple1")
(set-face-foreground font-lock-builtin-face "CornflowerBlue")
(set-face-foreground font-lock-keyword-face "DeepSkyBlue")
(set-face-bold font-lock-keyword-face t)
(set-face-foreground 'minibuffer-prompt "LawnGreen")
(set-face-foreground 'font-lock-constant-face "gold3")
(set-face-foreground 'default "snow")
(set-face-background 'default "black")
(set-face-foreground 'match "black")
(set-face-background 'match "yellow1")
(set-face-foreground 'font-lock-type-face "LimeGreen")
(set-face-background 'secondary-selection "MediumPurple4")
(set-face-foreground 'font-lock-function-name-face "MediumTurquoise")
(set-face-foreground 'font-lock-variable-name-face "DarkOliveGreen1")

(eval-after-load 'em-prompt
  #'(progn
      (set-face-foreground 'eshell-prompt "green")))
(eval-after-load 'em-ls
  #'(progn
      (set-face-foreground 'eshell-ls-directory "dodgerblue")))
(eval-after-load 'popup
  #'(progn
      (set-face-background 'popup-scroll-bar-foreground-face "gray40")))
(eval-after-load 'smerge-mode
  #'(progn
      (set-face-foreground 'smerge-markers "#fff")
      (set-face-foreground 'smerge-base "#000")
      (set-face-foreground 'smerge-mine nil)
      (set-face-background 'smerge-mine "#100")
      (set-face-foreground 'smerge-other nil)
      (set-face-background 'smerge-other "#001")
      (set-face-foreground 'smerge-markers "#000")
      (set-face-foreground 'smerge-refined-added "#000")
      (set-face-foreground 'smerge-refined-removed "#000")))
(eval-after-load 'ediff-init
  #'(progn
      (set-face-background 'ediff-odd-diff-A "gray10")
      (set-face-background 'ediff-odd-diff-B "gray10")
      (set-face-background 'ediff-odd-diff-C "gray10")
      (set-face-underline  'ediff-odd-diff-A t)
      (set-face-underline  'ediff-odd-diff-B t)
      (set-face-underline  'ediff-odd-diff-C t)

      (set-face-foreground 'ediff-fine-diff-A "#000")
      (set-face-foreground 'ediff-fine-diff-B "#000")
      (set-face-foreground 'ediff-fine-diff-C "#000")
      (set-face-foreground 'ediff-fine-diff-Ancestor "#000")
      (set-face-foreground 'ediff-current-diff-A "#000")
      (set-face-foreground 'ediff-current-diff-B "#000")
      (set-face-foreground 'ediff-current-diff-C "#000")
      (set-face-foreground 'ediff-current-diff-Ancestor "#000")))
(eval-after-load 'package
  #'(progn
      (set-face-foreground 'package-name "aquamarine")))

(provide 'init)
;;; init.el ends here
