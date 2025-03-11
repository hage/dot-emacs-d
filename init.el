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

(custom-set-variables `(gc-cons-threshold ,(* gc-cons-threshold 75)))

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
(setq garbage-collection-messages t)
(setq native-comp-async-report-warnings-errors nil)

;; 秘密の値をロード
(load "~/.emacs.d/.secret.el")


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

(leaf language-environment
  ;; cf. https://apribase.net/2024/07/26/emacs-language-environment-mac/
  :config
  ;; 言語環境を UTF-8 にします。coding system と input method のデフォルト設定が決まります。
  (set-language-environment 'utf-8)
  ;; coding system のデフォルト言語を指定します。
  (set-default-coding-systems 'utf-8-unix)
  ;; 自動認識テキストエンコーディングのリストを並び替えます。セットした言語がリストの先頭になります。
  (prefer-coding-system 'japanese-shift-jis)
  (prefer-coding-system 'utf-8)

  (when (and (memq window-system '(ns nil))
             (fboundp 'mac-get-current-input-source))
    (when (version< "27.0" emacs-version) ;; 27.0 以上だったら
      ;; Required for some cases when auto detection is failed or the locale is "en".
      (setq mac-default-input-source "jp.monokakido.inputmethod.Kawasemi4.Japanes"))
    (mac-input-method-mode 1)))

(leaf themes
  :custom-face
  (mode-line . '((t (:box "dark olive green"))))
  (mode-line-inactive . '((t (:box "#445"))))
  :config
  (leaf color-theme-sanityinc-solarized
    :disabled t
    :doc "A version of Ethan Schoonover's Solarized themes"
    :req "emacs-24.1" "cl-lib-0.6"
    :tag "themes" "faces" "emacs>=24.1"
    :added "2021-03-19"
    :url "https://github.com/purcell/color-theme-sanityinc-solarized"
    :emacs>= 24.1
    :ensure t
    :config (load-theme 'sanityinc-solarized-dark t))
  (leaf doom-themes
    :doc "an opinionated pack of modern color-themes"
    :req "emacs-25.1" "cl-lib-0.5"
    :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
    :added "2021-04-09"
    :url "https://github.com/hlissner/emacs-doom-theme"
    :emacs>= 25.1
    :ensure t
    :config  (load-theme 'doom-one t)))

(leaf basic-config
  :init (progn
          (put 'narrow-to-region 'disabled nil)
          (line-number-mode t)
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
           (blink-cursor-blinks        . 0)
           (confirm-kill-emacs         . #'yes-or-no-p)
           (ring-bell-function         . (lambda () (princ "[RING] ")))
           (shell-file-name            . "/bin/sh")
           (package-hidden-regexps     . '("^\\(ac\\|helm\\|ivy\\|counsel\\|auto-complete\\)-"))))

(leaf basic-key-bindings
  :bind (("M-n" . indent-and-next-line))
  :init
  (global-unset-key (kbd "C-q"))
  (global-unset-key (kbd "C-w"))
  (global-unset-key (kbd "C-x DEL"))
  (global-unset-key (kbd "C-<wheel-down>"))
  (global-unset-key (kbd "C-<wheel-up>"))
  ;; インデントして次の行に移動する
  (defun indent-and-next-line ()
    (interactive)
    (indent-according-to-mode)
    (forward-line 1))
  (define-key global-map "\M-n" 'indent-and-next-line)

  (leaf basic-key-bindings-bind
    :init
    (defun switch-to-used-buffer ()
      (interactive)
      (switch-to-buffer nil))

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
           ("C-M-_"       . indent-region)
           ("M-h"         . backward-kill-word)
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
           ("M-g g"       . goto-line)
           ("M-f"         . forward-word)
           ("M-b"         . backward-word)
           ("M-F"         . forward-to-word)
           ("M-B"         . backward-to-word)
           ("M-O"         . switch-to-used-buffer)
           ("M-N"         . down-list)
           ("M-I"         . imenu))))

(leaf window-system
  :if (window-system)
  :init
  (global-unset-key (kbd "C-t"))
  (set-face-foreground 'vertical-border "#685")
  (defun my-emacs-startup-hook-handler ()
    (setq ns-use-native-fullscreen t)
    (toggle-frame-fullscreen)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode 1)
    (global-hl-line-mode 1)
    (global-so-long-mode))
  (leaf all-the-icons
    :doc "A library for inserting Developer icons"
    :req "emacs-24.3"
    :tag "lisp" "convenient" "emacs>=24.3"
    :added "2021-04-08"
    :url "https://github.com/domtronn/all-the-icons.el"
    :emacs>= 24.3
    :ensure t
    :require t)
  (leaf all-the-icons-completion
    :doc "Add icons to completion candidates"
    :req "emacs-26.1" "all-the-icons-5.0"
    :tag "lisp" "convenient" "emacs>=26.1"
    :url "https://github.com/iyefrat/all-the-icons-completion"
    :added "2022-05-25"
    :emacs>= 26.1
    :ensure t
    :after all-the-icons
    :init (all-the-icons-completion-mode +1)
    (leaf all-the-icons-completion-magnalia-mode
      :after marginalia all-the-icons
      :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)))
  (leaf doom-modeline
    :doc "A minimal and modern mode-line"
    :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
    :tag "mode-line" "faces" "emacs>=25.1"
    :added "2021-04-08"
    :url "https://github.com/seagle0128/doom-modeline"
    :emacs>= 25.1
    :ensure t
    :after all-the-icons
    :custom ((doom-modeline-buffer-file-name-style . 'relative-from-project))
    :init (doom-modeline-mode 1))
  (leaf pos-tip
    :doc "Show tooltip at point"
    :tag "tooltip"
    :added "2021-04-12"
    :ensure t
    :custom ((pos-tip-border-width . 2)
             (pos-tip-internal-border-width . 5)
             (pos-tip-foreground-color . "gray20")
             (pos-tip-background-color . "light cyan")))
  (leaf font-setting
    :tag "out-of-MELPA"
    :added "2021-04-27"
    :config
    (leaf font-setting-source-han-mono
      :disabled nil
      :init
      (create-fontset-from-ascii-font "Source Han Mono:slant=normal:size=11" nil "SourceHanMono")
      (set-fontset-font "fontset-SourceHanMono" 'unicode "DFPLeiSho-SB-14" nil 'append))
    (leaf font-setting-jetbrains
      :disabled nil
      :init
      (create-fontset-from-ascii-font "JetBrains Mono:slant=normal:size=12" nil "JetBrains")
      (set-fontset-font "fontset-JetBrains" 'unicode "DFPLeiSho-SB-14" nil 'append))
    (leaf font-setting-victor
      :disabled t
      :init
      (create-fontset-from-ascii-font "Victor Mono:slant=normal:size=12" nil "Victor")
      (set-fontset-font "fontset-Victor" 'unicode "DFPLeiSho-SB-14" nil 'append))
    (add-to-list 'default-frame-alist '(font . "fontset-JetBrains")))
  (leaf switch-to-terminal
    :init
    (defun my-switch-terminal-emulator()
      "Switch to terminal emulator (iTerm2 etc.)."
      (interactive)
      (make-process :name "iterm" :command '("open" "-a" "iTerm")))
    :bind (("C-t C-t" . my-switch-terminal-emulator))) ; -nw で tmux の C-t C-t でシェルに切り替えていた代わり
  :hook (emacs-startup-hook . my-emacs-startup-hook-handler))

(leaf not-window-system
  :if (not (window-system))
  :config
  (menu-bar-mode -1)
  (set-face-background 'vertical-border "#334")
  (set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ? ))
  (leaf pbcopy
    :if (string-equal system-type "darwin")
    :doc "Emacs Interface to pbcopy"
    :tag "pbcopy" "osx" "mac"
    :added "2021-03-23"
    :url "https://github.com/jkp/pbcopy.el"
    :ensure t
    :init (turn-on-pbcopy))
  (leaf keyboard-quit
    :config
    (defun keyboard-quit-advice-before ()
      (run-with-timer 0.2 nil
                      (lambda ()
                        (redraw-display))))
    :advice ((:before keyboard-quit keyboard-quit-advice-before))))

(leaf my-preview
  ;; `my-preview-commands`は、メジャーモードごとにファイルをプレビュー
  ;; するために実行されるシェルコマンドを定義するための変数です。この
  ;; 変数は、各メジャーモードとそれに対応するシェルコマンドのペアを含
  ;; むアリストとして構成されています。

  ;; ### 変数の構造:
  ;; - **形式**: `((モード名 . "コマンド") ...)`
  ;; - **例**:
  ;; - `(ruby-mode . "ruby '{file}'")`: Rubyモードの場合、`ruby`コマンドを使用してファイルをプレビュー。
  ;; - `(python-mode . "python '{file}'")`: Pythonモードの場合、`python`コマンドを使用。

  ;; ### .dir-locals.elでの設定
  ;; `.dir-locals.el`ファイルを利用することで、特定のプロジェクトやディ
  ;; レクトリ内で使用する設定を指定できます。このファイルに
  ;; `my-preview-commands`を設定することで、プロジェクトに特有のプレ
  ;; ビューコマンドを簡単に適用できます。

  ;; ### 設定例:
  ;; 以下のように`.dir-locals.el`ファイルを作成または編集して、プロジェ
  ;; クトごとのプレビューコマンドを指定します。

  ;; ```elisp
  ;; ((nil
  ;;   (my-preview-commands . ((ruby-mode . "ruby -c '{file}'")  ; Rubyメジャーモードのコマンド
  ;;                           (python-mode . "python '{file}'") ; Pythonメジャーモードのコマンド
  ;;                           (javascript-mode . "node '{file}'") ; JavaScriptメジャーモードのコマンド
  ;;                           ;; 他のメジャーモードに合わせてコマンドを追加
  ;;                           (t . "less '{file}'")))))
  ;; ```

  ;; `my-preview-commands`は、各メジャーモードに応じて特定のシェルコマ
  ;; ンドを定義する変数です。この変数内のコマンド文字列に含まれるプレー
  ;; スホルダー`{file}`と`{project-root}`は、実行時にそれぞれ現在のファ
  ;; イル名とプロジェクトのルートパスに置き換えられ、具体的なファイル
  ;; 操作を可能にします。`{file}`はバッファのファイルを指し、
  ;; `{project-root}`はそのファイルが属するプロジェクトのルートディレ
  ;; クトリを指します。これにより、ユーザーは柔軟に異なる環境に合わせ
  ;; たコマンドを設定できます。

  ;; ### まとめ:
  ;; `my-preview-commands`変数は、メジャーモードに応じたファイルプレ
  ;; ビューコマンドを柔軟に設定するために使用されます。
  ;; `.dir-locals.el`を利用することで、プロジェクト毎にこの設定を変更
  ;; できるため、様々な開発環境に応じたカスタマイズが可能です。


  :init
  (defvar my-preview-commands
    '((ruby-mode . "ruby -c '{file}'")  ; Rubyメジャーモードのコマンド
      (python-mode . "python '{file}'") ; Pythonメジャーモードのコマンド
      (javascript-mode . "node '{file}'") ; JavaScriptメジャーモードのコマンド
      ;; 他のメジャーモードに合わせてコマンドを追加
      (t . "less '{file}'"))            ; デフォルトコマンド
    "Alist of preview commands based on major modes.

Each entry is of the form (MODE . COMMAND), where MODE is a major mode
symbol and COMMAND is a shell command. The placeholders '{file}' and
'{project-root}' can be used within COMMAND and will be replaced
with the current buffer's file name and the project root directory,
respectively.")

  ;; .dir-locals.el で `my-preview-commands' を設定時の警告を抑止
  (put 'my-preview-commands 'safe-local-variable (lambda (_) t))

  (defun my-project-root ()
    "Return the project root directory using VC Mode."
    (let ((root (or (vc-root-dir) default-directory))) ; VC-Mode (Git, Mercurialなど)を使用している場合
      (expand-file-name root)))         ; チルダをフルパスに展開

  (defun my-preview ()
    "Preview the current buffer's file using the command based on the major mode."
    (interactive)
    (let* ((file-name (buffer-file-name))
           (project-root (my-project-root))
           (my-preview-command (or (cdr (assoc major-mode my-preview-commands))
                                   "less '{file}'"))) ; デフォルト値
      (if file-name
          (let* ((command (replace-regexp-in-string
                           "{file}" (shell-quote-argument file-name)
                           (replace-regexp-in-string
                            "{project-root}" (shell-quote-argument project-root)
                            my-preview-command)))
                 ;; rbenv_versionをクリアしてコマンドを実行
                 (process (start-process "my-preview" "*my-preview-output*" "zsh" "-c"
                                         (format "unset RBENV_VERSION; %s" command))))
            ;; プロセスの出力を表示するためのフック
            (set-process-sentinel process
                                  (lambda (proc _)
                                    (when (eq (process-status proc) 'exit) ; プロセスが終了したとき
                                      (with-current-buffer "*my-preview-output*"
                                        (goto-char (point-max)) ; 出力の最後にカーソルを移動
                                        (insert "Preview finished.\n")))))
            ;; バッファをクリアし、モードを設定する
            (with-current-buffer "*my-preview-output*"
              (erase-buffer)            ; バッファをクリア
              (fundamental-mode)        ; 基本モードを設定
              (local-set-key (kbd "C-c C-c") 'revert-buffer) ; リロード用のキーを設定
              (insert "Preview Output:\n\n"))       ; ヘッダーを追加
            (display-buffer "*my-preview-output*")) ; バッファを表示
        (message "This buffer is not visiting a file."))))
  :bind
  (("C-q C-v" . my-preview)))

(leaf ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter-hook . my-colorize-compilation-buffer))

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

(leaf delete-trailing-whitespace-before-save
  :doc "セーブ時行末にスペースがあったら消すかどうか聞く"
  :init
  (setq delete-trailing-lines nil)
  (defvar my-delete-trailing-whitespace-enable-mode
    '(text-mode markdown-mode html-mode scss-mode web-mode)
    "セーブ時行末スペースを削除するかどうか聞くモードの指定")
  (defun before-save-hook-handler-of-delete-trailing-whitespace ()
    (when (and
           (buffer-file-name)           ; ファイル付きのバッファで
           (string-match "[ 	]$" (buffer-string)) ; 行末が空白の行があって
           (or (not (memq major-mode my-delete-trailing-whitespace-enable-mode))
               (y-or-n-p "detected trailing whitespace. delete it?")))
      (delete-trailing-whitespace)))
  :hook (before-save-hook . before-save-hook-handler-of-delete-trailing-whitespace))

(leaf yank-and-indent
  :init
  (defun yank-and-indent ()
    "Yank and indent it."
    (interactive)
    (yank)
    (save-excursion
      (indent-region (mark) (point))))
  :hook
  ((emacs-lisp-mode-hook ruby-mode-hook js2-mode-hook typescript-mode-hook web-mode-hook css-mode-hook php-mode-hook) .
   (lambda ()
     (local-set-key (kbd "C-y") #'yank-and-indent)
     (local-set-key (kbd "C-M-y") #'yank))))

(leaf my-comment-sexp
  :init
  (defun my-comment-sexp ()
    "Comment out the sexp region"
    (interactive)
    (save-excursion
      (sp-mark-sexp)
      (comment-region (point) (mark))))
  :bind (("M-C-;" . my-comment-sexp)))

(leaf copy-buffer-file-name-to-kill-ring
  :init
  (defun copy-buffer-file-name-to-kill-ring (prefix)
    "Copy the current buffer's file name to the kill ring.
If called with a universal argument, copy the full file name (directory + name) and show a message."
    (interactive "P")
    (if (buffer-file-name)
        (let ((file-name (buffer-file-name)))
          (if prefix
              (progn
                (kill-new file-name)
                (message "Copied buffer full file name: %s" file-name))
            (let ((base-name (file-name-nondirectory file-name)))
              (kill-new base-name)
              (message "Copied buffer file name: %s" base-name))))
      (message "This buffer is not visiting a file.")))
  :bind (("C-q C-f" . copy-buffer-file-name-to-kill-ring)))

(leaf my-move-file-current-buffer
  :init
  (defun my-move-file-current-buffer (new-name)
    "Rename the current buffer's file to NEW-NAME and delete the original file.
If a file with the same name already exists, prompt for confirmation."
    (interactive "FNew file name: ")
    (let ((current-file (buffer-file-name)))
      (when current-file
        (if (file-exists-p new-name)
            (if (y-or-n-p (format "File %s already exists. Overwrite? " new-name))
                (progn
                  (rename-file current-file new-name t) ; force is t
                  (set-visited-file-name new-name)
                  (set-buffer-modified-p nil))
              (message "Rename canceled."))
          (rename-file current-file new-name nil) ; normal rename
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(leaf zenkaku-hankaku-translate
  :init
  (defun my-zenkaku-to-hankaku-region
      (interactive)
		(japanese-hankaku-region (region-beginning) (region-end) t))
  :bind
  (("C-q h" . my-zenkaku-to-hankaku-region)
   ("C-q z" . japanese-zenkaku-region)))

(leaf eldoc
  :blackout t)

(leaf autorevert
  :blackout auto-revert-mode)

(leaf show-paren-mode
  :custom ((show-paren-style . 'mixed))
  :config
  (show-paren-mode t)
  (set-face-foreground 'show-paren-match "chartreuse1")
  (set-face-bold 'show-paren-match-expression nil))

(leaf recentf
  :custom ((recentf-max-saved-items . 1024)
           (recentf-auto-cleanup    . 'never)))

(leaf save-place
  :init
  (save-place-mode 1))

(leaf repeat-mode
  :config (repeat-mode 1)
  :custom ((repeat-exit-timeout . 2)))

(leaf auto-insert-mode
  :url "https://higepon.hatenablog.com/entry/20080731/1217491155"
  :custom `((auto-insert-directory . "~/.emacs.d/template"))
  :init
  (auto-insert-mode 1)
  (setq auto-insert-alist (append '(("\\.jsx$" . ["template.jsx" my-template])) auto-insert-alist))
  (defvar template-replacements-alists
    '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      ("%include-guard%"    . (lambda () (format "__SCHEME_%s__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))
  (defun my-template ()
    (time-stamp)
    (mapc #'(lambda(c)
              (progn
                (goto-char (point-min))
                (replace-string (car c) (funcall (cdr c)) nil)))
          template-replacements-alists)
    (goto-char (point-max))
    (message "done."))
  :hook ((find-file-not-found-hooks . auto-insert)))

(leaf ffap
  :doc "find file (or url) at point"
  :tag "builtin"
  :added "2021-04-19"
  :bind (("M-g M-g" . ffap)))

(leaf transpose-frame
  :doc "Transpose windows arrangement in a frame"
  :tag "window"
  :added "2021-04-22"
  :ensure t
  :bind (("C-w '" . transpose-frame)
         ("C-w \"" . flop-frame)))

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
  :custom ((migemo-command . "/opt/homebrew/bin/cmigemo")
           (migemo-dictionary . "/opt/homebrew/share/migemo/utf-8/migemo-dict")
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
  :commands magit-gitdir
  :bind (("C-q g g" . magit-status)
         ("C-q g l" . magit-log-all)
         ("C-q g d" . magit-diff-buffer-file))
  :config
  (set-face-foreground 'magit-branch-current "#33E9FF")
  (set-face-bold 'magit-branch-current t)
  (defun git-commit-prefix-select ()
    (if (= 10 (following-char))
        (insert ":")))
  (add-hook 'git-commit-setup-hook 'git-commit-prefix-select))

(leaf git-gutter
  :if (not (window-system))
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
         ("M-g M-p" . git-gutter:previous-hunk)
         ("C-q g r" . git-gutter:revert-hunk)))

(leaf git-gutter-fringe
  :if (window-system)
  :ensure t
  :blackout t
  :init (global-git-gutter-mode)
  :bind (("M-g M-n" . git-gutter:next-hunk)
         ("M-g M-p" . git-gutter:previous-hunk)))

(leaf treemacs
  :doc "A tree style file explorer package"
  :req "emacs-26.1" "cl-lib-0.5" "dash-2.11.0" "s-1.12.0" "ace-window-0.9.0" "pfuture-1.7" "hydra-0.13.2" "ht-2.2" "cfrs-1.3.2"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-04-06"
  :emacs>= 26.1
  :ensure t
  :bind (("C-x d" . treemacs))
  :config (progn (treemacs-project-follow-mode 1)
                 (set-face-background 'treemacs-hl-line-face "#443")))
(leaf treemacs-persp
  :doc "Persp-mode integration for treemacs"
  :req "emacs-26.1" "treemacs-0.0" "persp-mode-2.9.7" "dash-2.11.0"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2023-10-25"
  :emacs>= 26.1
  :ensure t
  :after treemacs persp-mode
  :config (treemacs-set-scope-type 'Perspectives))
(leaf treemacs-all-the-icons
  :doc "all-the-icons integration for treemacs"
  :req "emacs-26.1" "all-the-icons-4.0.1" "treemacs-0.0"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-04-06"
  :emacs>= 26.1
  :ensure t
  :after all-the-icons treemacs
  :config
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons")
  (treemacs-refresh))

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/minad/vertico"
  :added "2022-03-29"
  :emacs>= 27.1
  :ensure t
  :after all-the-icons
  :init
  (vertico-mode 1)
  :custom ((vertico-count . 25))
  :bind ((vertico-map
          ("C-l" . vertico-directory-up))))
(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :req "emacs-26.1"
  :tag "extensions" "emacs>=26.1"
  :url "https://github.com/oantolin/orderless"
  :added "2022-03-29"
  :emacs>= 26.1
  :ensure t
  :custom ((completion-styles . '(orderless))))
(leaf savehist
  :doc "Save minibuffer history"
  :tag "builtin"
  :added "2022-03-29"
  :config (savehist-mode))
(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :url "https://github.com/minad/marginalia"
  :added "2022-03-30"
  :emacs>= 26.1
  :ensure t
  :after vertico
  :config (marginalia-mode)
  :custom ((marginalia-align . 'right)))
(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :url "https://github.com/minad/consult"
  :added "2022-03-29"
  :emacs>= 26.1
  :ensure t
  :config (progn
            (recentf-mode)

            (setq consult-buffer-filter
                  (append consult-buffer-filter
                          '("\\`magit-"
                            "\\`\\*ruby-ls")))
            (delete-dups consult-buffer-filter)

            (define-key consult-narrow-map
              (vconcat consult-narrow-key "?") #'consult-narrow-help)
            (consult-customize
             consult--source-recent-file
             consult--source-project-recent-file
             :preview-key (list
                           (kbd "C-o")
                           :debounce 0.5 'any)))
  (leaf my-consult-project-file
    :after consult project
    :config
    (defvar my-consult--source-project-file
      `(:name "Project Whole File"
              :narrow   (?p . "Project")
              :category file
              :face     consult-file
              :history  file-name-history
              :state    ,#'consult--file-state
              :new      ,#'consult--file-action
              :items
              ,(lambda ()
                 (let ((current-project (project-current)))
                   (if current-project
                       (project-files current-project)
                     nil))))
      "Project file candidate source for `project-files'.")
    (add-to-list 'consult-buffer-sources 'my-consult--source-project-file t)
    (setq consult-project-buffer-sources
          (list
           `(:hidden nil :narrow ?b ,@consult--source-project-buffer)
           `(:hidden nil :narrow ?f ,@my-consult--source-project-file))))
  :bind (("M-I"     . consult-imenu-multi)
         ("C-s"     . consult-line)
         ("M-g g"   . consult-goto-line)
         ("C-x b"   . consult-project-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-h a"   . describe-symbol)
         ("M-s M-s" . consult-git-grep)
         ("M-s g"   . consult-grep)
         ("M-s s"   . consult-line-multi)
         ("M-s F"   . consult-find)
         ("M-s o"   . consult-outline)
         ("M-y"     . consult-yank-pop)))
(leaf embark
  :doc "Conveniently act on minibuffer completions"
  :req "emacs-26.1"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/oantolin/embark"
  :added "2022-04-04"
  :emacs>= 26.1
  :ensure t
  :bind (("C-M-m" . embark-act)))
(leaf embark-consult
  :doc "Consult integration for Embark"
  :req "emacs-26.1" "embark-0.12" "consult-0.10"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/oantolin/embark"
  :added "2022-04-03"
  :emacs>= 26.1
  :ensure t
  :after vertico consult
  :config (define-key vertico-map (kbd "M-o") #'embark-act))
(leaf my-consult-buffer
  :init
  (defun my-consult-project-buffer (uarg)
    (interactive "P")
    (if (or uarg (string-prefix-p "*scratch*" (buffer-name)) (not (project-current)))
        (consult-buffer)
      (consult-project-buffer)))
  :bind (("C-M-o" . my-consult-project-buffer)))
(leaf orderless-migemo
  :doc "consultをmigemoizeしたい (未完→だいたいできた)"
  :url "https://nyoho.jp/diary/?date=20210615"
  :after orderless marginalia migemo
  :init
  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  (orderless-define-completion-style orderless-default-style
    "An orderless completion style with literals."
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp)))

  (orderless-define-completion-style orderless-migemo-style
    "An orderless completion style with migemo."
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp
                                 orderless-migemo)))
  (setq completion-category-overrides
        '((command (styles orderless-default-style))
          (file (styles orderless-migemo-style))
          (buffer (styles orderless-migemo-style))
          (symbol (styles orderless-default-style))
          (consult-location (styles orderless-migemo-style)) ; category `consult-location' は `consult-line' などに使われる
          (consult-multi (styles orderless-migemo-style)) ; category `consult-multi' は `consult-buffer' などに使われる
          (org-roam-node (styles orderless-migemo-style)) ; category `org-roam-node' は `org-roam-node-find' で使われる
          (unicode-name (styles orderless-migemo-style))
          (variable (styles orderless-default-style))))
  :config
  (add-to-list 'marginalia-prompt-categories
               '("\\<File\\>" . file)))

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
          ("C-m" . nil)
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-i" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :global-minor-mode global-company-mode
  :custom ((company-idle-delay            . .8)
           (company-minimum-prefix-length . 3)
           (company-transformers          . '(company-sort-by-occurrence)))

  :hook (((ruby-mode-hook js2-mode-hook) .
          (lambda ()
            (setq company-backends
                  '((company-capf company-tabnine company-dabbrev company-yasnippet)
                    company-files company-semantic company-keywords)))))

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
    :disabled nil
    :doc "A company-mode backend for TabNine"
    :req "emacs-25" "company-0.9.3" "cl-lib-0.5" "dash-2.16.0" "s-1.12.0" "unicode-escape-1.1"
    :tag "convenience" "emacs>=25"
    :added "2021-03-23"
    :url "https://github.com/TommyX12/company-tabnine/"
    :emacs>= 25
    :ensure t
    :after company)
  (leaf company-quickhelp
    :doc "Popup documentation for completion candidates"
    :req "emacs-24.3" "company-0.8.9" "pos-tip-0.4.6"
    :tag "quickhelp" "documentation" "popup" "company" "emacs>=24.3"
    :added "2021-04-12"
    :url "https://www.github.com/expez/company-quickhelp"
    :emacs>= 24.3
    :ensure t
    :after company pos-tip
    :init (company-quickhelp-mode))
  (leaf company-box
    :if (window-system)
    :doc "Company front-end with icons"
    :req "emacs-26.0.91" "dash-2.19.0" "company-0.9.6" "frame-local-0.0.1"
    :tag "convenience" "front-end" "completion" "company" "emacs>=26.0.91"
    :url "https://github.com/sebastiencs/company-box"
    :added "2022-04-05"
    :emacs>= 26.0
    :ensure t
    :hook ((company-mode-hook . company-box-mode))))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :added "2021-03-30"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :custom (flycheck-disabled-checkers . '(emacs-lisp-checkdoc))
  :bind (("C-q C-e" . flycheck-list-errors))
  :config
  (leaf flycheck-pos-tip
    :doc "Display Flycheck errors in GUI tooltips"
    :req "emacs-24.1" "flycheck-0.22" "pos-tip-0.4.6"
    :tag "convenience" "tools" "emacs>=24.1"
    :added "2021-04-18"
    :url "https://github.com/flycheck/flycheck-pos-tip"
    :emacs>= 24.1
    :ensure t
    :after flycheck pos-tip
    :if (window-system)
    :custom ((flycheck-pos-tip-timeout . 20)
             (flycheck-display-errors-function . #'flycheck-pos-tip-error-messages))
    :config
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode))))

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
  (yas-global-mode)
  (defun yas--modes-to-activate (&optional mode)
    "Compute list of mode symbols that are active for `yas-expand' and friends."
    (defvar yas--dfs)        ;We rely on dynbind.  We could use `letrec' instead!
    (let* ((explored (if mode (list mode) ; Building up list in reverse.
                       (cons major-mode (reverse yas--extra-modes))))
           (yas--dfs
            (lambda (mode)
              (cl-loop for neighbour
                       in (cl-list* 'fundamental-mode
                                    (and (fboundp mode) (symbol-function mode))
                                    (gethash mode yas--parents))
                       when (and neighbour
                                 (not (memq neighbour explored))
                                 (symbolp neighbour))
                       do (push neighbour explored)
                       (funcall yas--dfs neighbour)))))
      (mapc yas--dfs explored)
      (nreverse explored))))

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
  :init
  (popwin-mode 1)
  (customize-set-variable
   'popwin:special-display-config
   (append '((help-mode :position right :width 81)
             ("*compilation*" :height .4 :stick t)
             ("^\\*eshell.*\\*\\'" :position bottom :height .4 :stick t :regexp t)
             ("*Backtrace*" :noselect t)
             ("*xref*")
             ("*Messages*")
             ("*Compile-Log*")
             ("*osx-dictionary*")
             ("*GPTel Suggestion*" :position bottom :height .3)
             ("*Embark Export: .*" :position bottom :height .6 :regexp t :dedicated t :stick t)
             ("*my-preview-output*" :position bottom :height .4 :dedicated t :noselect t :stick t))
            (cadar (get 'popwin:special-display-config 'standard-value)))))

(leaf smartparens
  :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs."
  :req "dash-2.13.0" "cl-lib-0.3"
  :tag "editing" "convenience" "abbrev"
  :added "2021-03-23"
  :url "https://github.com/Fuco1/smartparens"
  :ensure t
  :defer-config (require 'smartparens-config)
  :init (smartparens-global-mode)
  :bind ((smartparens-mode-map
          ("C-M-k" . sp-kill-hybrid-sexp)
          ("M-R" . sp-raise-sexp)
          ("M-D" . sp-splice-sexp)
          ("M-J" . sp-backward-barf-sexp)
          ("M-H" . sp-backward-slurp-sexp)
          ("M-L" . sp-forward-slurp-sexp)
          ("M-K" . sp-forward-barf-sexp))))

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
  :custom-face
  ;; これが有効になるのは persp-modestring-short が nil のとき
  (persp-selected-face . '((t (:foreground "#eee" :underline t :bold nil))))
  :init
  (persp-mode)

  (defvar my-persp-loaded-state-p nil "persp-state-load をしたとき t に設定される")

  (defun my-persp-state-load-hook-handler ()
    (setq my-persp-loaded-state-p t))

  (defun kill-emacs-hook-handler-of-perspective ()
    "persp-state-loadをしたときにのみ終了時に自動的に状態を記録する"
    (when (and my-persp-loaded-state-p (y-or-n-p "save a perspective state?"))
      (persp-state-save persp-state-default-file)))

  :hook ((kill-emacs-hook             . kill-emacs-hook-handler-of-perspective)
         (persp-state-after-load-hook . my-persp-state-load-hook-handler))

  :custom `((persp-mode-prefix-key    . ,(kbd "M-t"))
            (persp-sort               . 'created)
            (persp-state-default-file . ,(concat user-emacs-directory ".persp-save.el"))
            (persp-modestring-short   . t))
  :config
  (defun my-persp-switch-to-main ()
    (interactive)
    (persp-switch "main"))
  :bind ((persp-mode-map
          ("M-t M-t" . persp-switch-last)
          ("M-t t"   . persp-switch)
          ("M-t l"   . persp-state-load)
          ("M-t s"   . persp-state-save)
          ("M-t k"   . persp-kill)
          ("M-t 0"   . my-persp-switch-to-main))))

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1" "pkg-info-0.4"
  :tag "convenience" "project" "emacs>=25.1"
  :added "2021-03-25"
  :url "https://github.com/bbatsov/projectile"
  :emacs>= 25.1
  :ensure t
  :bind ((projectile-mode-map
          ("C-w C-w" . my-run-eshell)))
  :custom ((projectile-mode-line-prefix . " P"))
  :init
  (projectile-mode)
  (defun my-run-eshell ()
    "invoke projectile-run-eshell when under git repository, otherwize invoke eshell"
    (interactive)
    (if (magit-gitdir)
        (projectile-run-eshell)
      (eshell))))

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs."
  :req "cl-lib-0.5"
  :tag "cursors" "editing"
  :added "2021-03-25"
  :url "https://github.com/magnars/multiple-cursors.el"
  :ensure t
  :bind (("C-w C-SPC" . mc/mark-all-dwim)
         ("C-w C-n"   . mc/mark-next-like-this)
         ("C-w n"   . mc/mark-next-word-like-this)
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
  :bind ("C-w C-w" . eshell))
(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2021-04-10"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :if  (memq window-system '(mac ns x))
  :custom ((exec-path-from-shell-warn-duration-millis . 5000)
           (exec-path-from-shell-shell-name . "/opt/homebrew/bin/zsh"))
  :config
  (exec-path-from-shell-initialize))

(leaf shell-service
  :require server
  :config
  (unless (server-running-p)
    ;; (setq server-socket-dir "~/.emacs.d/emacsserver")
    (server-start))
  (defun return-current-working-directory-to-shell ()
    (file-name-directory
     (expand-file-name
      (buffer-file-name (car (buffer-list)))))))

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
  :hook ((ruby-mode-hook php-mode-hook) .
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

(leaf ace-jump-mode
  :doc "a quick cursor location minor mode for emacs"
  :tag "cursor" "location" "motion"
  :added "2021-04-10"
  :url "https://github.com/winterTTr/ace-jump-mode/"
  :ensure t
  :bind (("C-M-s" . ace-jump-mode))
  :config
  (leaf ace-isearch
    :disabled t
    :doc "A seamless bridge between isearch, ace-jump-mode, avy, helm-swoop and swiper"
    :req "emacs-24"
    :tag "emacs>=24"
    :added "2021-04-10"
    :url "https://github.com/tam17aki/ace-isearch"
    :emacs>= 24
    :ensure t
    :after swiper
    :custom ((ace-isearch-input-length . 2))
    :bind (("C-s" . isearch-forward))
    :init
    (global-ace-isearch-mode +1)))

(leaf highlight-indent-guides
  :doc "Minor mode to highlight indentation"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2021-04-11"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :emacs>= 24.1
  :ensure t
  :custom ((highlight-indent-guides-method     . 'character)
           (highlight-indent-guides-responsive . t)))

(leaf info-ja
  :custom ((info-ja-directory . `,(concat user-emacs-directory "info")))
  :preface
  (defun info-find-node-info-ja (orig-fn filename &rest args)
    (apply orig-fn
           (pcase filename
             ("emacs" "emacs-ja")
             ("elisp" "elisp-ja")
             (t filename))
           args))
  :init
  (add-to-list 'Info-directory-list info-ja-directory)
  :advice (:around Info-find-node info-find-node-info-ja))

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :tag "faces"
  :added "2021-04-14"
  :url "http://elpa.gnu.org/packages/rainbow-mode.html"
  :ensure t
  :custom ((rainbow-html-colors  . t)
           (rainbow-x-colors     . t)
           (rainbow-latex-colors . t)
           (rainbow-ansi-colors  . t)))

(leaf docker
  :doc "Emacs interface to Docker"
  :req "dash-2.14.1" "docker-tramp-0.1" "emacs-24.5" "json-mode-1.7.0" "s-1.12.0" "tablist-0.70" "transient-0.2.0"
  :tag "convenience" "filename" "emacs>=24.5"
  :added "2021-04-14"
  :url "https://github.com/Silex/docker.el"
  :emacs>= 24.5
  :ensure t
  :after docker-tramp json-mode tablist
  :config
  (leaf docker-tramp
    :doc "TRAMP integration for docker containers"
    :req "emacs-24" "cl-lib-0.5"
    :tag "convenience" "docker" "emacs>=24"
    :added "2021-04-14"
    :url "https://github.com/emacs-pe/docker-tramp.el"
    :emacs>= 24
    :ensure t))

(leaf auto-highlight-symbol
  :doc "Automatic highlighting current symbol minor mode"
  :tag "convenience" "match" "face" "highlight"
  :added "2021-04-26"
  :url "http://github.com/jcs-elpa/auto-highlight-symbol"
  :config (progn
            (set-face-foreground 'ahs-plugin-default-face "#fff")
            (set-face-background 'ahs-plugin-default-face "DarkOliveGreen4")
            (set-face-foreground 'ahs-plugin-default-face-unfocused "gray")
            (set-face-background 'ahs-plugin-default-face-unfocused "DarkOliveGreen4")
            (set-face-foreground 'ahs-face "#333")
            (set-face-background 'ahs-face "#888")
            (global-auto-highlight-symbol-mode))
  :ensure t)

(leaf zoom-window
  :doc "Zoom window like tmux"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :added "2021-05-04"
  :url "https://github.com/syohex/emacs-zoom-window"
  :emacs>= 24.3
  :ensure t
  :bind (("C-w 1" . zoom-window-zoom))
  :custom ((zoom-window-mode-line-color . "#241"))
  :config
  (zoom-window-setup))

(leaf winner-mode
  :bind (("C-w w b" . winner-undo)
         ("C-w w f" . winner-redo)
         ("C-w w s" . window-configuration-to-register)
         ("C-w w l" . jump-to-register))
  :config (winner-mode 1))

(leaf string-inflection
  :doc "underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of names"
  :tag "elisp"
  :added "2021-05-12"
  :ensure t
  :bind (("M-u" . string-inflection-all-cycle))
  :hook
  (ruby-mode-hook . (lambda () (local-set-key (kbd "M-u") #'string-inflection-ruby-style-cycle))))

(leaf emr
  :doc "Emacs refactoring system."
  :req "s-1.3.1" "dash-1.2.0" "cl-lib-0.2" "popup-0.5.0" "emacs-24.1" "list-utils-0.3.0" "paredit-24.0.0" "projectile-0.9.1" "clang-format-0.0.1" "iedit-0.97"
  :tag "refactoring" "convenience" "tools" "emacs>=24.1"
  :url "https://github.com/Wilfred/emacs-refactor"
  :added "2021-08-23"
  :emacs>= 24.1
  :ensure t
  :after list-utils paredit projectile clang-format iedit)

(leaf wgrep
  :doc "Writable grep buffer and apply the changes to files"
  :tag "extensions" "edit" "grep"
  :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el"
  :added "2021-11-24"
  :ensure t
  :init (setq wgrep-enable-key (kbd "M-e")))

(leaf cycle-quotes
  :doc "Cycle between quote styles"
  :tag "convenience"
  :url "http://elpa.gnu.org/packages/cycle-quotes.html"
  :added "2022-03-16"
  :ensure t
  :bind (("M-\"" . cycle-quotes)))

(leaf restclient
  :doc "An interactive HTTP client for Emacs"
  :tag "http"
  :added "2022-04-09"
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major modes

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-27.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=27.1"
  :url "https://github.com/jrblevin/markdown-mode"
  :added "2024-11-19"
  :emacs>= 27.1
  :ensure t
  :config
  (define-key markdown-mode-map (kbd "C-c c") markdown-mode-command-map)
  (define-key markdown-mode-map (kbd "C-c C-c") nil))

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
           (web-mode-extra-auto-pairs                 . t)
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
  :bind (web-mode-map ("M-SPC" . web-mode-mark-and-expand)
                      ("C-q C-y" . my-web-mode-element-content-replace-by-yank)
                      ("C-M-u" . web-mode-element-parent)
                      ("C-M-d" . sp-down-sexp)
                      ("C-M-n" . web-mode-element-sibling-next)
                      ("C-M-p" . web-mode-element-sibling-previous)
                      ("C-M-e" . web-mode-element-end)
                      ("C-M-a" . web-mode-element-beginning)
                      ("C-M-h" . web-mode-element-content-select)
                      ("C-M-k" . web-mode-element-kill)
                      ("M-N"   . my-web-mode-forward-content-of-element)
                      ("M-P"   . my-web-mode-backward-content-of-element))
  :hook (web-mode-hook . (lambda () (sgml-electric-tag-pair-mode 1)))
  :config
  (defun my-web-mode-element-content-replace-by-yank ()
    (interactive)
    (web-mode-element-content-select)
    (delete-region (mark) (point))
    (yank-and-indent))
  (defun my-web-mode-forward-content-of-element ()
    (interactive)
    (web-mode-element-sibling-next)
    (sp-down-sexp))
  (defun my-web-mode-backward-content-of-element ()
    (interactive)
    (web-mode-element-sibling-previous)
    (sp-down-sexp)))

(leaf emmet-mode
  :doc "Unofficial Emmet's support for emacs"
  :tag "convenience"
  :added "2021-03-26"
  :url "https://github.com/smihica/emmet-mode"
  :ensure t
  :custom((emmet-preview-default        . t)
          (emmet-indentation            . 2)
          (emmet-self-closing-tag-style . ""))
  :hook ((web-mode-hook css-mode-hook) . (lambda ()
                                           (emmet-mode t)
                                           (local-set-key (kbd "C-M-i") #'emmet-expand-line))))

(leaf rbenv
  :doc "Emacs integration for rbenv"
  :tag "rbenv" "ruby"
  :url "https://github.com/senny/rbenv.el"
  :added "2021-08-21"
  :ensure t
  :hook (global-rbenv-mode-hook . (lambda ()
                                    (setf rbenv-modeline-function (lambda ()))
                                    (setq rbenv--modestring "")))
  :init (global-rbenv-mode))
(leaf ruby-mode
  :doc "Major mode for editing Ruby files"
  :tag "builtin"
  :added "2021-03-26"
  :custom ((ruby-insert-encoding-magic-comment . nil)
           (ruby-align-chained-calls           . t))
  :bind (("M-\"" . ruby-toggle-string-quotes))
  :config
  (leaf ruby-interpolation
    :doc "Ruby string interpolation helpers"
    :added "2021-03-30"
    :url "http://github.com/leoc/ruby-interpolation.el"
    :ensure t
    :hook ruby-mode-hook
    :blackout t)
  (leaf inf-ruby
    :doc "Run a Ruby process in a buffer"
    :req "emacs-24.3"
    :tag "ruby" "languages" "emacs>=24.3"
    :url "http://github.com/nonsequitur/inf-ruby"
    :added "2021-08-19"
    :emacs>= 24.3
    :ensure t)
  (leaf rubocop
    :doc "An Emacs interface for RuboCop"
    :req "emacs-24"
    :tag "convenience" "project" "emacs>=24"
    :added "2021-04-21"
    :url "https://github.com/rubocop/rubocop-emacs"
    :emacs>= 24
    :ensure t
    :hook (ruby-mode-hook . rubocop-mode)))

(leaf javascript-helpers
  :init
  (defun my-is-face-at-point (face)
    (memq face (text-properties-at (point))))
  (defun my-js-open-brace ()
    "`{' を入力されたときいい感じに展開する"
    (interactive)
    (let ((current-line (if (and (eolp) (bolp)) ; thing-at-pointを空行に適用すると前の行を取ってきて誤動作するので
                            ""                  ; 空行のときは強制的に空文字列にする
                          (thing-at-point 'line))))
      (if (or (my-is-face-at-point 'font-lock-string-face)
              (string-match-p "<.*>" current-line)
              (and (not (bolp))         ; 空文字列のときは font-lock-string-face が設定されないため
                   (not (eolp))         ; ここで判別する
                   (string-match-p "\\(\"\"\\|''\\|``\\)" (buffer-substring-no-properties (- (point) 1) (+ (point) 1))))
              (and (string-match-p "^\\(import\\|export\\) " current-line)
                   (not (string-match-p "\\(\\bfunction\\b\\|=>\\)" current-line)))
              ;; jsx属性値用 カーソル直前に=があったときは{}に展開
              (string-match-p "=\\s-*$" current-line))
          (progn (insert "{}")
                 (move-point-visually -1))
        (progn (insert "{\n\n}")
               (js-indent-line)
               (forward-line -1)
               (js-indent-line))))))

(leaf js2-mode
  :doc "Improved JavaScript editing mode"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "javascript" "languages" "emacs>=24.1"
  :added "2021-03-27"
  :url "https://github.com/mooz/js2-mode/"
  :emacs>= 24.1
  :ensure t
  :mode "\\.js$"
  :custom ((js-indent-level . 2))
  :bind (js2-mode-map ("{" . my-js-open-brace)))

(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :req "emacs-24.3"
  :tag "languages" "typescript" "emacs>=24.3"
  :added "2021-03-27"
  :url "http://github.com/ananthakumaran/typescript.el"
  :emacs>= 24.3
  :ensure t
  :custom ((typescript-indent-level . 2))
  :bind (typescript-mode-map ("{" . my-js-open-brace)))

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :added "2021-04-04"
  :emacs>= 24.1
  :ensure t)

(leaf elixir-mode
  :doc "Major mode for editing Elixir files"
  :req "emacs-25" "pkg-info-0.6"
  :tag "elixir" "languages" "emacs>=25"
  :added "2021-05-14"
  :url "https://github.com/elixir-editors/emacs-elixir"
  :emacs>= 25
  :ensure t)

(leaf howm
  :doc "Wiki-like note-taking tool"
  :req "cl-lib-0.5"
  :url "https://howm.osdn.jp"
  :added "2021-10-26"
  :ensure t
  :config
  (defun my-howm-save-and-kill-buffer ()
    (interactive)
    (howm-save-buffer)
    (kill-buffer nil))
  :custom ((howm-directory . "~/Documents/howm")
           (howm-view-summary-persistent . nil))
  :bind (("C-q , ," . howm-menu)
         ("C-q , s" . howm-list-grep)
         ("C-q , c" . howm-create)
         (howm-mode-map ("C-c C-c" . my-howm-save-and-kill-buffer))))

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24"
  :tag "docker" "emacs>=24"
  :url "https://github.com/spotify/dockerfile-mode"
  :added "2021-11-22"
  :emacs>= 24
  :ensure t)

(leaf php-mode
  :doc "Major mode for editing PHP code"
  :req "emacs-25.2"
  :tag "php" "languages" "emacs>=25.2"
  :url "https://github.com/emacs-php/php-mode"
  :added "2021-11-22"
  :emacs>= 25.2
  :ensure t
  :mode (("\\.inc\\'" . php-mode))
  :hook (php-mode-hook . php-align-setup)
  :bind (php-mode-map ("C-q C-m" . php-search-web-documentation)))

(leaf tsx-mode
 :doc "a batteries-included Emacs major mode for TSX/JSX files"
 :req "emacs-27"
 :el-get "orzechowskid/tsx-mode.el"
 :emacs>= 27
 :mode (("\\.jsx\\'" . tsx-mode)
        ("\\.tsx\\'" . tsx-mode))
 :init
 (progn
   (leaf tree-sitter
     :doc "Incremental parsing system"
     :req "emacs-25.1" "tsc-0.18.0"
     :tag "tree-sitter" "parsers" "tools" "languages" "emacs>=25.1"
     :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
     :added "2022-11-27"
     :emacs>= 25.1
     :ensure t
     :after tsc)
   (leaf tree-sitter-langs
     :doc "Grammar bundle for tree-sitter"
     :req "emacs-25.1" "tree-sitter-0.15.0"
     :tag "tree-sitter" "parsers" "tools" "languages" "emacs>=25.1"
     :url "https://github.com/emacs-tree-sitter/tree-sitter-langs"
     :added "2022-11-27"
     :emacs>= 25.1
     :ensure t
     :after tree-sitter)
   (leaf tsi-el
     :doc "tree-sitter indentation minor mode for Emacs"
     :el-get "orzechowskid/tsi.el"
     )
   (leaf coverlay
     :doc "Test coverage overlays"
     :req "emacs-24.1" "cl-lib-0.5"
     :tag "overlay" "coverage" "emacs>=24.1"
     :url "https://github.com/twada/coverlay.el"
     :added "2022-11-27"
     :emacs>= 24.1
     :ensure t)
   (leaf origami
     :doc "Flexible text folding"
     :req "s-1.9.0" "dash-2.5.0" "emacs-24" "cl-lib-0.5"
     :tag "folding" "emacs>=24"
     :url "https://github.com/gregsexton/origami.el"
     :added "2022-11-27"
     :emacs>= 24
     :ensure t)
   (leaf graphql-mode
     :doc "Major mode for editing GraphQL schemas"
     :req "emacs-24.3"
     :tag "languages" "emacs>=24.3"
     :url "https://github.com/davazp/graphql-mode"
     :added "2022-11-27"
     :emacs>= 24.3
     :ensure t))
 )

(leaf sql
  :init
  (push "/usr/local/opt/mysql-client@5.7/bin" exec-path))

(leaf csv-mode
  :ensure t)

(leaf emacs-lisp-mode
  :hook ((emacs-lisp-mode-hook . paredit-mode)))

(leaf paredit
  :after paredit
  :config
  (keymap-unset paredit-mode-map "M-s"))

(leaf osx-dictionary
  :doc "Interface for OSX Dictionary.app"
  :req "cl-lib-0.5"
  :tag "dictionary" "mac"
  :url "https://github.com/xuchunyang/osx-dictionary.el"
  :added "2024-10-22"
  :ensure t
  :bind (("M-s M-d" . osx-dictionary-search-word-at-point)
         ("M-s d"   . osx-dictionary-search-input)))

(leaf gptel
  :doc "Interact with ChatGPT or other LLMs"
  :req "emacs-27.1" "transient-0.4.0" "compat-29.1.4.1"
  :tag "convenience" "emacs>=27.1"
  :url "https://github.com/karthink/gptel"
  :added "2024-10-27"
  :emacs>= 27.1
  :ensure t
  :after compat
  :custom ((gptel-model . 'gpt-4o-mini))
  :bind (("C-w g f" . my-gptel-suggest-filename)
         ("C-w g g" . gptel)
         ("C-w g c" . gptel-send)
         ("C-w g m" . gptel-menu)
         ("C-w g a" . gptel-add)
         (gptel-mode-map
          ("C-c C-c" . gptel-send)))
  :config
  (defun my-gptel-after-reply-hook ()
    (run-at-time "0.1 sec" nil (lambda ()
                                 (goto-char (point-max)))))
  ;; (add-hook 'gptel-post-stream-hook 'my-gptel-after-reply-hook)

  (defun my-gptel-suggest-filename (user-input)
    "英語のファイル名の候補をいくつか挙げる"
    (interactive "sファイル名のトピックを入力してください: ")
    (let ((formatted-input (format "「%s」について記述した文書のファイル名をいくつか考えてください。ファイル名は英語のkebab-caseでお願いします。応答のタイトルには鍵括弧内をそのまま含めたものを出してください" user-input)))
      (gptel-request formatted-input
        :callback (lambda (response &optional error)
                    (let ((output-buffer (get-buffer-create "*GPTel Suggestion*")))
                      (with-current-buffer output-buffer
                        (erase-buffer)
                        (insert response)
                        (display-buffer output-buffer))))))))

(leaf eglot
  :hook
  (ruby-mode . eglot-ensure)
  (js2-mode  . eglot-ensure))

(leaf ellama
  :doc "Tool for interacting with LLMs"
  :req "emacs-28.1" "llm-0.6.0" "spinner-1.7.4" "transient-0.7" "compat-29.1"
  :tag "tools" "local" "help" "emacs>=28.1"
  :url "https://github.com/s-kostyaev/ellama"
  :added "2024-12-29"
  :emacs>= 28.1
  :ensure t
  :after llm spinner compat
  :init (require 'llm-ollama)
  :config
  (setopt ellama-language "日本語"))

;;;;;;;;;;;;;;;;
(defun key-valid-p (key)
  t)
;;;;;;;;;;;;;;;;

(leaf load-custom-file
  :init (load-library custom-file))

(leaf find-file-init-el
  :init (find-file "~/.emacs.d/init.el"))

(provide 'init)
;;; init.el ends here
