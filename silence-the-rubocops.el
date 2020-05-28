(eval-when-compile (require 'subr-x))

(defun my-is-current-line-empty? ()
  "カレント行が空行であるか判別する"
  (= (point-at-eol) (point-at-bol)))

(defun my-join-path (list)
  "リストを \"/\" でつなげる。連続した \"/\" は1文字に縮める"
  (replace-regexp-in-string "//+" "/"
                            (string-join list "/")))

(defun my-find-file-to-root (dir thing)
  "ディレクトリを遡ってファイル名thingを探す。見つけたらパスを、なければnilを返す
cf. http://syohex.hatenablog.com/entry/20121008/1349681293"
  (let ((path (my-join-path `(,dir ,thing))))
    (if (file-exists-p path)
        path
      (if (string= dir "/")
          nil
        (my-find-file-to-root (file-name-directory (directory-file-name dir)) thing)))))

(defun my-flycheck-error-messages-in-current-line ()
  "カレント行にあるflycheckのエラーメッセージをリストで返す"
  (mapcar #'flycheck-error-message
          (flycheck-overlay-errors-in (point-at-bol) (point-at-eol))))

(defun my-flycheck-rubocops-in-current-line ()
  "カレント行にあるrubocopのcopをリストで返す"
  (mapcar (lambda (m)
            (replace-regexp-in-string ":.*" "" m))
          (my-flycheck-error-messages-in-current-line)))


(defun my-silence-the-rubocops-cops ()
  "表示されてるrubocopのエラーを抑止する"
  (interactive)
  (if (and (string= "ruby-mode" major-mode)
           flycheck-mode)
      (let ((cops (my-flycheck-rubocops-in-current-line))
            (rubocop-yml (my-find-file-to-root default-directory ".rubocop.yml")))
        (when (and cops rubocop-yml)
          (with-current-buffer (find-file-noselect rubocop-yml)
            (goto-char (point-max))
            (if (not (my-is-current-line-empty?))
                (insert "\n"))
            (mapc (lambda (cop)
                    (insert (format "%s:\n  Enabled: false\n" cop)))
                  cops)
            (save-buffer))
          (flycheck-mode 0)
          (flycheck-mode 1)))))
