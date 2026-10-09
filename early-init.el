;;; native compileの環境設定
(when (eq system-type 'darwin)
  (let* ((homebrew-prefix "/opt/homebrew")
         (homebrew-bin (concat homebrew-prefix "/bin"))
         ;; インストールされているGCCのバージョン（14や15など）のディレクトリを自動検知
         (gcc-lib-base (concat homebrew-prefix "/lib/gcc"))
         (gcc-version-dir (when (file-directory-p gcc-lib-base)
                            (car (directory-files gcc-lib-base t "^[0-9]+$")))))

    (when (file-directory-p homebrew-bin)
      ;; 1. コマンドの実行パスを通す
      (setenv "PATH" (concat homebrew-bin ":" (getenv "PATH")))
      (setq exec-path (cons homebrew-bin exec-path))

      ;; 2. コンパイラ・リンカが emutls_w 等を見つけられるように環境変数を強固に固定
      (when gcc-version-dir
        (let ((paths (list gcc-version-dir
                           (concat homebrew-prefix "/opt/gcc/lib/gcc/current")
                           (concat homebrew-prefix "/opt/libgccjit/lib/gcc/current"))))
          (dolist (path paths)
            (when (file-directory-p path)
              (setenv "LIBRARY_PATH" (concat path ":" (getenv "LIBRARY_PATH")))
              (setenv "LD_RUN_PATH" (concat path ":" (getenv "LD_RUN_PATH"))))))))))
