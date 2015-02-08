(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;; カラーテーマ
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;;; 行数表示
(global-linum-mode t)
(setq linum-format "%4d| ")

;;; 対応する括弧をハイライト
(show-paren-mode t)

;;; インデントルール
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default show-trailing-whitespace t) ; 行末の空白をハイライト
;;(add-hook 'font-lock-mode-hook            ; タブをハイライト
;;          (lambda ()
;;            (font-lock-add-keywords
;;             nil
;;             '(("\t" 0 'trailing-whitespace prepend)))))

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

