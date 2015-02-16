;; Emacs package system
(require 'cask "~/.cask/cask.el" t)
(cask-initialize)


;;; 言語設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;; site-lisp auto-load
;(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
;  (add-to-list 'load-path default-directory)
;  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;      (normal-top-level-add-subdirs-to-load-path)))

;;; 行数表示
(global-linum-mode t)
(setq linum-format "%4d| ")

;;; 対応する括弧をハイライト
(show-paren-mode t)

;; インデントルール
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default show-trailing-whitespace t) ; 行末の空白をハイライト

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)


;; theme
(load-theme 'tango-dark t)
;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Coding
;; flycheck & flycheck-cask
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode 1)


;;; helm
(require 'helm)
(require 'helm-config)
(require 'helm-gtags)
;; 標準を置き換え
(helm-mode +1)
;;(global-set-key (kbd "C-x b") 'helm-for-files)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; helmショートカット
(global-set-key (kbd "C-x C-h h") 'helm-mini)
(global-set-key (kbd "C-x C-h r") 'helm-recentf)
(global-set-key (kbd "C-x C-h i") 'helm-imenu)
(global-set-key (kbd "C-x C-h k") 'helm-show-kill-ring)
;; helmコマンド内のキーバインド
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; helm-gtags-mode
(add-hook 'helm-gtags-mode-hook
'(lambda ()
   ;;入力されたタグの定義元へジャンプ
   (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
   ;;入力タグを参照する場所へジャンプ
   (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
   ;;入力したシンボルを参照する場所へジャンプ
   (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
   ;;タグ一覧からタグを選択し, その定義元にジャンプする
   (local-set-key (kbd "M-l") 'helm-gtags-select)
   ;;ジャンプ前の場所に戻る
   (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
(add-hook 'php-mode-hook 'helm-gtags-mode)
;(add-hook 'ruby-mode-hook 'helm-gtags-mode)



;;; prog-modes
;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; web-mode
(require 'web-mode)
; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)(require 'web-mode)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
