(require 'package)
;;;;; add melpa and orgmode for packages
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
;;;;; ensure to use use-package
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 言語環境、日本語入力など
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;; バックアップ・自動保存
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; フレーム、ウィンドウまわりのいろいろ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; モードラインの行番号・桁番号
(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)
(require 'linum)
(setq linum-format "%4d|")

;; カーソル
(setq default-frame-alist
      (append (list
               '(cursor-type . box)
               '(cursor-height . 4)
               )
              default-frame-alist))
(when window-system (blink-cursor-mode -1))

;; 対応するカッコを強調表示はrainbow-delimiter
;(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; 透明度を変更するコマンド M-x set-alpha
;;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
;(defun set-alpha (alpha-num)
;  "set frame parameter 'alpha"
;  (interactive "nAlpha: ")
;  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;; 上部のメニューはあるだけ邪魔なので消す
(tool-bar-mode -1)
(menu-bar-mode -1)

;; テーマ
(load-theme 'tango-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 開発言語関係いろいろ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; インデント関係 2幅でスペースがでふぉ
(setq tab-width 2)
(setq indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq js-indent-level 2)

;; インデントルール
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default show-trailing-whitespace t) ; 行末の空白をハイライト


;;; helm
(require 'helm)
;(require 'helm-config)
(require 'helm-gtags)
;; 標準を置き換え
(helm-mode +1)
(global-set-key (kbd "C-x b") 'helm-for-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
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
(add-hook 'ruby-mode-hook 'helm-gtags-mode)

;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)
(autoload 'rspec-mode "rspec-mode")
(add-hook 'ruby-mode-hook 'rspec-mode)
(autoload 'flycheck-mode "flycheck")
(add-hook 'ruby-mode-hook 'flycheck-mode)
(setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))
(projectile-rails-global-mode)

;; rust-mode
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
;;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook (lambda ()
                            (flycheck-rust-setup)))

;; elixir-mode and alchemist
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

;; k8s-mode
(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))
