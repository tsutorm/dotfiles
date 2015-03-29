;; Emacs package system
(require 'cask "~/.cask/cask.el" t)
(cask-initialize)

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
(setq linum-format "%4d|")

;; カーソル
(setq default-frame-alist
      (append (list
               '(cursor-type . box)
               '(cursor-height . 4)
               )
              default-frame-alist))
(when window-system (blink-cursor-mode -1))

;; 対応するカッコを強調表示
(show-paren-mode t)
(setq show-paren-style 'mixed) ; `parenthesis' or `expression' or `mixed'

;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;;; 上部のメニューはあるだけ邪魔なので消す
(tool-bar-mode -1)
(menu-bar-mode -1)

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

;; php-mode
(load-library "php-mode")
(require 'php-mode)
(add-hook 'php-mode-hook (lambda ()
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (defun ywb-php-lineup-arglist-intro (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) c-basic-offset))))
    (defun ywb-php-lineup-arglist-close (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (current-column))))
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)



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
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'ruby-mode-hook 'helm-gtags-mode)



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
