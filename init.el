
;; Font Configuration -------------------------------------------------------
(defvar efs/default-font-size 150)
(defvar efs/default-variable-font-size 150)
(defvar efs/frame-transparency '(90 . 90))

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; mela package repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; keep folders clean
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; solve mac ls option problems
 (setq insert-directory-program "gls" dired-use-ls-dired t)
 (setq dired-listing-switches "-al --group-directories-first")

;; basic ui setup
(setq inhibit-startup-message t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 10)        ; Give some breathing room00D

;; (menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; whiteline mode
(whitespace-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;;Org mode configuration
;; Org Mode Configuration ------------------------------------------------------

;; ref: https://dr-knz.net/a-tour-of-emacs-as-go-editor.html
(require 'swiper)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)


;; (add-mode-hook 'go-mode-hook (lambda ()
;;     (setq tab-width 4)))

(require 'yasnippet)
(yas-global-mode 1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; delete end of line space
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; auto refresh buffer
(global-auto-revert-mode t)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


;; evil collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; dired setup
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-al --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
			      "h" 'dired-up-directory
			      "l" 'dired-find-file))
(use-package vertico
  :ensure t
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit)
	      )

  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; theme & modeline
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; ivy completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
(counsel-mode 1))


;; magit configure
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

;; sql
(eval-after-load "sql"
  '(load-library "sql-indent"))

(defun sql-mode-hooks()
  (setq sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (sql-set-product "postgres"))

(add-hook 'sql-mode-hook 'sql-mode-hooks)

;; disable menu-bar
(menu-bar-mode -1)

;; pane move
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(quelpa corfu-terminal corfu typescript-mode doom-modeline doom-themes ivy-rich counsel evil-collection yasnippet yaml-mode vertico use-package swiper no-littering exec-path-from-shell evil)))


(add-to-list 'load-path "/Users/ko-han/Repos/github.com/manateelazycat/lsp-bridge")
(require 'yasnippet)
(yas-global-mode 1)

(quelpa '(corfu-terminal
          :fetcher git
          :url "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(quelpa '(popon :fetcher git
                :url "https://codeberg.org/akib/emacs-popon.git"))

(require 'orderless)

(require 'corfu)
(global-corfu-mode)

(require 'corfu-terminal)
(setq corfu-terminal-disable-on-gui nil)
(corfu-terminal-mode)

(require 'lsp-bridge)
(global-lsp-bridge-mode)

;; setup web-mode
(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

  (setq indent-tabs-mode nil)
  (setq tab-width 2)
)


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
