;; Font Configuration -------------------------------------------------------
(defvar efs/frame-transparency '(90 . 90))

;; mela package repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; solve mac ls option problems
 (setq insert-directory-program "gls" dired-use-ls-dired t)
 (setq dired-listing-switches "-al --group-directories-first")

;; basic ui setup
(setq inhibit-startup-message t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(when window-system
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 91 80))

;; ref: https://dr-knz.net/a-tour-of-emacs-as-go-editor.html
(require 'swiper)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)

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


(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/sort-tab")
(require 'sort-tab)
(sort-tab-mode 1)

(global-set-key (kbd "s-1") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-2") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-3") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-4") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-5") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-6") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-7") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-8") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-9") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-0") 'sort-tab-select-visible-tab)
(global-set-key (kbd "s-Q") 'sort-tab-close-all-tabs)
(global-set-key (kbd "s-q") 'sort-tab-close-mode-tabs)
(global-set-key (kbd "C-;") 'sort-tab-close-current-tab)


;; dont know right load-path
;; (add-to-list `load-path (expand-file-name "~/elisp"))
;; (require 'awesome-tray)
;; (awesome-tray-mode 1)


(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/thing-edit")
(require 'thing-edit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(package-selected-packages
   '(ace-window sis flycheck projectile tide company-tabnine obsidian zenburn-theme enh-ruby-mode go-mode quelpa corfu-terminal corfu typescript-mode doom-modeline doom-themes ivy-rich counsel evil-collection yasnippet yaml-mode vertico use-package swiper no-littering exec-path-from-shell evil)))


(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/lsp-bridge")

(require 'yasnippet)
(yas-global-mode 1)

;; lsp-bridge configuration
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq acm-enable-tabnine-helper 1)
(setq lsp-bridge-enable-hover-diagnostic t)

(bind-key (kbd "C-c d") 'lsp-bridge-find-def)
(bind-key (kbd "C-c i") 'lsp-bridge-find-impl)


;; setup for coding
;;; typescript mode
(use-package typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;;; ruby
(use-package ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

;;; python
(use-package python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; obsidian setup
(require 'obsidian)
(obsidian-specify-path "/Users/han-ko/Library/Mobile Documents/iCloud~md~obsidian/Documents")
;; If you want a different directory of `obsidian-capture':
(setq obsidian-inbox-directory "record")

;; Replace standard command with Obsidian.el's in obsidian vault:
(bind-key (kbd "C-c o") 'obsidian-follow-link-at-point 'obsidian-mode-map)
(bind-key (kbd "C-c l") 'obsidian-insert-link 'obsidian-mode-map)
(bind-key (kbd "C-c n") 'obsidian-capture 'obsidian-mode-map)
(bind-key (kbd "C-c n") 'obsidian-capture)
(bind-key (kbd "C-c j") 'obsidian-jump)
(bind-key (kbd "C-c j") 'obsidian-jump 'obsidian-mode-map)


;; Use either `obsidian-insert-wikilink' or `obsidian-insert-link':
;; Activate detectino of Obsidian vault
(global-obsidian-mode t)

(use-package go-mode)
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook
          (lambda ()
	    (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(add-hook 'go-mode-hook #'tree-sitter-mode)
(add-hook 'go-mode-hook #'tree-sitter-hl-mode)

;; move between window
(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

(use-package sis
  ;; :hook
  ;; enable the /context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  ;; For MacOS
  (sis-ism-lazyman-config
   "com.apple.keylayout.ABC"
   "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese")

  (defun w/sis--guess-context-by-prev-chars (backward-chars forward-chars)
  "Detect the context based on the 2 chars before the point.

It has a side effect of deleting the previous whitespace if
there is a whitespace/newline and a comma before the point."
  (when (and (>= (point) 3)
             sis-context-mode
             (memq major-mode '(org-mode)))
    (let ((prev (preceding-char))
          (pprev (char-before (1- (point)))))
      (cond
       ((and (or (char-equal ?  pprev) (char-equal 10 pprev)) ; a whitespace or newline
             (char-equal ?, prev))
        (delete-char -1)                ; side effect: delete the second whitespace
        'other)
       ((string-match-p "[[:ascii:]]" (char-to-string (preceding-char)))
        'english)
       (t 'other)))))
(setq sis-context-detectors '(w/sis--guess-context-by-prev-chars))

(setq sis-context-hooks '(post-command-hook)) ; may hurt performance

(sis-global-respect-mode t)
(sis-global-context-mode t))

(bind-key (kbd "C-x j") 'sis-set-other)

(use-package projectile
  :ensure t
  :diminish
  :custom
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-mode +1)
  (when (executable-find "ghq")
    (setq projectile-known-projects
          (mapcar
           (lambda (x) (abbreviate-file-name x))
           (split-string (shell-command-to-string "ghq list --full-path")))))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package nyan-mode
  :if (display-graphic-p)
  :init
  (setq nyan-animate-nyancat nil)
  (nyan-mode t))

(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config
  (setq anzu-replace-to-string-separator (if (char-displayable-p ?→) " → " " -> ")
        ;; let spaceline handle auzu info in modeline
        anzu-cons-mode-line-p nil))

(use-package powerline
  :defer t
  :config
  (setq powerline-height 23
        powerline-default-separator (if (display-graphic-p) 'arrow 'utf-8)))

(use-package spaceline
  :defer t
  :init
  (add-hook 'after-init-hook (lambda () (require 'spaceline)))
  :config
  ;; To get the mode-line highlight to change color depending on the evil state
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (use-package spaceline-segments
    :ensure nil
    :config
    (setq spaceline-window-numbers-unicode t
          spaceline-minor-modes-separator ""
          spaceline-workspace-numbers-unicode t)
    ;;define version control segment
    (spaceline-define-segment version-control
                              "Version control information."
                              (when vc-mode
                                (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
                                  (powerline-raw (concat (kevin/maybe-alltheicon "git" :face 'warning :v-adjust -0.05)
                                                         " "
                                                         branch)))))
    ;;define major mode segment
    (spaceline-define-segment major-mode
                              "Return simplifyed major mode name."
                              (let* ((major-name (format-mode-line "%m"))
                                     (replace-table '(
                                                      Emacs-Lisp "Elisp"
                                                      Shell-script ""
                                                      Python ""
                                                      Go ""
                                                      C++//l "C++"
                                                      Protocol-Buffers//l "PB"
                                                      Fundamental ""))
                                     (replace-name (plist-get replace-table (intern major-name))))
                                (if replace-name
                                    replace-name major-name)))
    ;; define buffer id segment
    (spaceline-define-segment buffer-id
                              "Shorten buufer fileanme."
                              (when (buffer-file-name)
                                (concat
                                 (kevin/maybe-faicon-icon "floppy-o" :face 'warning :v-adjust -0.05)
                                 " "
                                 (shorten-directory default-directory 6)
                                 (file-relative-name buffer-file-name)))))

  (use-package spaceline-config
    :ensure nil
    :config
    (spaceline-toggle-persp-name-on)
    (spaceline-toggle-workspace-number-on)
    (spaceline-toggle-window-number-on)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-remote-host-off)
    (spaceline-toggle-flycheck-info-off)
    (spaceline-toggle-selection-info-on)
    (spaceline-toggle-input-method-on)
    (spaceline-toggle-buffer-encoding-abbrev-on)
    (spaceline-toggle-nyan-cat-on)
    ;; hide the current position in the buffer as a percentage
    (spaceline-toggle-buffer-position-off)
    ;; shows the currently visible part of the buffer.
    (spaceline-toggle-hud-off)
    (spaceline-toggle-major-mode-on)
    ;; configure the separator between the minor modes
    (unless (display-graphic-p)
      (spaceline-toggle-minor-modes-off))
    ;; custom spaceline theme
    (spaceline-compile
     ;; define spaceline theme name: spaceline-ml-custom
     "custom"
     ;; left side
     '(((((persp-name :fallback workspace-number) window-number) :separator "")
        :fallback evil-state
        :face highlight-face
        :priority 100)
       (anzu :priority 95)
       ((buffer-id) :priority 98)
       (process :when active)
       ((flycheck-error flycheck-warning flycheck-info) :when active :priority 99)
       (version-control :when active :priority 97)
       (org-pomodoro :when active)
       (org-clock :when active)
       (nyan-cat :when active :priority 70)
       (major-mode :when active :priority 79)
       (minor-modes :when active :priority 78))
     ;; right side
     '((purpose :priority 94)
       (selection-info :priority 95)
       input-method
       ((buffer-encoding-abbrev line-column) :separator "|" :priority 96)
       (global :when active)
       (hud :priority 99)))

    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-custom))))))

(provide 'init-modeline)
;;; init-modeline ends here
