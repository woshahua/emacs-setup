;; frame
(defvar efs/frame-transparency '(90 . 90))
(set-face-attribute 'default nil :font "Source Code Pro-14")

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

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

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

;; treesit config
(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

;; ref: https://dr-knz.net/a-tour-of-emacs-as-go-editor.html
(require 'swiper)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; delete end of line space
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; auto refresh buffer
(global-auto-revert-mode t)


;; setup evil
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

(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/thing-edit")
(require 'thing-edit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(elfeed-feeds
   '("https://www.fatbobman.com/feed.rss" "https://onevcat.com/feed.xml" "https://catcoding.me/atom.xml" "https://feeds.feedburner.com/lzyy" "https://raw.githubusercontent.com/RSS-Renaissance/awesome-blogCN-feeds/master/feedlist.opml" "https://oleb.net/blog/atom.xml" "https://www.appcoda.com/navigationstack/" "feed://developer.apple.com/news/rss/news.rss" "https://developer.apple.com/news/" "https://pofat.substack.com/" "https://www.avanderlee.com/" "https://sarunw.com/" "https://github.com/SwiftOldDriver/iOS-Weekly/releases.atom"))
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

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(unless (display-graphic-p)
  (straight-use-package
   '(popon :host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
  (straight-use-package
   '(acm-terminal :host github :repo "twlz0ne/acm-terminal")))

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'yasnippet)
            (yas-global-mode 1)

            (require 'lsp-bridge)
            (global-lsp-bridge-mode)

            (unless (display-graphic-p)
              (with-eval-after-load 'acm
                (require 'acm-terminal)))))

;; setup for coding
;;; typescript mode
(use-package typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
(setq typescript-indent-level 2)

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

(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(setq yaml-indent-offset 2)

;; move between window
(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

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

(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/awesome-pair") ; add awesome-pair to your load-path
(require 'awesome-pair)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))
  (add-hook hook '(lambda () (awesome-pair-mode 1))))

(define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
(define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
(define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
(define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

(define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

(define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)
(define-key awesome-pair-mode-map (kbd "RET") 'awesome-pair-newline)


(require 'company)
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backands")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
	  (and (listp backend)
	       (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yasnippet))))

(setq company-backends
      (mapcar #'company-mode/backend-with-yas  company-backends))

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace nil)  ; automatically delete spaces at the end of the line when saving

;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "gpg"
      (file-name-extension (buffer-name)) t))))

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))


;; system style
(load-theme 'spacemacs-dark t)

;; setup for orgmode operation
;;; setup for orgmode style
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Minimal UI
(require 'org-modern)

;; Choose some fonts
;;(set-face-attribute 'default nil :family "Iosevka")
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

;;; org-capture
(require 'org)
(global-set-key (kbd "C-c c") 'org-capture)
;;;; set files for org-agenda-files
(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
			 "~/gtd/done.org"
                         "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/done.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))


;; ;; vterm
;; (use-package vterm
;;     :ensure t)

;; elfeed
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed ))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

;; sis
(require 'sis)
(sis-ism-lazyman-config
 "com.apple.keylayout.ABC"
 "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese")
;; enable the /cursor color/ mode
(sis-global-cursor-color-mode t)
;; enable the /respect/ mode
(sis-global-respect-mode t)
;; enable the /context/ mode for all buffers
(sis-global-context-mode t)
;; enable the /inline english/ mode for all buffers
(sis-global-inline-mode t)

;; get code link on github
(global-set-key (kbd "C-c g l") 'git-link)


;; chatgpt
(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/mind-wave")
(require 'mind-wave)
