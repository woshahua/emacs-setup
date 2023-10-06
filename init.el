;; use shell setup
;; frame
(defvar efs/frame-transparency '(90 . 90))
(set-face-attribute 'default nil :font "Source Code Pro-14")

;; mela package repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (setq exec-path-from-shell-arguments '("-l"))
(setq exec-path-from-shell-shell-name "/bin/zsh")
(setq exec-path-from-shell-debug t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(load-theme 'atom-one-dark t)

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
(setq insert-directory-program "/opt/homebrew/bin/gls" dired-use-ls-dired t)
;; (setq insert-directory-program "gls" dired-use-ls-dired t)
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
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; setup kotlin

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
 '(acm-backend-codeium-api-key "c7d24326-3dd7-41c8-9818-9400bf71304d")
 '(custom-safe-themes
   '("bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(elfeed-feeds
   '("https://www.fatbobman.com/feed.rss" "https://onevcat.com/feed.xml" "https://catcoding.me/atom.xml" "https://feeds.feedburner.com/lzyy" "https://raw.githubusercontent.com/RSS-Renaissance/awesome-blogCN-feeds/master/feedlist.opml" "https://oleb.net/blog/atom.xml" "https://www.appcoda.com/navigationstack/" "feed://developer.apple.com/news/rss/news.rss" "https://developer.apple.com/news/" "https://pofat.substack.com/" "https://www.avanderlee.com/" "https://sarunw.com/" "https://github.com/SwiftOldDriver/iOS-Weekly/releases.atom"))
 '(package-selected-packages
   '(ace-window sis flycheck projectile tide company-tabnine obsidian zenburn-theme enh-ruby-mode go-mode quelpa corfu-terminal corfu typescript-mode doom-modeline doom-themes ivy-rich counsel evil-collection yasnippet yaml-mode vertico use-package swiper no-littering exec-path-from-shell evil)))

;; setup for coding
;;; typescript mode
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
(bind-key (kbd "C-c m") 'obsidian-capture 'obsidian-mode-map)
(bind-key (kbd "C-c m") 'obsidian-capture)
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
  ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("w" . projectile-ripgrep)))


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

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))

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


;; elfeed
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (bind-key "r" #'elfeed-update elfeed-search-mode-map)
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
;; get code link on github
(global-set-key (kbd "C-c g l") 'git-link)

(require 'yasnippet)
(yas-global-mode 1)

;; temporary disable lsp-bridge
;; lsp-bridge configuration
(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/lsp-bridge")

;; org-roam mode
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/Users/han-ko/Library/Mobile Documents/com~apple~CloudDocs/org-roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))


;; copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;;; enable copilot on programming mode
(add-hook 'prog-mode-hook 'copilot-mode)

;;; bind completion key
(define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion)

(require 'org)
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)
(setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
(org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets

;; org-mode 换行问题
(add-hook 'org-mode-hook 'visual-line-mode)

;; org-ai cache problem

;; pangu-spacing
;;; https://github.com/coldnew/pangu-spacing
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t)


(setq org-startup-folded t)
(defun my-org-mode-auto-fold ()
  (when (and (buffer-file-name)
             (and (eq (file-name-extension (buffer-file-name)) "org")
                  (not (file-exists-p (buffer-file-name)))))
    (org-cycle)))

(add-hook 'org-mode-hook 'my-org-mode-auto-fold)

;; ob-swiftui
;;; https://github.com/xenodium/ob-swiftui
(require 'ob-swiftui)
(ob-swiftui-setup)

;; comment-region
;;; TODO: comment line is better usage
(global-set-key (kbd "C-M-c") 'comment-region)
(global-set-key (kbd "C-M-u") 'uncomment-region)

;; nerd-icons
(require 'nerd-icons)
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

(use-package typescript-mode
  :mode (("\.ts\'" . typescript-mode)
	 ("\.tsx\'" . typescriptreact-mode)))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas-global-mode)
         ("C-c y m" . yas-minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand)
         :map yas-keymap
         ("C-i" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

;; tree-sitter typescript
(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; setup typescript
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; autoformat
(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma"  "es5"
              "--bracket-spacing" "true"
              "--single-quote"    "true"
              "--semi"            "true"
              "--print-width"     "100"
	      "--tabWidth"          "4"
              file))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (apheleia-global-mode t))


;; setup eglot related thing
(use-package lsp-bridge
  :custom
  (lsp-bridge-signature-function 'eldoc-message)
  (lsp-bridge-multi-lang-server-extension-list
    '((("ts" "tsx") . "typescript_eslint"))))

(setq acm-enable-tabnine-helper 1)
(setq lsp-bridge-enable-hover-diagnostic t)

(bind-key (kbd "C-c d") 'lsp-bridge-find-def)
(bind-key (kbd "C-c i") 'lsp-bridge-find-impl)
(bind-key (kbd "C-c r") 'lsp-bridge-rename)

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
	    (add-hook 'typescript-mode-hook #'lsp-bridge-mode)
	    (add-hook 'typescriptreact-mode #'lsp-bridge-mode)
	    (add-hook 'elisp-mode-hook #'lsp-bridge-mode)

            (unless (display-graphic-p)
              (with-eval-after-load 'acm
                (require 'acm-terminal)))))

;; setup robe for rails / ruby
(use-package ruby-ts-mode :mode (("\\.rb\\'" . ruby-ts-mode)))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-ts-mode-hook 'robe-mode)

(eval-after-load 'company
  '(push 'company-robe company-backends))

;; setup rspec mode
(require 'rspec-mode)
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))

;; rime
(use-package rime
  :custom
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (default-input-method "rime"))

(setq rime-translate-keybindings
  '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))

(setq rime-user-data-dir "~/rime-config/")

(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p))

(setq rime-posframe-properties (list :internal-border-width 1
                                          :font "Source Code Pro-14"))

(setq word-wrap-by-category t)
(setq default-input-method "rime"
      rime-show-candidate 'posframe)


;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)


;; pyqterminal test later maybe
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-pyqterminal)


;; sort-tab
(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/sort-tab") ; add sort-tab to your load-path
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

;; forge setup
(with-eval-after-load 'magit
  (require 'forge))
