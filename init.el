;; fix python-mode bug
(setq elpy-shell-echo-output nil)

;; use shell setup
;; frame
(defvar efs/frame-transparency '(90 . 90))
(set-face-attribute 'default nil :font "Source Code Pro-14")

;; mela package repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; read zsh config
;; (setq exec-path-from-shell-arguments '("-l"))
(setq exec-path-from-shell-shell-name "/bin/zsh")
(setq exec-path-from-shell-debug t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

;; search using swiper
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
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "cd51c4e5a258ab9fd82de1fb4c0eee54326de5e307e3bf2895467ae103bc562b" "65c29375a6215a070bbc41af0a2dd4d3b2bf462b32e95d7fe4d8c86052ab0b9d" "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(elfeed-feeds
   '("https://www.fatbobman.com/feed.rss" "https://onevcat.com/feed.xml" "https://catcoding.me/atom.xml" "https://feeds.feedburner.com/lzyy" "https://raw.githubusercontent.com/RSS-Renaissance/awesome-blogCN-feeds/master/feedlist.opml" "https://oleb.net/blog/atom.xml" "https://www.appcoda.com/navigationstack/" "feed://developer.apple.com/news/rss/news.rss" "https://developer.apple.com/news/" "https://pofat.substack.com/" "https://www.avanderlee.com/" "https://sarunw.com/" "https://github.com/SwiftOldDriver/iOS-Weekly/releases.atom"))
 '(org-agenda-files
   '("~/Library/Mobile Documents/com~apple~CloudDocs/org-roam/20231219131733-task_yokoyan.org" "/Users/han-ko/gtd/inbox.org" "/Users/han-ko/gtd/gtd.org" "/Users/han-ko/gtd/done.org" "/Users/han-ko/gtd/tickler.org"))
 '(package-selected-packages
   '(ace-window sis flycheck projectile tide company-tabnine obsidian zenburn-theme enh-ruby-mode go-mode quelpa corfu-terminal corfu typescript-mode doom-modeline doom-themes ivy-rich counsel evil-collection yasnippet yaml-mode vertico use-package swiper no-littering exec-path-from-shell evil)))

;; setup for coding
;;; python
(use-package python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

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

;; awesome-pair 括号补全
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

;; yasnippet configuration
(require 'yasnippet)
(yas-global-mode 1)

;; lsp-bridge configuration
(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/lsp-bridge")

;; org-roam configuration
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

;; org-ai setup
(add-to-list 'load-path "/Users/han-ko/ghq/github.com/rksm/org-ai")
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)
(setq org-ai-default-chat-model "gpt-4o") ; if you are on the gpt-4 beta:
(org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets

;; org-mode 换行问题
(add-hook 'org-mode-hook 'visual-line-mode)

;; pangu-spacing
;;; https://github.com/coldnew/pangu-spacing
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t)

;; nerd-icons
(require 'nerd-icons)
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; yasnippet
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
  )

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


(setq doom-modeline-support-imenu t)
;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-project-detection 'auto)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)

;; auto-save
(add-to-list 'load-path "/Users/han-ko/ghq/github.com/manateelazycat/auto-save") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)   ; quietly save (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "gpg"
      (file-name-extension (buffer-name)) t))))

(defun nerd-icon-for-tags (tags)
  "Generate Nerd Font icon based on tags.
  Returns default if no match."
  (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
        ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
        ((member "emacs" tags) (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
        ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
        (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))


(defun dss/-org-ai-after-chat-insertion-hook (type _text)
  (when (and (eq type 'end) (eq major-mode 'org-mode) (memq 'org-indent-mode minor-mode-list))
    (org-indent-indent-buffer)))
(add-hook 'org-ai-after-chat-insertion-hook #'dss/-org-ai-after-chat-insertion-hook)

(org-ql-search (org-agenda-files)
  '(ts :from -7 :to today)
  :title "Recent Items"
  :sort '(todo priority date)
  :super-groups '((:auto-ts t)))

;; complete the block
(bind-key (kbd "C-c a c") 'org-ai-complete-block)

;; setup auth-sources
(setq auth-sources
    '((:source "~/authinfo.gpg")))

;; org-mode style
(add-to-list 'load-path "/Users/han-ko/ghq/github.com/tonyaldon/org-bars")
(require 'org-bars)
(add-hook 'org-mode-hook 'org-bars-mode)

(setq org-startup-indented t)    ;; 見出しをインデント
(setq org-indent-mode-turns-on-hiding-stars nil)  ;; 見出しをインデントした時にアスタリスクが減るのを防ぐ
(setq org-indent-indentation-per-level 4)  ;; インデントの幅を設定
(setq org-startup-folded 'content)  ;; 見出しの初期状態（見出しだけ表示）
(setq org-stratup-with-inline-images t)  ;; インライン画像を表示

;; org-download
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; setup python interpreter
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)


;; setup ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; comment tags
;; (autoload 'comment-tags-mode "comment-tags-mode")
;; (setq comment-tags-keymap-prefix (kbd "C-c t"))
;; (with-eval-after-load "comment-tags"
;;   (setq comment-tags-keyword-faces
;;         `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
;;           ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
;;           ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
;;           ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
;;           ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
;;           ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
;;           ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
;;           ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
;;   (setq comment-tags-comment-start-only t
;;         comment-tags-require-colon t
;;         comment-tags-case-sensitive t
;;         comment-tags-show-faces t
;;         comment-tags-lighter nil))
;; (add-hook 'prog-mode-hook 'comment-tags-mode)

;; super agenda
(let ((org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "bills"
                :priority "A")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "Shopping in town"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :tag "@town"))
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          (:name "Space-related (non-moon-or-planet-related)"
                                 ;; Regexps match case-insensitively on the entire entry
                                 :and (:regexp ("space" "NASA")
                                               ;; Boolean NOT also has implicit OR between selectors
                                               :not (:regexp "moon" :tag "planet")))))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         )))
  (org-agenda nil "a"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme .t)
   ))


;; setup org-mode
;; Must do this so the agenda knows where to look for my files
(setq org-agenda-files '("~/org"))

;; When a TODO is set to a done state, record a timestamp
(setq org-log-done 'time)

;; Follow the links
(setq org-return-follows-link  t)

;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make the indentation look nicer
(add-hook 'org-mode-hook 'org-indent-mode)

;; Remap the change priority keys to use the UP or DOWN key
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)

;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; When you want to change the level of an org item, use SMR
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)

;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)

;; Wrap the lines in org mode so that things are easier to read
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-capture-templates
      '(
	("j" "Work Log Entry"
         entry (file+datetree "~/org/work-log.org")
         "* %?"
         :empty-lines 0)
        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 0)

        ("d" "Door Codes"
         entry (file+headline "~/org/notes.org" "Door Codes")
         "** %?"
         :empty-lines 0)
        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)
        ("c" "Code To-Do"
         entry (file+headline "~/org/todos.org" "Code Related Tasks")
         "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
         :empty-lines 0)
        )
      )

(setq org-todo-keywords
      '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )
        ))


(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "GoldenRod" :weight bold))
        ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("OBE" . (:foreground "LimeGreen" :weight bold))
        ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
        ))


;; Tags
(setq org-tag-alist '(
                      ;; Ticket types
                      (:startgroup . nil)
                      ("@bug" . ?b)
                      ("@feature" . ?u)
                      ("@spike" . ?j)
                      (:endgroup . nil)

                      ;; Ticket flags
                      ("@write_future_ticket" . ?w)
                      ("@emergency" . ?e)
                      ("@research" . ?r)

                      ;; Meeting types
                      (:startgroup . nil)
                      ("big_sprint_review" . ?i)
                      ("cents_sprint_retro" . ?n)
                      ("dsu" . ?d)
                      ("grooming" . ?g)
                      ("sprint_retro" . ?s)
                      (:endgroup . nil)

                      ;; Code TODOs tags
                      ("QA" . ?q)
                      ("backend" . ?k)
                      ("broken_code" . ?c)
                      ("frontend" . ?f)

                      ;; Special tags
                      ("CRITICAL" . ?x)
                      ("obstacle" . ?o)

                      ;; Meeting tags
                      ("HR" . ?h)
                      ("general" . ?l)
                      ("meeting" . ?m)
                      ("misc" . ?z)
                      ("planning" . ?p)

                      ;; Work Log Tags
                      ("accomplishment" . ?a)
                      ))


(setq org-tag-faces
      '(
        ("planning"  . (:foreground "mediumPurple1" :weight bold))
        ("backend"   . (:foreground "royalblue1"    :weight bold))
        ("frontend"  . (:foreground "forest green"  :weight bold))
        ("QA"        . (:foreground "sienna"        :weight bold))
        ("meeting"   . (:foreground "yellow1"       :weight bold))
        ("CRITICAL"  . (:foreground "red1"          :weight bold))
        )
      )
