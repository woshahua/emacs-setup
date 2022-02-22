;; Font Configuration -------------------------------------------------------
(defvar efs/default-font-size 150)
(defvar efs/default-variable-font-size 150)
(defvar efs/frame-transparency '(90 . 90))

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

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

;; org mode setup

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; solve mac ls option problems
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; basic ui setup
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

;; (menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;;Org mode configuration
;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("/Users/hang.gao/org-files"))

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
(require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))


(with-eval-after-load 'org-faces
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("77c16fe29860c6fc12cde892d6e3051c740a7b3781c988aeb4df9743f4a24e0d" default))
 '(package-selected-packages
   '(rainbow-delimiters forge lsp-treemacs counsel ivy-rich ivy-avy doom-modeline all-the-icons doom-themes no-littering evil-collection org-bullets magit vertico helm shx dired-sidebar company lsp-ui exec-path-from-shell go-mode dracula-theme use-package org-roam org)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/Users/hang.gao/org-files"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))


;; ref: https://dr-knz.net/a-tour-of-emacs-as-go-editor.html
(require 'swiper)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)


;; (add-mode-hook 'go-mode-hook (lambda ()
;;     (setq tab-width 4)))

(require 'yasnippet)
(yas-global-mode 1)

(require 'flycheck)
(global-flycheck-mode 1)

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp)

(add-hook 'before-save-hook 'gofmt-before-save)


(flycheck-define-checker go-build-escape
  "A Go escape checker using `go build -gcflags -m'."
  :command ("go" "build" "-gcflags" "-m"
            (option-flag "-i" flycheck-go-build-install-deps)
            ;; multiple tags are listed as "dev debug ..."
            (option-list "-tags=" flycheck-go-build-tags concat)
            "-o" null-device)
  :error-patterns
  (
   (warning line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline) "escapes to heap")
          line-end)
   (warning line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message "moved to heap:" (one-or-more not-newline))
          line-end)
   (info line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message "inlining call to " (one-or-more not-newline))
          line-end)
  )
  :modes go-mode
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))\
  )

(with-eval-after-load 'flycheck
   (add-to-list 'flycheck-checkers 'go-build-escape)
   (flycheck-add-next-checker 'go-gofmt 'go-build-escape))

; lsp, if enabled, disables the flycheck checkers by default.
; Re-add ours in that case.
(add-hook 'lsp-configure-hook
      (lambda ()
        (when (eq major-mode 'go-mode)
          (flycheck-add-next-checker 'lsp 'go-build-escape)))
      100)

; dired-x
(add-hook 'dired-load-hook (lambda () (load "dired-x")))

;; windows pane movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; golang setup
;;;; gofmt
(add-hook 'before-save-hook 'gofmt-before-save)

;;;; company

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; lsp tree mode
(use-package lsp-treemacs
  :after lsp)

;; lsp-ivy
(use-package lsp-ivy
  :after lsp)

;; exec env setting from shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; display line number
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

(use-package all-the-icons)
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

;; rainbow
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
