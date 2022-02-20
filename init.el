;; mela package repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; org mode setup

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("77c16fe29860c6fc12cde892d6e3051c740a7b3781c988aeb4df9743f4a24e0d" default))
 '(package-selected-packages
   '(vertico helm shx dired-sidebar company lsp-ui exec-path-from-shell go-mode dracula-theme use-package org-roam org)))
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

;; Don't change the font size for some headings and titles (default t)
(setq dracula-enlarge-headings nil)

;; Adjust font size of titles level 1 (default 1.3)
(setq dracula-height-title-1 1.25)

;; Adjust font size of titles level 2 (default 1.1)
(setq dracula-height-title-1 1.15)

;; Adjust font size of titles level 3 (default 1.0)
(setq dracula-height-title-1 1.05)

;; Adjust font size of document titles (default 1.44)
(setq dracula-height-doc-title 1.4)

;; Use less pink and bold on the mode-line and minibuffer (default nil)
(setq dracula-alternate-mode-line-and-minibuffer t)

;; load dracula
(load-theme 'dracula t)

(menu-bar-mode 0)
(column-number-mode 1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;; see the apropos entry for whitespace-style
(setq
   whitespace-style
   '(face ; viz via faces
     trailing ; trailing blanks visualized
     lines-tail ; lines beyond
                ; whitespace-line-column
     space-before-tab
     space-after-tab
     newline ; lines with only blanks
     indentation ; spaces used for indent
                 ; when config wants tabs
     empty ; empty lines at beginning or end
     )
   whitespace-line-column 100 ; column at which
        ; whitespace-mode says the line is too long
   )


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


;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; windows pane movement
(global-set-key [M-left] 'windmove-left)          ; move to left window
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to lower window

;; set default fullscreen
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


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


;; dired setup
(use-package dired
  :eunsure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "=-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
			      "h" 'dired-up-directory
			      "l" dired-find-file))


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
