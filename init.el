;; fix python-mode bug
(setq elpy-shell-echo-output nil)

;; use shell setup
;; frame
(defvar efs/frame-transparency '(90 . 90))
(set-face-attribute 'default nil :font "Source Code Pro-12")

;; hide top bar
(add-to-list 'default-frame-alist '(undecorated . t))

;; straight.el bootstrap (primary package manager)
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

;; Use straight.el with use-package
(straight-use-package 'use-package)
;; Don't use straight by default - we'll specify :straight t when needed
(setq straight-use-package-by-default nil)

;; Keep package.el for compatibility (but don't use it for installation)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Function to clone git repos if they don't exist
(defun ensure-git-repo (repo-url local-path)
  "Clone git repo if it doesn't exist locally."
  (let ((full-path (expand-file-name local-path)))
    (unless (file-directory-p full-path)
      (message "Cloning %s to %s..." repo-url full-path)
      (shell-command (format "git clone %s %s" repo-url full-path)))))

;; Ensure git repos are cloned
(ensure-git-repo "https://github.com/manateelazycat/auto-save.git"
                 "~/ghq/github.com/manateelazycat/auto-save")
(ensure-git-repo "https://github.com/tonyaldon/org-bars.git"
                 "~/ghq/github.com/tonyaldon/org-bars")

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; solve mac ls option problems
(setq insert-directory-program "/opt/homebrew/bin/gls" dired-use-ls-dired t)
;; (setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; basic ui setup
(setq inhibit-startup-message t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(when window-system
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 91 80))

;; search using swiper
;; ref: https://dr-knz.net/a-tour-of-emacs-as-go-editor.html
(require 'swiper nil t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; delete end of line space
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; auto refresh buffer
(global-auto-revert-mode t)

;; setup evil
(use-package evil
  :straight t
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
  :straight t
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
  :straight t
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
  :straight t
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
  :straight t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :straight t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(acm-backend-codeium-api-key "c7d24326-3dd7-41c8-9818-9400bf71304d")
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "40352d95bc42c2e3acb7fc75afb3029d81a76897e14e9438857729cc87630980"
     "97283a649cf1ffd7be84dde08b45a41faa2a77c34a4832d3884c7f7bba53f3f5"
     "0279c1b81b569e46a4ee8e87e5e381568bf931257287191fd091f2948f7e2e8e"
     "daf189a2af425e9f376ddb9e99627e9d8f2ebdd5cc795065da81633f88389b4b"
     "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79"
     "d0dc7861b33d68caa92287d39cf8e8d9bc3764ec9c76bdb8072e87d90546c8a3"
     "4f03e70554a58349740973c69e73aefd8ce761a77b22a9dc52a19e708532084a"
     "fb7595c9571f2bd41635745d12551f35322296b70330056ddd0020ab2374671c"
     "c3e62e14eb625e02e5aeb03d315180d5bb6627785e48f23ba35eb7b974a940af"
     "144aa208033b570b4c31e054b77afa01b9e2349cdba14bb17c3e484c82effa30"
     "a087e01778a85f8381b2aa2b7b0832951aea078621b38844b6c8c8d638d73e3b"
     "b93039071f490613499b76c237c2624ae67a9aafbc717da9b4d81f456344e56e"
     "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8"
     "cd51c4e5a258ab9fd82de1fb4c0eee54326de5e307e3bf2895467ae103bc562b"
     "65c29375a6215a070bbc41af0a2dd4d3b2bf462b32e95d7fe4d8c86052ab0b9d"
     "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f"
     "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28"
     "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d"
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     default))
 '(elfeed-feeds
   '("https://us1.campaign-archive.com/feed?u=faa8eb4ef3a111cef92c4f3d4&id=e505c88a2e"
     "https://cprss.s3.amazonaws.com/javascriptweekly.com.xml"
     "http://www.alloyteam.com/feed/"
     "https://www.fatbobman.com/feed.rss"
     "https://onevcat.com/feed.xml" "https://catcoding.me/atom.xml"
     "https://feeds.feedburner.com/lzyy"
     "https://raw.githubusercontent.com/RSS-Renaissance/awesome-blogCN-feeds/master/feedlist.opml"
     "https://oleb.net/blog/atom.xml"
     "https://www.appcoda.com/navigationstack/"
     "feed://developer.apple.com/news/rss/news.rss"
     "https://developer.apple.com/news/" "https://pofat.substack.com/"
     "https://www.avanderlee.com/" "https://sarunw.com/"
     "https://github.com/SwiftOldDriver/iOS-Weekly/releases.atom"))
 '(org-agenda-files
   '("/Users/hanggao/org/inbox.org" "/Users/hanggao/org/journal.org") nil nil "Customized with use-package org")
 '(package-selected-packages
   '(ace-window sis flycheck projectile tide company-tabnine obsidian
		zenburn-theme enh-ruby-mode go-mode quelpa
		corfu-terminal corfu typescript-mode doom-modeline
		doom-themes ivy-rich counsel evil-collection yasnippet
		yaml-mode vertico use-package swiper no-littering
		exec-path-from-shell evil)))

;; move between window
(use-package ace-window
  :straight t)
(global-set-key (kbd "M-o") 'ace-window)

(use-package projectile
  :straight t
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
  :straight t
  :defer t
  :config
  (setq powerline-height 23
        powerline-default-separator (if (display-graphic-p) 'arrow 'utf-8)))

(provide 'init-modeline)

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

;; org-roam configuration
(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/org-roam/"))
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

;; org-mode 换行问题
(add-hook 'org-mode-hook 'visual-line-mode)

;; nerd-icons
(when (require 'nerd-icons nil t)
  (use-package treemacs-nerd-icons
    :straight t
    :config
    (treemacs-load-theme "nerd-icons"))

  (when (require 'nerd-icons-dired nil t)
    (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)))

;; yasnippet
(use-package yasnippet
  :straight t
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
  :straight t
  :defer t
  :after yasnippet)

;; autoformat
(use-package apheleia
  :straight t
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

;; auto-save
(add-to-list 'load-path "~/ghq/github.com/manateelazycat/auto-save") ; add auto-save to your load-path
(when (require 'auto-save nil t)
  (auto-save-enable))

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


;; setup auth-sources
(setq auth-sources
      '((:source "~/authinfo.gpg")))

;; org-mode style
(add-to-list 'load-path "~/ghq/github.com/tonyaldon/org-bars")
(when (require 'org-bars nil t)
  (add-hook 'org-mode-hook 'org-bars-mode))

;; org-download
(when (require 'org-download nil t)
  (add-hook 'dired-mode-hook 'org-download-enable))
(setq org-download-method 'directory) ; Save images in a directory
(setq org-download-image-dir "~/org/images") ; Directory relative to the Org file
(setq org-download-image-org-width 600)

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


(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme .t)
   ))


;; Enhanced org-mode setup
(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :custom
  ;; Basic org settings
  (org-directory "~/org")
  (org-agenda-files '("~/org" "~/org/roam"))
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Agenda settings
  (org-agenda-span 'week)
  (org-agenda-start-day nil)
  (org-agenda-start-on-weekday nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-block-separator ?─)
  (org-agenda-compact-blocks t)
  (org-agenda-start-with-log-mode t)

  ;; Refile settings
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Clock settings
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  (org-clock-persist-query-resume nil)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-remove-zero-time-clocks t)

  ;; Archive settings
  (org-archive-location "~/org/archive/%s_archive::")

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("C-c C-q" . counsel-org-tag)
         ("C-c <up>" . org-priority-up)
         ("C-c <down>" . org-priority-down))

  :config
  (org-clock-persistence-insinuate)

  ;; Better agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE"
                       ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("w" "Work Tasks" tags-todo "+work")
          ("p" "Personal Tasks" tags-todo "+personal")
          ("r" "Review"
           ((stuck "")
            (todo "WAITING"))))))


;; Enhanced TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAITING(w@/!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELLED(c@/!)")
        (sequence "IDEA(i)" "PLANNING(P)" "IN-PROGRESS(@/!)" "VERIFYING(v!)" "BLOCKED(b@)" "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)")))


(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#7c7c75" :weight bold))
        ("NEXT" . (:foreground "#0098dd" :weight bold))
        ("PROJ" . (:foreground "#708090" :weight bold))
        ("WAITING" . (:foreground "#9f7efe" :weight bold))
        ("SOMEDAY" . (:foreground "#d0bf8f" :weight bold))
        ("IDEA" . (:foreground "#ff6347" :weight bold))
        ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("CANCELLED" . (:foreground "gray" :weight bold))
        ("OBE" . (:foreground "LimeGreen" :weight bold))
        ("WONT-DO" . (:foreground "LimeGreen" :weight bold))))


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
(setq org-modern-star '("◉" "○" "◈" "◇" "*"))

;; org-super-agenda for better agenda grouping
(use-package org-super-agenda
  :straight t
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "🔥 Overdue"
                 :deadline past
                 :order 0)
          (:name "📅 Today"
                 :time-grid t
                 :todo "TODAY"
                 :scheduled today
                 :deadline today
                 :order 1)
          (:name "⚡ Next Tasks"
                 :todo "NEXT"
                 :order 2)
          (:name "🏢 Work"
                 :and (:tag "work" :not (:todo "SOMEDAY"))
                 :order 3)
          (:name "🏠 Personal"
                 :and (:tag "personal" :not (:todo "SOMEDAY"))
                 :order 4)
          (:name "📚 Learning"
                 :tag "learning"
                 :order 5)
          (:name "⏰ Waiting"
                 :todo "WAITING"
                 :order 6)
          (:name "💡 Ideas"
                 :todo "IDEA"
                 :order 7)
          (:name "📋 Projects"
                 :todo "PROJ"
                 :order 8)
          (:name "🔮 Someday"
                 :todo "SOMEDAY"
                 :order 9))))

;; org-ql for powerful querying
(use-package org-ql
  :straight t
  :after org
  :bind (("C-c o q" . org-ql-search)
         ("C-c o v" . org-ql-view)
         ("C-c o s" . org-ql-sparse-tree))
  :config
  ;; Some useful org-ql views
  (setq org-ql-views
        '(("Overview" :buffers-files org-agenda-files
           :query (and (todo)
                      (not (tags "archive")))
           :sort (date priority todo)
           :super-groups ((:name "Critical" :priority "A")
                         (:name "Important" :priority "B")
                         (:auto-category t)))

          ("Next Actions" :buffers-files org-agenda-files
           :query (todo "NEXT")
           :sort (priority deadline scheduled date)
           :super-groups ((:auto-tags t)))

          ("Stuck Projects" :buffers-files org-agenda-files
           :query (and (todo "PROJ")
                      (not (children (todo))))
           :sort date
           :super-groups ((:auto-tags t)))

          ("Waiting For" :buffers-files org-agenda-files
           :query (todo "WAITING")
           :sort date
           :super-groups ((:auto-tags t)))

          ("This Week" :buffers-files org-agenda-files
           :query (or (scheduled :from -7 :to 7)
                     (deadline :from -7 :to 7))
           :sort (scheduled deadline)
           :super-groups ((:name "Overdue" :scheduled past :deadline past)
                         (:name "Today" :scheduled today :deadline today)
                         (:auto-date t))))))

;; Calendar view with calfw
(use-package calfw
  :straight t
  :bind ("C-c o c" . cfw:open-org-calendar)
  :config
  (setq cfw:display-calendar-holidays nil))

(use-package calfw-org
  :straight t
  :after (calfw org))

;; Habit tracking
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (setq org-habit-graph-column 60
        org-habit-preceding-days 21
        org-habit-following-days 7
        org-habit-show-habits-only-for-today t))

;; Clock reporting and time tracking improvements
(use-package org-clock-today
  :straight t
  :after org
  :config
  (setq org-clock-today-hide-default-org-clock-mode-line t))

;; Quick capture improvements
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("T" "Todo with deadline" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  DEADLINE: %^{Deadline}t\n  %i\n  %a")
        ("s" "Scheduled todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  SCHEDULED: %^{Schedule}t\n  %i\n  %a")
        ("n" "Next Action" entry (file+headline "~/org/inbox.org" "Next Actions")
         "* NEXT %?\n  %i\n  %a")
        ("p" "Project" entry (file+headline "~/org/projects.org" "Projects")
         "* PROJ %? [/]\n  %i\n  %a\n** TODO First step")
        ("w" "Work Log" entry (file+datetree "~/org/work-log.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* IDEA %?\n  %i\n  %a")
        ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
         "* MEETING with %? :meeting:\n  %U")
        ("h" "Habit" entry (file+headline "~/org/habits.org" "Habits")
         "* TODO %?\n  SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n  :PROPERTIES:\n  :STYLE: habit\n  :REPEAT_TO_STATE: TODO\n  :END:\n  %U\n  %a\n  %i")
        ("r" "Review" entry (file+datetree "~/org/reviews.org")
         "* Review: %?\n  %U\n  %i")))

;; display remote image in markdown
(setq markdown-display-remote-images t)

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; configure doom-modeline
(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15))

(use-package json-reformat
  :straight t)

;; fanyi
(use-package fanyi
  :straight t
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))


;; puml setup
;; (setq plantuml-jar-path "/Users/hang.gao/plantuml.jar")
;; (setq plantuml-default-exec-mode 'jar)

;; display inline images
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
        (let* ((case-fold-search t)
	       (file-extension-re (image-file-name-regexp))
	       (link-abbrevs (mapcar #'car
                                     (append org-link-abbrev-alist-local
                                             org-link-abbrev-alist)))
	       ;; Check absolute, relative file names and explicit
	       ;; "file:" links.  Also check link abbreviations since
	       ;; some might expand to "file" links.
	       (file-types-re
                (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
                        (if (not link-abbrevs) ""
                          (concat "\\|" (regexp-opt link-abbrevs))))))
          (while (re-search-forward file-types-re end t)
            (let* ((link (org-element-lineage
                          (save-match-data (org-element-context))
                          '(link) t))
                   (linktype (org-element-property :type link))
                   (inner-start (match-beginning 1))
                   (path
                    (cond
                     ;; No link at point; no inline image.
                     ((not link) nil)
                     ;; File link without a description.  Also handle
                     ;; INCLUDE-LINKED here since it should have
                     ;; precedence over the next case.  I.e., if link
                     ;; contains filenames in both the path and the
                     ;; description, prioritize the path only when
                     ;; INCLUDE-LINKED is non-nil.
                     ((or (not (org-element-property :contents-begin link))
                          include-linked)
                      (and (or (equal "file" linktype)
			       (equal "attachment" linktype))
                           (org-element-property :path link)))
                     ;; Link with a description.  Check if description
                     ;; is a filename.  Even if Org doesn't have syntax
                     ;; for those -- clickable image -- constructs, fake
                     ;; them, as in `org-export-insert-image-links'.
                     ((not inner-start) nil)
                     (t
                      (org-with-point-at inner-start
                        (and (looking-at
                              (if (char-equal ?< (char-after inner-start))
                                  org-link-angle-re
                                org-link-plain-re))
                             ;; File name must fill the whole
                             ;; description.
                             (= (org-element-property :contents-end link)
                                (match-end 0))
                             (match-string 2)))))))
              (when (and path (string-match-p file-extension-re path))
                (let ((file (if (equal "attachment" linktype)
                                (progn
                                  (require 'org-attach)
                                  (ignore-errors (org-attach-expand path)))
			      (expand-file-name path))))
                  (when (and file (file-exists-p file))
                    (let ((width
                           ;; Apply `org-image-actual-width' specifications.
                           (cond

                            ;; if org-image-actual-width is true, return nil
                            ((eq org-image-actual-width t) nil)

                            ((listp org-image-actual-width)
                             (or
                              ;; First try to find a width among
                              ;; attributes associated to the paragraph
                              ;; containing link.
                              (pcase (org-element-lineage link '(paragraph))
                                (`nil nil)
                                (p
                                 (let* ((case-fold-search t)
                                        (end (org-element-property :post-affiliated p))
                                        (re "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"))
                                   (when (org-with-point-at
                                             (org-element-property :begin p)
                                           (re-search-forward re end t))
                                     (string-to-number (match-string 1))))))
                              ;; Otherwise, fall-back to provided number.
                              (car org-image-actual-width)))

                            ((numberp org-image-actual-width) (let* (
                                                                     ;; get the image dimensions
                                                                     (this-image-dimensions (image-size (create-image file) t nil))
                                                                     (actual-width (car this-image-dimensions))
                                                                     (actual-height (cdr this-image-dimensions))
                                                                     ;; calculate the aspect ratio
                                                                     (this-aspect-ratio (/ (float actual-width) (float actual-height)))
                                                                     ;; get the window width, and subtract 100 pixels
                                                                     (this-window-width (- (window-pixel-width) 100))
                                                                     ;; get the window height, and multiply it by 0.5
                                                                     (this-window-height (* (window-pixel-height) 0.5))
                                                                     ;; minimum of image width and window width
                                                                     (min-width (min actual-width this-window-width))
                                                                     ;; minimum of image height and window height
                                                                     (min-height (min actual-height this-window-height))
                                                                     ;; conserve aspect ratio
                                                                     (min-width-via-height (truncate (* (float min-height) this-aspect-ratio)))
                                                                     )
                                                                (max (min min-width min-width-via-height org-image-actual-width) 10)))

			    (t nil)))

			  (old (get-char-property-and-overlay
				(org-element-property :begin link)
				'org-image-overlay)))
		      (if (and (car-safe old) refresh)
			  (image-refresh (overlay-get (cdr old) 'display))
			(let ((image (create-image file
						   (and (image-type-available-p 'imagemagick)
							width 'imagemagick)
						   nil
						   :width width)))
			  (when image
			    (let ((ov (make-overlay
				       (org-element-property :begin link)
				       (progn
					 (goto-char
					  (org-element-property :end link))
					 (skip-chars-backward " \t")
					 (point)))))
			      (overlay-put ov 'display image)
			      (overlay-put ov 'face 'default)
			      (overlay-put ov 'org-image-overlay t)
			      (overlay-put
			       ov 'modification-hooks
			       (list 'org-display-inline-remove-overlay))
			      (when (<= 26 emacs-major-version)
				(cl-assert (boundp 'image-map))
				(overlay-put ov 'keymap image-map))
			      (push ov org-inline-image-overlays))))))))))))))))
;; enable the func on org-mode-hook
(setq org-image-actual-width 1800)

(add-hook 'org-mode-hook
          (lambda ()
            (org-display-inline-images)))

(load-theme 'spacemacs-dark)

;; handle space
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])  ; 全角空格显示为方块
	))  ; 制表符

;; 启用 whitespace-mode
(global-whitespace-mode 1)

;; 设置要显示的空白字符类型
(setq whitespace-style '(face trailing spaces tabs newline
                        space-mark))
