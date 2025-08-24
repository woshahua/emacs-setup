;; early-init.el --- Early initialization file

;; Disable package.el at startup to avoid conflicts with straight.el
(setq package-enable-at-startup nil)

;; Performance optimizations during startup
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Reset gc-cons-threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000 ; 800kb
                  gc-cons-percentage 0.1)))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be an expensive operation
(setq frame-inhibit-implied-resize t)

;; Disable startup screen
(setq inhibit-startup-screen t)