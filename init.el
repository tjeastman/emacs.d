;; https://github.com/nilcons/emacs-use-package-fast
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; store customizations outside of the emacs configuration directory
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

(load-file (expand-file-name "config.el" user-emacs-directory))
