;; (package-initialize)

;; https://github.com/nilcons/emacs-use-package-fast
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; store customizations outside of the emacs configuration directory
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'config-packages)
(require 'config-functions)
(require 'config-appearance)
(require 'config-editor)
(require 'config-keys)
(require 'config-features)
(require 'config-files)
(require 'config-python)
