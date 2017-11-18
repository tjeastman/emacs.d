;; (package-initialize)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; organize emacs feature state files
(setq user-emacs-state-directory
      (expand-file-name "state" user-emacs-directory))

;; store customizations outside of the emacs configuration directory
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(require 'config-packages)
(require 'config-functions)
(require 'config-editor)
(require 'config-features)
(require 'config-python)
(require 'config-keybindings)
