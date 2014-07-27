(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; organize emacs feature state files
(setq user-emacs-state-directory
      (expand-file-name "state" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(require 'config-packages)
(require 'config-editor)
(require 'config-features)
(require 'config-keybindings)
