;; (package-initialize)

;; https://github.com/nilcons/emacs-use-package-fast
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(defconst user-emacs-state-directory
  (expand-file-name "state" user-emacs-directory)
  "root directory for organizing feature state files")

(when (eq system-type 'darwin)
  (setq
   mac-command-modifier 'meta
   mac-option-modifier 'super))

;; store customizations outside of the emacs configuration directory
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'config-packages)
(require 'config-functions)
(require 'config-appearance)
(require 'config-editor)
(require 'config-features)
(require 'config-files)
(require 'config-python)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
