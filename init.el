(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

; organize emacs feature state files
(setq user-emacs-state-directory
      (expand-file-name "state" user-emacs-directory))

(defun add-top-level-subdirectories-to-load-path (&rest top-level-dirs)
  "Augment load-path with the top-level subdirectories of a list of directories."
  (dolist (top-level-dir top-level-dirs)
    (dolist (top-level-subdir (directory-files top-level-dir t "\\w+"))
      (add-to-list 'load-path top-level-subdir))))

(add-top-level-subdirectories-to-load-path
 (expand-file-name "contrib" user-emacs-directory))

; store customizations outside of the emacs configuration directory
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(require 'config-packages)
(require 'config-functions)
(require 'config-editor)
(require 'config-features)
(require 'config-keybindings)
