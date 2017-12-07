;; (package-initialize)

;; https://github.com/nilcons/emacs-use-package-fast
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; organize emacs feature state files
(setq user-emacs-state-directory
      (expand-file-name "state" user-emacs-directory))

;; store customizations outside of the emacs configuration directory
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; add subdirectories of contrib to the load path...
(let ((contrib-dir (expand-file-name "contrib" user-emacs-directory)))
  (if (file-directory-p contrib-dir)
      (dolist (contrib-subdir (directory-files contrib-dir))
        (unless (member contrib-subdir '("." ".."))
          (add-to-list 'load-path (expand-file-name contrib-subdir contrib-dir))))))

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
