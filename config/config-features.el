(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-ignore-extensions t
      ido-auto-merge-work-directories-length -1
      ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-state-directory)
      ido-use-faces nil)
(ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode +1)

(require 'smex)
(setq smex-save-file
      (expand-file-name "smex-items" user-emacs-state-directory))
(smex-initialize)

(provide 'config-features)
