(ido-mode)

(require 'smex)
(setq smex-save-file
      (expand-file-name "smex-items" user-emacs-state-directory))
(smex-initialize)

(provide 'config-features)
