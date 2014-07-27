; save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file
      (expand-file-name "places" user-emacs-state-directory))

; store backup files in a central directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(provide 'config-editor)
