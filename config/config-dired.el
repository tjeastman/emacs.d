(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (if (eq system-type 'darwin)
      (setq dired-use-ls-dired nil)))

(make-variable-buffer-local 'my-dired-rsync-destination)
(use-package dired-rsync
  :bind
  (:map dired-mode-map ("C-c C-r" . my-dired-rsync))
  :init
  (defun my-dired-rsync ()
    (interactive)
    (if (and (boundp 'my-dired-rsync-destination) my-dired-rsync-destination)
        (dired-rsync my-dired-rsync-destination)
      (message "rsync destination is not set"))))

(provide 'config-dired)
