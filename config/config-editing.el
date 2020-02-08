(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode t))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package mouse
  :ensure nil
  :custom
  (mouse-yank-at-point t))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this))
  :custom
  (mc/list-file (expand-file-name ".mc-lists.el" user-emacs-directory)))

(use-package subword
  :ensure nil
  :config
  (global-subword-mode))

(provide 'config-editing)
