(use-package delsel
  :config
  (delete-selection-mode t))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package mouse
  :straight (:type built-in)
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

(provide 'config-editing)
