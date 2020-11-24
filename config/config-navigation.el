(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0))
  :commands
  (avy-goto-char-timer
   avy-org-goto-heading-timer
   avy-org-refile-as-child)
  :custom
  (avy-all-windows t)
  (avy-background t)
  (avy-case-fold-search t)
  (avy-timeout-seconds 0.8)
  :config
  (avy-setup-default))

(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer))

(use-package recentf
  :ensure nil
  :bind
  ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250)
  :config
  (recentf-mode 1))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

(use-package window
  :ensure nil
  :bind
  ("M-o" . other-window))

(provide 'config-navigation)
