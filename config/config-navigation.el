(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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

(use-package avy-flycheck
  :bind
  ("C-c '" . avy-flycheck-goto-error))

(use-package deadgrep
  :bind
  ("C-c h" . deadgrep)
  :commands
  (deadgrep-edit-mode
   deadgrep-kill-all-buffers))

(use-package dired-sidebar
  :commands
  dired-sidebar-toggle-sidebar
  :custom
  (dired-sidebar-theme 'ascii))

(use-package goto-chg
  :bind
  ("C-c G" . goto-last-change))

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

(use-package subword
  :ensure nil
  :config
  (global-subword-mode))

(use-package swiper
  :bind
  ("C-s" . swiper))

(provide 'config-navigation)
