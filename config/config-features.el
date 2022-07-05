(use-package counsel
  :bind
  ("C-x C-m" . counsel-M-x)
  :custom
  (counsel-find-file-at-point t)
  :config
  (counsel-mode))

(use-package counsel-projectile
  :defer t
  :config
  (counsel-projectile-mode))

(use-package crux
  :bind
  (("C-c I" . crux-find-user-init-file)
   ("C-c ," . crux-find-user-custom-file)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("s-k" . crux-kill-whole-line)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)))

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package esup
  :commands
  (esup))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error))
  :custom
  (flycheck-indication-mode nil))

(use-package helpful
  :defer t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

(use-package hydra
  :commands
  (defhydra))

(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  (ivy-initial-inputs-alist nil)
  :config
  ;; prescient fuzzy filtering makes swiper unusable:
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)))
  (ivy-mode t))

(use-package ivy-prescient
  :after ivy
  :custom
  (prescient-filter-method 'fuzzy)
  :config
  (ivy-prescient-mode))

(use-package ivy-rich
  :init
  (ivy-rich-mode))

(use-package prescient
  :config
  (prescient-persist-mode))

;; use a tree-structured representation of undo history
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; highlight buffer changes caused by certain commands
(use-package volatile-highlights
  :defer t
  :config
  (volatile-highlights-mode t))

(use-package which-key
  :config
  (which-key-mode))

(provide 'config-features)
