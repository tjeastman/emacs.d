(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t))

(use-package aggressive-indent
  :delight
  :config
  (global-aggressive-indent-mode 1))

(use-package autoinsert
  :custom
  (auto-insert-alist nil)
  (auto-insert-query nil)
  :config
  (auto-insert-mode))

(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0))
  :config
  (avy-setup-defaults))

(use-package beacon
  :delight
  :config
  (beacon-mode 1))

(use-package browse-kill-ring
  :bind
  ("C-x y" . browse-kill-ring))

(use-package company
  :delight
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 3)
  (company-tooltip-limit 10)
  (company-tooltip-flip-when-above t)
  :config
  (global-company-mode t))

(use-package counsel
  :bind
  ("C-x C-m" . counsel-M-x)
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp
   (regexp-opt completion-ignored-extensions))
  :config
  (counsel-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package crux
  :bind
  (("C-c I" . crux-find-user-init-file)
   ("C-c ," . crux-find-user-custom-file)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("s-k" . crux-kill-whole-line)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)))

(use-package deadgrep
  :bind
  ("C-c h" . deadgrep))

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package electric-operator
  :delight
  :commands
  (electric-operator-mode
   electric-operator-get-rules-for-mode
   electric-operator-add-rules-for-mode))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :custom
  (exec-path-from-shell-shell-name "/usr/local/bin/zsh")
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "SSH_AGENT_PID"
     "SSH_AUTH_SOCK"))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error)))

(use-package git-commit
  :custom
  (git-commit-summary-max-length 72))

(use-package git-timemachine
  :commands
  (git-timemachine
   git-timemachine-toggle))

(use-package highlight-indentation
  :delight
  :commands
  highlight-indentation-mode)

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook (lambda ()
                            (ibuffer-vc-set-filter-groups-by-vc-root)
                            (unless (eq ibuffer-sorting-mode 'alphabetic)
                              (ibuffer-do-sort-by-alphabetic)))))

(use-package hydra
  :commands defhydra)

(use-package imenu-anywhere
  :bind
  ("C-c i" . imenu-anywhere))

(use-package ivy
  :delight
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

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ;; :hook
  ;; (magit-status-mode . magit-filenotify-mode)
  :custom
  (magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  (magit-save-repository-buffers 'dontask)
  (magit-repository-directories '(("~/projects/" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; (use-package magit-filenotify
;;   :commands
;;   magit-filenotify-mode)

(use-package magit-todos
  :config
  (magit-todos-mode))

(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package projectile
  :demand
  :delight '(:eval (format " P[%s]" (projectile-project-name)))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t)
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien)
  (projectile-project-search-path '("~/projects/"))
  (projectile-sort-order 'recentf)
  :config
  (projectile-mode +1))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :delight
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))

(use-package swiper
  :bind
  ("C-s" . swiper))

;; use a tree-structured representation of undo history
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

;; highlight buffer changes caused by certain commands
(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode t))

(use-package yatemplate
  :custom
  (yatemplate-dir (expand-file-name "templates" user-emacs-directory))
  :config
  (yatemplate-fill-alist))

(provide 'config-features)
