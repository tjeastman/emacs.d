;;; config-features.el --- configure features provided by third party packages

;;; Commentary:

;;; Code:

;; used by ivy for fuzzy matching below...
(use-package flx)

(use-package ivy
  :diminish ivy-mode
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-selectable-prompt t)
  (ivy-mode t))

(use-package ivy-historian
  :config
  (historian-mode t)
  (ivy-historian-mode t))

(use-package counsel
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp
   (regexp-opt completion-ignored-extensions))
  :bind
  (("C-x C-m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c k" . counsel-ag)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package avy
  :bind
  (("M-g g" . avy-goto-line)
   ("C-:" . avy-goto-char-timer)))

(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t))

;; highlight buffer changes caused by certain commands
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; text completion framework
(use-package company
  :diminish company-mode
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 3)
  (company-tooltip-limit 10)
  (company-tooltip-flip-when-above t)
  :config
  (global-company-mode t))

;; configure readline completion in shell mode
(use-package readline-complete
  :commands company-readline
  :hook (rlc-no-readline-hook . (lambda () (company-mode -1))))
(use-package shell
  :after company
  :custom
  (explicit-shell-file-name "bash")
  (explicit-bash-args '("-c" "export TERM=eterm-color; export EMACS=; stty echo; bash"))
  :config
  (push 'company-readline company-backends))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))
(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;; navigate contents of the kill ring
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

;; use a tree-structured representation of undo history
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; project management
(use-package projectile
  :after neotree
  :custom
  (projectile-use-git-grep t)
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien)
  (projectile-switch-project-action 'neotree-projectile-action)
  :config
  (projectile-global-mode))

;; highlight color strings with the colors they represent
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (rainbow-mode t))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  (magit-save-repository-buffers 'dontask))

(use-package git-commit
  :custom
  (git-commit-summary-max-length 72))

(use-package git-timemachine
  :commands git-timemachine)

;; preview files in dired
(use-package peep-dired
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package docker-tramp)

(use-package multiple-cursors)

(use-package electric-operator
  :hook
  ((c-mode-common . electric-operator-mode)
   (python-mode . electric-operator-mode)
   (protobuf-mode . electric-operator-mode))
  :config
  (electric-operator-add-rules-for-mode 'protobuf-mode (cons "=" " = ")))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package autoinsert
  :custom
  (auto-insert-alist nil)
  (auto-insert-query nil)
  :config
  (auto-insert-mode))

(use-package yatemplate
  :config
  (yatemplate-fill-alist))

(use-package imenu-anywhere
  :bind ("C-c i" . imenu-anywhere))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error)))

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(global-set-key (kbd "C-;") 'backward-kill-word)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key [remap move-beginning-of-line]
                'my-move-to-beginning-of-line)

(use-package rtags)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package irony
  :diminish irony-mode
  :hook
  ((c++-mode . irony-mode)
   (c-mode . irony-mode)))

(use-package flycheck-irony)

(use-package neotree
  :custom
  (neo-smart-open t)
  (neo-theme 'icons 'arrow))

(provide 'config-features)
;;; config-features.el ends here
