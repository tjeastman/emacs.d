;;; config-features.el --- misc. feature configuration

;;; Commentary:

;;; Code:
(use-package diminish)

(setq use-package-always-ensure t)

(require 'ido)
(require 'flx-ido)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-ignore-extensions t
      ido-auto-merge-work-directories-length -1
      ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-state-directory)
      ido-use-faces nil)
(ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode +1)

(add-to-list 'completion-ignored-extensions ".jar")
(add-to-list 'completion-ignored-extensions ".elf")
(add-to-list 'completion-ignored-extensions ".hex")
(add-to-list 'completion-ignored-extensions ".ropeproject/")
(add-to-list 'completion-ignored-extensions ".db")
(add-to-list 'completion-ignored-extensions "__pycache__/")

(use-package smex
  :custom
  (smex-save-file (expand-file-name "smex-items" user-emacs-state-directory))
  :bind ("C-x C-m" . smex)
  :config
  (smex-initialize))
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package ag
  :defer t
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t))

;; highlight buffer changes caused by certain commands
(use-package volatile-highlights
  :defer t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package aggressive-indent
  :defer t
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
(use-package shell
  :custom
  (explicit-shell-file-name "bash")
  (explicit-bash-args '("-c" "export EMACS=; stty echo; bash")))

(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

;; navigate contents of the kill ring
(use-package browse-kill-ring
  :defer t
  :config
  (browse-kill-ring-default-keybindings))

;; use a tree-structured representation of undo history
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; project management
(use-package projectile
  :custom
  (projectile-use-git-grep t)
  :config
  (projectile-global-mode t))

;; indicate current match index and total matches in the mode line when searching
(use-package anzu
  :defer t
  :diminish anzu-mode
  :config
  (global-anzu-mode))

;; highlight color strings with the colors they represent
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (rainbow-mode t))

(use-package magit
  :custom
  (magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  :bind ("C-x g" . magit-status))

(use-package git-commit
  :defer t
  :custom
  (git-commit-summary-max-length 72))

;; use ssh-specific modes for ssh configuration files
(use-package ssh-config-mode
  :mode ((".ssh/config$" . ssh-config-mode)
         ("sshd_config$" . ssh-config-mode)
         ("ssh_config$" . ssh-config-mode)
         ("known_hosts$" . ssh-known-hosts-mode)
         ("authorized_keys$" . ssh-authorized-keys-mode)))

;; preview files in dired
(use-package peep-dired
  :defer t  ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp)

(use-package multiple-cursors)

(use-package electric-spacing
  :diminish electric-spacing-mode
  :config
  (add-hook 'prog-mode-hook #'electric-spacing-mode))

(use-package thrift)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package autoinsert
  :custom
  (auto-insert-alist nil)
  :config
  (auto-insert-mode))

(use-package yatemplate
  :config
  (yatemplate-fill-alist))

(use-package imenu-anywhere
  :bind ("C-c i" . imenu-anywhere))

(use-package arduino-mode
  :mode "\\.ino\\'")

(use-package ansible-doc
  :bind (:map ansible-doc-module-mode ("C-c h a" . ansible-doc))
  :commands ansible-doc)

(use-package yaml-mode
  :bind (:map yaml-mode-map ("C-c h a" . ansible-doc))
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  (add-hook 'yaml-mode-hook 'flyspell-prog-mode))

(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(global-set-key (kbd "C-;") 'backward-kill-word)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key [remap move-beginning-of-line]
                'my-move-to-beginning-of-line)

(provide 'config-features)
;;; config-features.el ends here
