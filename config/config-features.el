;;; config-features.el --- misc. feature configuration

;;; Commentary:

;;; Code:
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

(use-package smex
  :ensure t
  :custom
  (smex-save-file (expand-file-name "smex-items" user-emacs-state-directory))
  :bind ("C-x C-m" . smex)
  :config
  (smex-initialize))
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package ag
  :ensure t
  :defer t
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t))

;; highlight buffer changes caused by certain commands
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; text completion framework
(use-package company
  :ensure t
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
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

;; navigate contents of the kill ring
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

;; use a tree-structured representation of undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; project management
(use-package projectile
  :ensure t
  :custom
  (projectile-use-git-grep t)
  :config
  (projectile-global-mode t))

;; indicate current match index and total matches in the mode line when searching
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode))

;; highlight color strings with the colors they represent
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (rainbow-mode t))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  (magit-save-repository-buffers 'dontask))

(use-package git-commit
  :ensure t
  :custom
  (git-commit-summary-max-length 72))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)

;; preview files in dired
(use-package peep-dired
  :ensure t
  :defer t  ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package docker-compose-mode
  :ensure t)
(use-package docker-tramp
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package electric-spacing
  :ensure t
  :hook (prog-mode . electric-spacing-mode)
  :diminish electric-spacing-mode)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package autoinsert
  :ensure t
  :custom
  (auto-insert-alist nil)
  :config
  (auto-insert-mode))

(use-package yatemplate
  :ensure t
  :config
  (yatemplate-fill-alist))

(use-package imenu-anywhere
  :ensure t
  :bind ("C-c i" . imenu-anywhere))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(global-set-key (kbd "C-;") 'backward-kill-word)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key [remap move-beginning-of-line]
                'my-move-to-beginning-of-line)

(use-package rtags
  :ensure t)

(provide 'config-features)
;;; config-features.el ends here
