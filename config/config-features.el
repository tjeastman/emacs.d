(use-package diminish)

(load-theme 'zenburn t)

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
  :init
  (setq smex-save-file (expand-file-name "smex-items" user-emacs-state-directory))
  :config
  (smex-initialize))

(use-package ag
  :init
  (setq ag-highlight-search t)
  (setq ag-reuse-window t))

; highlight buffer changes caused by certain commands
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

; text completion framework
(require 'company)
(setq company-idle-delay 0.5
      company-minimum-prefix-length 3
      company-tooltip-limit 10
      company-tooltip-flip-when-above t)
(global-company-mode t)

; configure readline completion in shell mode
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
;;(setq comint-process-echoes t)
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))

(use-package yasnippet
  :config
  (yas-global-mode t))

; navigate contents of the kill ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

; use a tree-structured representation of undo history
(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

; project management
(use-package projectile
  :config
  (projectile-global-mode t))

; indicate current match index and total matches in the mode line when searching
(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode))

; highlight color strings with the colors they represent
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (rainbow-mode t))

; enable flycheck in python-mode
(require 'flycheck)
(add-hook 'python-mode-hook (lambda () (flycheck-mode)))

; enable access to rope refactoring library for Python through Pymacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-guess-project t)
(setq ropemacs-enable-autoimport t)
(setq ropemacs-autoimport-modules '("os" "sys"))

(add-hook 'python-mode-hook (lambda () (subword-mode +1)))

(setq git-commit-summary-max-length 72)

(setq magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))

; use ssh-specific modes for ssh configuration files
(use-package ssh-config-mode
  :ensure t
  :mode ((".ssh/config$" . ssh-config-mode)
         ("sshd_config$" . ssh-config-mode)
         ("ssh_config$" . ssh-config-mode)
         ("known_hosts$" . ssh-known-hosts-mode)
         ("authorized_keys$" . ssh-authorized-keys-mode)))

; preview files in dired
(use-package peep-dired
  :ensure t
  :defer t  ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(provide 'config-features)
