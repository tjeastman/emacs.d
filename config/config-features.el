(require 'diminish)

(load-theme 'zenburn t)

(require 'ido)
(require 'ido-ubiquitous)
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

(require 'smex)
(setq smex-save-file
      (expand-file-name "smex-items" user-emacs-state-directory))
(smex-initialize)

(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window t)

; highlight buffer changes caused by certain commands
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

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

; enable yasnippet template system
(require 'yasnippet)
(yas-global-mode t)

; navigate contents of the kill ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

; use a tree-structured representation of undo history
(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

; project management
(require 'projectile)
(projectile-global-mode t)

; indicate current match index and total matches in the mode line when searching
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

; highlight color strings with the colors they represent
(require 'rainbow-mode)
(rainbow-mode t)
(diminish 'rainbow-mode)

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

(provide 'config-features)
