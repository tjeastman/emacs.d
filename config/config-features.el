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

(provide 'config-features)
