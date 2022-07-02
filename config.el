(use-package direnv
  :config
  (direnv-mode))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :custom
  (exec-path-from-shell-shell-name "/opt/homebrew/bin/zsh")
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "SSH_AGENT_PID"
     "SSH_AUTH_SOCK"))
  :config
  (exec-path-from-shell-initialize))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package edit-indirect
  :commands
  edit-indirect-region)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package mouse
  :straight (:type built-in)
  :custom
  (mouse-yank-at-point t))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this))
  :custom
  (mc/list-file (expand-file-name ".mc-lists.el" user-emacs-directory)))

(defun my-run-new-shell-always ()
  "Run a shell in a new buffer regardless of how many shells are already running."
  (interactive)
  (let ((shell-buffer-index 0)
        (shell-buffer-name-format "*shell-%d*")
        (shell-buffer-name))
    (while ;; loop until an unused shell buffer name is found
        (progn
          (setq shell-buffer-index (1+ shell-buffer-index))
          (setq shell-buffer-name (format shell-buffer-name-format shell-buffer-index))
          (get-buffer shell-buffer-name)))
    (shell shell-buffer-name)))

(defun my-copy-filename-to-clipboard ()
  "Copy filename corresponding to the current buffer to clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename))))

(use-package git-commit
  :defer t
  :custom
  (git-commit-summary-max-length 72))

(use-package git-gutter-fringe
  :if window-system
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (set-face-foreground 'git-gutter-fr:added "#709080")
  (set-face-foreground 'git-gutter-fr:deleted "#cc9393")
  (set-face-foreground 'git-gutter-fr:modified "#f0dfaf")
  (global-git-gutter-mode t))

(use-package git-messenger
  :bind
  ("C-x G" . git-messenger:popup-message)
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t))

(use-package git-timemachine
  :commands
  (git-timemachine
   git-timemachine-toggle))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook (lambda ()
                            (ibuffer-vc-set-filter-groups-by-vc-root)
                            (unless (eq ibuffer-sorting-mode 'alphabetic)
                              (ibuffer-do-sort-by-alphabetic)))))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  :custom
  (magit-repository-directories '(("~/projects/" . 1)))
  (magit-save-repository-buffers 'dontask)
  (magit-section-visibility-indicator nil)
  (magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package projectile
  :demand
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

(use-package company
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 3)
  (company-tooltip-limit 10)
  (company-tooltip-flip-when-above t)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  :config
  (global-company-mode t))

(use-package ns-win
  :straight (:type built-in)
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super))

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
  :bind
  ("C-x C-b" . ibuffer))

(use-package recentf
  :bind
  ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250)
  :config
  (recentf-mode 1))

(use-package rg)

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package subword
  :config
  (global-subword-mode))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package electric-operator
  :commands
  (electric-operator-mode
   electric-operator-get-rules-for-mode
   electric-operator-add-rules-for-mode))

(use-package ccls
  :custom
  (ccls-sem-highlight-method 'overlays))

(use-package highlight-indentation
  :commands
  (highlight-indentation-mode
   highlight-indentation-current-column-mode))

(use-package lsp-ivy
  :commands
  lsp-ivy-workspace-symbol)

(use-package lsp-mode
  :commands
  (lsp
   lsp-deferred)
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-indentation nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-pyright)

(use-package platformio-mode
  :commands
  (platformio-mode
   platformio-conditionally-enable))

(use-package prog-mode
  :straight (:type built-in)
  :hook
  ((prog-mode . turn-on-smartparens-strict-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))

(use-package which-func
  :config
  (which-function-mode 1))

(use-package make-mode
  :mode
  ("Make.rules" . makefile-mode)
  :hook
  (makefile-mode . (lambda () (whitespace-toggle-options '(tabs)))))

(use-package elisp-mode
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode 'emacs-lisp-mode (cons "." " . ")))

(use-package blacken
  :commands blacken-buffer)

(use-package pip-requirements
  :custom
  (pip-requirements-index-url nil))

(use-package py-isort
  :commands py-isort-before-save)

(use-package python
  :hook
  (python-mode . highlight-indentation-mode))

(use-package python-pytest
  :bind
  ("C-c t" . python-pytest)
  ("C-x t" . python-pytest-dispatch)
  :custom
  (python-pytest-unsaved-buffers-behavior 'save-all))

(use-package ob-emacs-lisp
  :straight nil
  :commands
  (org-babel-expand-body:emacs-lisp
   org-babel-execute:emacs-lisp))

(use-package ob-http
  :commands
  (org-babel-expand-body:http
   org-babel-execute-body:http))

(use-package ob-python
  :straight nil
  :commands
  org-babel-execute:python)

(use-package ob-shell
  :straight nil
  :commands
  org-babel-execute:shell)

(use-package org
  :custom
  (org-babel-load-languages nil)
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-use-speed-commands t))

(use-package ob-async
  :after org
  :custom
  (org-babel-load-languages
   '((C . t)
     (clojure . t)
     (emacs-lisp . t)
     (http . t)
     (python . t)
     (shell . t))))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (if (eq system-type 'darwin)
      (setq dired-use-ls-dired nil)))
