;; [[file:config.org::*Garbage Collection][Garbage Collection:1]]
(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)
;; Garbage Collection:1 ends here

;; [[file:config.org::*Garbage Collection][Garbage Collection:2]]
(add-hook 'after-init-hook
  (lambda ()
    (setq
     gc-cons-threshold 16777216
     gc-cons-percentage 0.1)))
;; Garbage Collection:2 ends here

;; [[file:config.org::*Messages][Messages:1]]
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
;; Messages:1 ends here

;; [[file:config.org::*Customizations][Customizations:1]]
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)
;; Customizations:1 ends here

;; [[file:config.org::*Packages][Packages:1]]
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Packages:1 ends here

;; [[file:config.org::*Packages][Packages:2]]
(straight-use-package 'use-package)
;; Packages:2 ends here

;; [[file:config.org::*Packages][Packages:3]]
(use-package straight
  :custom
  (straight-use-package-by-default t))
;; Packages:3 ends here

;; [[file:config.org::*Packages][Packages:4]]
(use-package use-package
  :custom
  (use-package-verbose t))
;; Packages:4 ends here

;; [[file:config.org::*Packages][Packages:5]]
(use-package no-littering
  :config
  (no-littering-theme-backups))
;; Packages:5 ends here

;; [[file:config.org::*Appearance][Appearance:1]]
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))
;; Appearance:1 ends here

;; [[file:config.org::*Appearance][Appearance:2]]
(setq-default cursor-in-non-selected-windows nil)
;; Appearance:2 ends here

;; [[file:config.org::*Appearance][Appearance:3]]
(use-package hl-line
  :straight (:type built-in)
  :if window-system
  :custom
  (global-hl-line-sticky-flag nil)
  :config
  (global-hl-line-mode t))
;; Appearance:3 ends here

;; [[file:config.org::*Appearance][Appearance:4]]
(setq visible-bell t)
(if (eq system-type 'darwin)
    (setq ring-bell-function 'ignore))

(setq-default
 ;; only allow continuation lines in buffers that occupy the full frame width
 truncate-lines nil
 truncate-partial-width-windows t)

;; prefer horizontal splits
(setq split-height-threshold 9999)

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))
;; Appearance:4 ends here

;; [[file:config.org::*Appearance][Appearance:5]]
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width 5)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-env-enable-python t)
  (doom-modeline-height 20)
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes nil))
;; Appearance:5 ends here

;; [[file:config.org::*Appearance][Appearance:6]]
(use-package doom-themes
  :if window-system
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-solarized-light t))
;; Appearance:6 ends here

;; [[file:config.org::*Appearance][Appearance:7]]
(use-package frame
  :straight (:type built-in)
  :bind
  ("M-RET" . toggle-frame-fullscreen))
;; Appearance:7 ends here

;; [[file:config.org::*Appearance][Appearance:8]]
(use-package highlight-indentation
  :commands
  (highlight-indentation-mode
   highlight-indentation-current-column-mode))
;; Appearance:8 ends here

;; [[file:config.org::*Appearance][Appearance:9]]
(use-package rainbow-delimiters
  :commands
  (rainbow-delimiters-mode))
;; Appearance:9 ends here

;; [[file:config.org::*Appearance][Appearance:10]]
(use-package simple
  :straight (:type built-in)
  :config
  (line-number-mode t)
  (size-indication-mode t))
;; Appearance:10 ends here

;; [[file:config.org::*Environment][Environment:1]]
(use-package direnv
  :config
  (direnv-mode))

(use-package dotenv-mode
  :defer t)

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

(use-package keychain-environment
  :defer t
  :config
  (keychain-refresh-environment))
;; Environment:1 ends here

;; [[file:config.org::*Projects][Projects:1]]
(use-package git-gutter-fringe
  :if window-system
  :defer t
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

(use-package git-modes
  :straight (:host github :repo "magit/git-modes" :branch "main")
  :mode
  ("/\\.gitconfig\\'" . gitconfig-mode)
  ("/\\.git/config\\'" . gitconfig-mode)
  ("/\\.gitignore\\'" . gitignore-mode)
  ("/.dockerignore\\'" . gitignore-mode))

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
  (git-commit-summary-max-length 72)
  (magit-repository-directories '(("~/projects/" . 1)))
  (magit-save-repository-buffers 'dontask)
  (magit-section-visibility-indicator nil)
  (magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package vc
  :custom
  (vc-follow-symlinks t))
;; Projects:1 ends here

;; [[file:config.org::*Interface][Interface:1]]
(defalias 'yes-or-no-p 'y-or-n-p)
;; Interface:1 ends here

;; [[file:config.org::*Interface][Interface:2]]
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Interface:2 ends here

;; [[file:config.org::*Interface][Interface:3]]
(use-package crux
  :bind
  (("C-c D" . crux-delete-file-and-buffer)
   ("C-c I" . crux-find-user-init-file)
   ("C-c ," . crux-find-user-custom-file)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("s-k" . crux-kill-whole-line)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line))
  :commands
  (crux-capitalize-region
   crux-downcase-region
   crux-upcase-region
   crux-eval-and-replace))
;; Interface:3 ends here

;; [[file:config.org::*Interface][Interface:4]]
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command))
  :custom
  (helpful-max-buffers 10))
;; Interface:4 ends here

;; [[file:config.org::*Interface][Interface:5]]
(use-package hydra
  :commands
  (defhydra))

(use-package ns-win
  :straight (:type built-in)
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super))

(use-package simple
  :straight (:type built-in)
  :bind
  ("C-x C-m" . execute-extended-command))
;; Interface:5 ends here

;; [[file:config.org::*Interface][Interface:6]]
(use-package which-key
  :config
  (which-key-mode))
;; Interface:6 ends here

;; [[file:config.org::*Navigation][Navigation:1]]
(setq
 ;; preserve the vertical position of the line containing the point
 scroll-preserve-screen-position t
 ;; never vertically recenter windows
 scroll-conservatively 100000
 scroll-margin 0)
;; Navigation:1 ends here

;; [[file:config.org::*Navigation][Navigation:2]]
(use-package ace-link
  :config
  (ace-link-setup-default))
;; Navigation:2 ends here

;; [[file:config.org::*Navigation][Navigation:3]]
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;; Navigation:3 ends here

;; [[file:config.org::*Navigation][Navigation:4]]
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
  (avy-dispatch-alist '((?x . avy-action-kill-move)
                        (?X . avy-action-kill-stay)
                        (?t . avy-action-teleport)
                        (?m . avy-action-mark)
                        (?n . avy-action-copy)
                        (?y . avy-action-yank)
                        (?Y . avy-action-yank-line)
                        (?i . avy-action-ispell)
                        (?z . avy-action-zap-to-char)))
  :config
  (avy-setup-default))

(use-package deadgrep
  :bind
  ("C-c h" . deadgrep)
  :commands
  (deadgrep-edit-mode
   deadgrep-kill-all-buffers))

(use-package dired-sidebar
  :commands
  (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'ascii))

(use-package goto-chg
  :bind
  ("C-c G" . goto-last-change))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package imenu-anywhere
  :bind
  ("C-c i" . imenu-anywhere))

(use-package recentf
  :straight (:type built-in)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode 1))
;; Navigation:4 ends here

;; [[file:config.org::*Navigation][Navigation:5]]
(use-package wgrep)
;; Navigation:5 ends here

;; [[file:config.org::*Navigation][Navigation:6]]
(use-package saveplace
  :straight (:type built-in)
  :config
  (save-place-mode 1))
;; Navigation:6 ends here

;; [[file:config.org::*Navigation][Navigation:7]]
(use-package subword
  :straight (:type built-in)
  :config
  (global-subword-mode))
;; Navigation:7 ends here

;; [[file:config.org::*Navigation][Navigation:8]]
(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))
;; Navigation:8 ends here

;; [[file:config.org::*Navigation][Navigation:9]]
(use-package winner
  :straight (:type built-in)
  :init
  (winner-mode))
;; Navigation:9 ends here

;; [[file:config.org::*Completion][Completion:1]]
(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("M-g M-g" . consult-goto-line)
   ("C-c r" . consult-ripgrep))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :commands
  (consult-yank-pop))
;; Completion:1 ends here

;; [[file:config.org::*Completion][Completion:2]]
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode)
  :config
  (corfu-history-mode 1))
;; Completion:2 ends here

;; [[file:config.org::*Completion][Completion:3]]
(use-package embark
  :bind
  ("C-c a" . embark-act))

(use-package embark-consult
  :ensure nil
  :after (embark consult))
;; Completion:3 ends here

;; [[file:config.org::*Completion][Completion:4]]
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-blend-frac 0.08)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; Completion:4 ends here

;; [[file:config.org::*Completion][Completion:5]]
(use-package marginalia
  :defer 1
  :custom
  (marginalia-align 'left)
  (marginalia-max-relative-age 0)
  :config
  (marginalia-mode))
;; Completion:5 ends here

;; [[file:config.org::*Completion][Completion:6]]
(use-package orderless
  :preface
  (defun my-orderless-dispatch (pattern index total)
    (cond
     ((string= "!" pattern) `(orderless-literal . ""))
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "^" pattern) `(orderless-prefixes . ,(substring pattern 1)))
     ((= index 0) 'orderless-flex)))
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (orderless-style-dispatchers '(my-orderless-dispatch))
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Completion:6 ends here

;; [[file:config.org::*Completion][Completion:7]]
(use-package savehist
  :straight (:type built-in)
  :defer 1
  :custom
  (savehist-autosave-interval (* 5 60))
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history))
;; Completion:7 ends here

;; [[file:config.org::*Completion][Completion:8]]
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :defer 1
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-count 10)
  (vertico-cycle t)
  (vertico-resize 'grow-only)
  (vertico-scroll-margin 2)
  :config
  (vertico-mode))

(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))
;; Completion:8 ends here

(setq tab-always-indent 'complete)

;; [[file:config.org::*Editing][Editing:1]]
(setq-default
 indent-tabs-mode nil
 tab-width 4)
;; Editing:1 ends here

;; [[file:config.org::*Editing][Editing:2]]
(use-package abbrev
  :straight (:type built-in)
  :custom
  (abbrev-file-name (expand-file-name "abbreviations" user-emacs-directory))
  :config
  (setq-default abbrev-mode t)
  (quietly-read-abbrev-file abbrev-file-name))
;; Editing:2 ends here

;; [[file:config.org::*Editing][Editing:3]]
(use-package aggressive-indent
  :commands
  (aggressive-indent-mode))
;; Editing:3 ends here

;; [[file:config.org::*Editing][Editing:4]]
(use-package autoinsert
  :straight (:type built-in)
  :custom
  (auto-insert-alist nil)
  (auto-insert-query nil)
  :config
  (auto-insert-mode))
;; Editing:4 ends here

;; [[file:config.org::*Editing][Editing:5]]
(use-package autorevert
  :config
  (global-auto-revert-mode t))
;; Editing:5 ends here

;; [[file:config.org::*Editing][Editing:6]]
(use-package browse-kill-ring
  :bind
  ("C-x y" . browse-kill-ring))
;; Editing:6 ends here

;; [[file:config.org::*Editing][Editing:7]]
(use-package delsel
  :config
  (delete-selection-mode t))
;; Editing:7 ends here

;; [[file:config.org::*Editing][Editing:8]]
(use-package edit-indirect
  :commands
  (edit-indirect-region))
;; Editing:8 ends here

;; [[file:config.org::*Editing][Editing:9]]
(use-package electric-operator
  :commands
  (electric-operator-mode
   electric-operator-get-rules-for-mode
   electric-operator-add-rules-for-mode))
;; Editing:9 ends here

;; [[file:config.org::*Editing][Editing:10]]
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  :commands
  (er/expand-region
   er/contract-region))
;; Editing:10 ends here

;; [[file:config.org::*Editing][Editing:11]]
(use-package mouse
  :straight (:type built-in)
  :custom
  (mouse-yank-at-point t))

(use-package move-text
  :commands
  (move-text-up
   move-text-down))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this))
  :commands
  (mc/edit-lines)
  :custom
  (mc/list-file (expand-file-name ".mc-lists.el" user-emacs-directory)))

(use-package selected
  :commands
  (selected-minor-mode)
  :bind (:map selected-keymap
              ("=" . er/expand-region)
              ("-" . er/contract-region)
              ("a" . apply-macro-to-region-lines)
              ("y" . consult-yank-pop)
              ("w" . count-words-region)
              ("c" . crux-capitalize-region)
              ("l" . crux-downcase-region)
              ("u" . crux-upcase-region)
              ("d" . crux-duplicate-and-comment-current-line-or-region)
              ("e" . crux-eval-and-replace)
              ("p" . move-text-up)
              ("n" . move-text-down)
              ("m" . mc/edit-lines)
              ("q" . selected-off)
              ("w" . kill-region)
              ("r" . delete-region)
              ("/" . undo-tree-undo)))

(use-package simple
  :straight (:type built-in)
  :custom
  (next-line-add-newlines t))

;; use a tree-structured representation of undo history
(use-package undo-tree
  :commands
  (undo-tree-undo)
  :config
  (global-undo-tree-mode))

;; visualize unwanted whitespace characters and lines that are too long
(use-package whitespace
  :commands
  (whitespace-cleanup)
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face tabs empty trailing lines-tail)))

(use-package yasnippet
  :defer t
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode t))

(use-package yatemplate
  :defer t
  :custom
  (yatemplate-dir (expand-file-name "templates" user-emacs-directory))
  :config
  (yatemplate-fill-alist))
;; Editing:11 ends here

(use-package flyspell
  :straight (:type built-in)
  :commands
  (flyspell-mode
   flyspell-prog-mode))

(use-package ispell
  :custom
  (ispell-personal-dictionary "~/.aspell.en.pws")
  (ispell-program-name "aspell"))

(use-package text-mode
  :straight (:type built-in)
  :hook
  (text-mode . flyspell-mode))

;; [[file:config.org::*General][General:1]]
(use-package apheleia
  :config
  (apheleia-global-mode))
;; General:1 ends here

;; [[file:config.org::*General][General:2]]
(use-package treesit
  :straight (:type built-in)
  :defer t
  :preface
  (defun my-setup-install-grammars ()
    (dolist (grammar
             '((c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.23.4"))
               (cmake . ("https://github.com/uyha/tree-sitter-cmake" "v0.5.0"))
               (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
           '((c++-mode . c++-ts-mode)
             (c-mode . c-ts-mode)
             (cmake-mode . cmake-ts-mode)
             (javascript-mode . js-ts-mode)
             (python-mode . python-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (my-setup-install-grammars))
;; General:2 ends here

;; [[file:config.org::*General][General:3]]
(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename))
  :custom-face
  (eglot-highlight-symbol-face ((t :inherit normal :underline t))))

(use-package elec-pair
  :straight (:type built-in)
  :commands
  electric-pair-local-mode)

(use-package flymake
  :straight (:type built-in)
  :bind
  (:map flymake-mode-map
        ("C-c e n" . flymake-goto-next-error)
        ("C-c e p" . flymake-goto-prev-error)
        ("C-c e d" . flymake-show-buffer-diagnostics)
        ("C-c e D" . flymake-show-project-diagnostics))
  :custom
  (flymake-fringe-indicator-position nil)
  (flymake-margin-indicator-position nil)
  (flymake-no-changes-timeout 1))

(use-package lsp-mode
  :preface
  ;; https://github.com/minad/corfu/issues/41
  (defun my-lsp-passthrough-to-orderless ()
    "Replace lsp-passthrough completion style with orderless."
    (setcdr (cadr (assq 'lsp-capf completion-category-defaults)) '(orderless)))
  :commands
  (lsp
   lsp-deferred)
  :hook
  (lsp-mode . my-lsp-passthrough-to-orderless)
  :custom
  (lsp-auto-guess-root t)
  (lsp-completion-provider :none)
  (lsp-enable-indentation nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-lens-enable nil))

(use-package lsp-pyright
  :defer t)

(use-package paredit
  :commands
  (enable-paredit-mode))

(use-package prog-mode
  :straight (:type built-in)
  :hook
  ((prog-mode . flyspell-prog-mode)
   (prog-mode . rainbow-delimiters-mode)))

(use-package which-func
  :config
  (which-function-mode 1))
;; General:3 ends here

;; [[file:config.org::*General][General:4]]
(use-package combobulate
  :after treesit
  :custom
  (combobulate-key-prefix "C-c o"))
;; General:4 ends here

;; [[file:config.org::*Make][Make:1]]
(use-package make-mode
  :mode
  ("Make.rules" . makefile-mode)
  :hook
  (makefile-mode . (lambda () (whitespace-toggle-options '(tabs)))))
;; Make:1 ends here

;; [[file:config.org::*CMake][CMake:1]]
(use-package cmake-integration
  :straight (:host github :repo "darcamo/cmake-integration" :branch "main")
  :defer 1
  :commands
  (cmake-integration-cmake-reconfigure
   cmake-integration-cmake-configure-with-preset
   cmake-integration-save-and-compile
   cmake-integration-save-and-compile-last-target
   cmake-integration-run-last-target
   cmake-integration-run-last-target-with-arguments))

(use-package cmake-mode
  :defer t)
;; CMake:1 ends here

;; [[file:config.org::*C/C++][C/C++:1]]
(use-package cc-mode
  :hook
  (c-mode-common . electric-pair-local-mode))

(use-package ccls
  :custom
  (ccls-sem-highlight-method 'overlays))

(use-package cuda-mode
  :mode "\\.cuh?\\'")
;; C/C++:1 ends here

;; [[file:config.org::*Emacs Lisp][Emacs Lisp:1]]
(use-package elisp-mode
  :straight (:type built-in)
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)
   (emacs-lisp-mode . enable-paredit-mode))
  :config
  (electric-operator-add-rules-for-mode 'emacs-lisp-mode (cons "." " . ")))
;; Emacs Lisp:1 ends here

;; [[file:config.org::*JavaScript][JavaScript:1]]
(use-package typescript-ts-mode
  :straight (:type built-in))
;; JavaScript:1 ends here

;; [[file:config.org::*Python][Python:1]]
(use-package pip-requirements
  :defer t
  :custom
  (pip-requirements-index-url nil))

(use-package python
  :hook
  ((python-ts-mode . highlight-indentation-mode)
   (python-ts-mode . electric-pair-local-mode)
   (python-ts-mode . combobulate-mode)))

(use-package python-pytest
  :bind
  ("C-c t" . python-pytest)
  ("C-x t" . python-pytest-dispatch)
  :custom
  (python-pytest-unsaved-buffers-behavior 'save-all))
;; Python:1 ends here

;; [[file:config.org::*Shell][Shell:1]]
(use-package sh-script
  :mode
  (("zshrc\\'" . sh-mode)
   (".zsh_personal\\'" . sh-mode)
   ("\\.zsh-theme\\'" . sh-mode)))
;; Shell:1 ends here

;; [[file:config.org::*Embedded][Embedded:1]]
(use-package arduino-mode
  :mode
  (("\\.ino\\'" . arduino-mode)
   ("\\.pde\\'" . arduino-mode)))

(use-package platformio-mode
  :commands
  (platformio-mode
   platformio-conditionally-enable))
;; Embedded:1 ends here

;; [[file:config.org::*Org][Org:1]]
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
  (org-babel-execute:python))

(use-package ob-shell
  :straight nil
  :commands
  (org-babel-execute:shell))

(use-package org
  :defer t
  :hook (org-mode . visual-line-mode)
  :custom
  (org-babel-load-languages nil)
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-use-speed-commands t))

(use-package org-roam
  :defer t
  :custom
  (org-roam-directory (concat (getenv "HOME") "/Documents/notes/"))
  :config
  (org-roam-db-autosync-enable))

(use-package ob-async
  :after org
  :defer t
  :custom
  (org-babel-load-languages
   '((C . t)
     (clojure . t)
     (emacs-lisp . t)
     (http . t)
     (python . t)
     (shell . t))))
;; Org:1 ends here

;; [[file:config.org::*Dired][Dired:1]]
(use-package dired
  :straight (:type built-in)
  :custom
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (if (eq system-type 'darwin)
      (setq dired-use-ls-dired nil)))
;; Dired:1 ends here

;; [[file:config.org::*Docker][Docker:1]]
(use-package docker
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :defer t)

(use-package docker-compose-mode
  :defer t)
;; Docker:1 ends here

;; [[file:config.org::*Files][Files:1]]
(use-package apt-sources-list
  :defer t)

(use-package i3wm-config-mode
  :straight (:host github :repo "Alexander-Miller/i3wm-Config-Mode" :branch "master")
  :commands
  (i3wm-config-mode))

(use-package json-mode
  :mode
  ("Pipfile.lock\\'" . json-mode))

(use-package ledger-mode
  :defer t)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package markdown-preview-mode
  :defer t)

;; use ssh-specific modes for ssh configuration files
(use-package ssh-config-mode
  :defer t)

(use-package systemd
  :defer t)

(use-package terraform-mode
  :defer t)

(use-package toml-mode
  :mode
  ("Pipfile\\'" . toml-mode)
  :hook
  (toml-mode . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode 'toml-mode (cons "=" " = ")))

(use-package udev-mode
  :defer t)

(use-package yaml-mode
  :after docker-compose-mode
  :mode ("\\.yaml\\'" "\\.yml\\'" "group_vars/.+\\'")
  :hook (yaml-mode . flyspell-prog-mode))
;; Files:1 ends here

;; [[file:config.org::*Functions][Functions:1]]
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
;; Functions:1 ends here

;; [[file:config.org::*Miscellaneous][Miscellaneous:1]]
(use-package comint
  :straight (:type built-in)
  :custom
  (comint-buffer-maximum-size 20000)
  (comint-process-echoes t))

(use-package compile
  :straight (:type built-in)
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (ansi-color-for-compilation-mode t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'next-error))

(use-package esup
  :commands
  (esup))

(use-package files
  :straight (:type built-in)
  :hook
  (before-save . whitespace-cleanup)
  :custom
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (save-abbrevs 'silently)
  (require-final-newline t)
  (confirm-nonexistent-file-or-buffer nil))
;; Miscellaneous:1 ends here
