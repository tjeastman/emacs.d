;;; config-editor.el --- configure features built into the editor

;;; Commentary:

;;; Code:

;; consistently ask yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t)
(if (eq system-type 'darwin)
    (setq ring-bell-function 'ignore))

(setq
 ;; preserve the vertical position of the line containing the point
 scroll-preserve-screen-position t
 ;; never vertically recenter windows
 scroll-conservatively 100000
 scroll-margin 0)

;; prefer horizontal splits
(setq split-height-threshold 9999)

;; hide cursor in windows that are not selected
(setq-default cursor-in-non-selected-windows nil)

(setq-default
 ;; only allow continuation lines in buffers that occupy the full frame width
 truncate-lines nil
 truncate-partial-width-windows t)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(add-to-list 'completion-ignored-extensions ".jar")
(add-to-list 'completion-ignored-extensions ".elf")
(add-to-list 'completion-ignored-extensions ".hex")
(add-to-list 'completion-ignored-extensions ".db")
(add-to-list 'completion-ignored-extensions "__pycache__/")
(add-to-list 'completion-ignored-extensions "a.out")
(add-to-list 'completion-ignored-extensions ".pyc")
(add-to-list 'completion-ignored-extensions ".cache/")
(add-to-list 'completion-ignored-extensions ".python-environments/")
(add-to-list 'completion-ignored-extensions ".coverage")

(use-package mouse
  :ensure nil
  :custom
  (mouse-yank-at-point t))

(use-package vc
  :custom
  (vc-follow-symlinks t))

(use-package comint
  :ensure nil
  :custom
  (comint-buffer-maximum-size 20000)
  (comint-process-echoes t)
  (comint-prompt-read-only t))

;; replace text in the active region with typed text
(delete-selection-mode t)

;; highlight the current line (the line containing the point)
(global-hl-line-mode t)

;; show the current line number and indicate the buffer size in the mode line
(line-number-mode t)
(size-indication-mode t)

;; highlight matching bracket delimiters
(use-package smartparens
  :delight
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))

;; save point position between sessions
(use-package saveplace
  :custom
  (save-place-file (expand-file-name "places" user-emacs-state-directory))
  :config
  (save-place-mode 1))

;; improved mechanism for making buffer names unique
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; visualize unwanted whitespace characters and lines that are too long
(use-package whitespace
  :diminish
  (global-whitespace-mode
   whitespace-mode
   whitespace-newline-mode)
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face tabs empty trailing lines-tail))
  :hook
  ((prog-mode . (lambda () (whitespace-mode +1)))
   (makefile-mode . (lambda () (whitespace-toggle-options '(tabs))))
   (before-save . whitespace-cleanup)))

;; make it possible to undo and redo window configuration changes
(use-package winner
  :config
  (winner-mode t))

;; automatically revert buffers when the corresponding file changes
(use-package autorevert
  :diminish
  (auto-revert-mode
   auto-revert-tail-mode
   global-auto-revert-mode)
  :config
  (global-auto-revert-mode t))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)          ; revert dired buffers when revisiting
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package files
  :ensure nil
  :custom
  ;; store backup files in a central directory
  (backup-directory-alist
   `(("." . ,(expand-file-name "backups" user-emacs-state-directory))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (save-abbrevs 'silently)
  (require-final-newline t)
  (confirm-nonexistent-file-or-buffer nil))

;; set up abbreviations
(use-package abbrev
  :ensure nil
  :delight
  :custom
  (abbrev-file-name (expand-file-name "abbreviations" user-emacs-directory))
  :config
  (setq-default abbrev-mode t)
  (quietly-read-abbrev-file abbrev-file-name))

(use-package ispell
  :custom
  (ispell-personal-dictionary "~/.aspell.en.pws")
  (ispell-program-name "aspell"))

(use-package flyspell
  :delight
  (flyspell-mode
   flyspell-prog-mode)
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

;; enabled region case manipulation commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; enable buffer narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(use-package recentf
  :demand
  :bind
  ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-state-directory))
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250)
  :config
  (recentf-mode 1))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))
(use-package ibuffer-vc
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package subword
  :delight
  :hook
  ((prog-mode . subword-mode)
   (yaml-mode . subword-mode)
   (protobuf-mode . subword-mode)))

(use-package which-func
  :config
  (which-function-mode 1))

(use-package simple
  :ensure nil
  :custom
  (next-line-add-newlines t))

(use-package make-mode
  :mode ("Make.rules" . makefile-mode))

(use-package compile
  :hook
  (compilation-filter
   . (lambda ()
       (let ((inhibit-read-only t))
         (ansi-color-apply-on-region compilation-filter-start (point)))))
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'next-error)
  (compilation-environment '("TERM=eterm-color")))

(use-package window
  :ensure nil
  :bind
  ("M-o" . other-window))

(use-package elisp-mode
  :ensure nil
  :delight
  (emacs-lisp-mode "ELisp"))

(provide 'config-editor)
;;; config-editor.el ends here
