;;; config-editor.el --- editor configuration

;;; Commentary:

;;; Code:

;; consistently ask yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t)

(setq
 ;; preserve the vertical position of the line containing the point
 scroll-preserve-screen-position t
 ;; never vertically recenter windows
 scroll-conservatively 100000
 scroll-margin 0)

(setq-default
 ;; only allow continuation lines in buffers that occupy the full frame width
 truncate-lines nil
 truncate-partial-width-windows t)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(use-package mouse
  :defer t
  :custom
  (mouse-yank-at-point t))

(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks t))

(use-package comint
  :defer t
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
(use-package smartparens-config
  :config
  (show-smartparens-global-mode +1))

;; save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file
        (expand-file-name "places" user-emacs-state-directory)))

;; improved mechanism for making buffer names unique
(use-package uniquify
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
  :config
  (add-hook 'prog-mode-hook (lambda () (whitespace-mode +1))))

;; make it possible to undo and redo window configuration changes
(use-package winner
  :defer t
  :config
  (winner-mode t))

;; automatically revert buffers when the corresponding file changes
(use-package autorevert
  :diminish autorevert-mode
  :config
  (global-auto-revert-mode t))

(use-package dired
  :custom
  (dired-auto-revert-buffer t))         ; revert dired buffers when revisiting

(use-package files
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
  :diminish abbrev-mode
  :custom
  (abbrev-file-name (expand-file-name "abbreviations" user-emacs-directory))
  :config
  (setq-default abbrev-mode t)
  (quietly-read-abbrev-file abbrev-file-name))

(use-package ispell
  :defer t
  :custom
  (ispell-personal-dictionary "~/.aspell.en.pws"))

(use-package flyspell
  :diminish (flyspell-mode flyspell-prog-mode)
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; enabled region case manipulation commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; enable buffer narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(use-package sh-script
  :mode (("\\.zsh$" . sh-mode)          ; standard bash and zsh files
         ("\\.zsh-theme$" . sh-mode)
         ("zshrc$" . sh-mode)
         (".zsh_personal$" . sh-mode)
         ("bashrc$" . sh-mode)
         ("bash_profile$" . sh-mode)
         ("bash_logout$" . sh-mode)
         ("profile$" . sh-mode)))

(use-package conf-mode
  :mode ((".preseed$". conf-mode)))     ; Debian preseed files

(use-package recentf
  :demand
  :defer t
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250))

(use-package browse-kill-ring
  :bind ("C-x y" . browse-kill-ring))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package subword
  :diminish subword-mode
  :config
  (add-hook 'prog-mode-hook (lambda () (subword-mode +1))))

(provide 'config-editor)
;;; config-editor.el ends here
