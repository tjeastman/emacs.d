;;; config-editor.el --- editor configuration

;;; Commentary:

;;; Code:

;; configure UI appearance
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; disable bold text after loading a theme
(defadvice load-theme (after disable-bold-text activate)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal))
   (face-list)))

;; consistently ask yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t)

(use-package mouse
  :defer t
  :custom
  (mouse-yank-at-point t))

(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks t))

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

(use-package comint
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
  (show-smartparens-global-mode +1)
  (setq blink-matching-paren nil))

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
  :diminish whitespace-mode
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (add-hook 'prog-mode-hook (lambda () (whitespace-mode +1))))

;; make it possible to undo and redo window configuration changes
(winner-mode t)

;; automatically revert buffers when the corresponding file changes
(global-auto-revert-mode t)

;; revert dired buffers when revisiting
(setq dired-auto-revert-buffer t)

;; store backup files in a central directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-state-directory))))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(use-package files
  :custom
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

(setq ispell-personal-dictionary "~/.aspell.en.pws")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'c-mode-hook 'flyspell-prog-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

;; enabled region case manipulation commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; enable buffer narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; open bash and zsh files in sh-mode
(use-package sh-script
  :mode (("\\.zsh$" . sh-mode)
         ("\\.zsh-theme$" . sh-mode)
         ("zshrc$" . sh-mode)
         (".zsh_personal$" . sh-mode)
         ("bashrc$" . sh-mode)
         ("bash_profile$" . sh-mode)
         ("bash_logout$" . sh-mode)
         ("profile$" . sh-mode)))

;; open Debian preseed files in conf-mode
(use-package conf-mode
  :mode ((".preseed$". conf-mode)))

(use-package recentf
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(provide 'config-editor)
;;; config-editor.el ends here
