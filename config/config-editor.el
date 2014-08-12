; configure UI appearance
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

; consistently ask yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistent-file-or-buffer nil)
(setq require-final-newline nil)
(setq mouse-yank-at-point t)
(setq vc-follow-symlinks t)

(setq
 ; preserve the vertical position of the line containing the point
 scroll-preserve-screen-position t
 ; never vertically recenter windows
 scroll-conservatively 100000
 scroll-margin 0)

(setq-default
 ; only allow continuation lines in buffers that occupy the full frame width
 truncate-lines nil
 truncate-partial-width-windows t)

; replace text in the active region with typed text
(delete-selection-mode t)

; highlight the current line (the line containing the point)
(global-hl-line-mode t)

; highlight matching bracket delimiters
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(setq blink-matching-paren nil)

; save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file
      (expand-file-name "places" user-emacs-state-directory))

; improved mechanism for making buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

; visualize unwanted whitespace characters and lines that are too long
(require 'whitespace)
(setq whitespace-line-column 100
      whitespace-style '(face tabs empty trailing lines-tail))
(whitespace-mode t)

; store backup files in a central directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-state-directory))))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

; open zsh files in sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . sh-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))
; open bash files in sh-mode
(add-to-list 'auto-mode-alist '("bashrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_profile$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_logout$" . sh-mode))
; open Debian preseed files in conf-mode
(add-to-list 'auto-mode-alist '(".preseed$" . conf-mode))

(provide 'config-editor)
