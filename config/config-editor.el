; configure UI appearance
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

; disable bold text after loading a theme
(defadvice load-theme (after disable-bold-text activate)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal))
   (face-list)))

; consistently ask yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t)

(setq confirm-nonexistent-file-or-buffer nil)
(setq require-final-newline t)
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

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq
 comint-process-echoes t
 comint-prompt-read-only t)

; replace text in the active region with typed text
(delete-selection-mode t)

; highlight the current line (the line containing the point)
(global-hl-line-mode t)

; show the current line number and indicate the buffer size in the mode line
(line-number-mode t)
(size-indication-mode t)

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
(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode +1)))

; make it possible to undo and redo window configuration changes
(winner-mode t)

; automatically revert buffers when the corresponding file changes
(global-auto-revert-mode t)

; store backup files in a central directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-state-directory))))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

; set up abbreviations
(setq-default abbrev-mode t)
(setq abbrev-file-name (expand-file-name "abbreviations" user-emacs-directory))
(quietly-read-abbrev-file abbrev-file-name)
(setq save-abbrevs 'silently)

(setq ispell-personal-dictionary "~/.aspell.en.pws")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'c-mode-hook 'flyspell-prog-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

; enabled region case manipulation commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
; enable buffer narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

; open zsh files in sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . sh-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".zsh_personal$" . sh-mode))
; open bash files in sh-mode
(add-to-list 'auto-mode-alist '("bashrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_profile$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_logout$" . sh-mode))
; open Debian preseed files in conf-mode
(add-to-list 'auto-mode-alist '(".preseed$" . conf-mode))

(provide 'config-editor)
