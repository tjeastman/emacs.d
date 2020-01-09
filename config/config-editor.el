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

;; enabled region case manipulation commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; enable buffer narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(use-package abbrev
  :ensure nil
  :delight
  :custom
  (abbrev-file-name (expand-file-name "abbreviations" user-emacs-directory))
  :config
  (setq-default abbrev-mode t)
  (quietly-read-abbrev-file abbrev-file-name))

(use-package autorevert
  :config
  (global-auto-revert-mode t))

(use-package cc-mode
  :hook
  (c++-mode . electric-operator-mode))

(use-package comint
  :ensure nil
  :custom
  (comint-buffer-maximum-size 20000)
  (comint-process-echoes t))

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

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)          ; revert dired buffers when revisiting
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (if (eq system-type 'darwin)
      (setq dired-use-ls-dired nil)))

(use-package eldoc
  :delight)

(use-package elisp-mode
  :after electric-operator
  :ensure nil
  :delight
  (emacs-lisp-mode "ELisp")
  :hook
  (emacs-lisp-mode . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode 'emacs-lisp-mode (cons "." " . ")))

(use-package files
  :ensure nil
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

(use-package flyspell
  :delight
  (flyspell-mode
   flyspell-prog-mode))

(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package ispell
  :custom
  (ispell-personal-dictionary "~/.aspell.en.pws")
  (ispell-program-name "aspell"))

(use-package make-mode
  :mode
  ("Make.rules" . makefile-mode)
  :hook
  (makefile-mode . (lambda () (whitespace-toggle-options '(tabs)))))

(use-package mouse
  :ensure nil
  :custom
  (mouse-yank-at-point t))

(use-package prog-mode
  :ensure nil
  :hook
  ((prog-mode . turn-on-smartparens-strict-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package recentf
  :demand
  :bind
  ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250)
  :config
  (recentf-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package simple
  :ensure nil
  :bind ("C-;" . backward-kill-word)
  :custom
  (next-line-add-newlines t)
  :config
  (line-number-mode t)
  (size-indication-mode t))

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

(use-package subword
  :delight
  :config
  (global-subword-mode))

(use-package text-mode
  :ensure nil
  :hook
  (text-mode . flyspell-mode))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package vc
  :custom
  (vc-follow-symlinks t))

(use-package which-func
  :config
  (which-function-mode 1))

;; visualize unwanted whitespace characters and lines that are too long
(use-package whitespace
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face tabs empty trailing lines-tail)))

(use-package window
  :ensure nil
  :bind
  ("M-o" . other-window))

(provide 'config-editor)
;;; config-editor.el ends here
