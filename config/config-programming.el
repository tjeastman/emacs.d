;; General

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package electric-operator
  :commands
  (electric-operator-mode
   electric-operator-get-rules-for-mode
   electric-operator-add-rules-for-mode))

(use-package highlight-indentation
  :commands
  highlight-indentation-mode)

(use-package lsp-mode
  :commands lsp)

(use-package prog-mode
  :ensure nil
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

;; C/C++

(use-package clang-format
  :commands
  (clang-format-region
   clang-format-buffer))

;; Go

(use-package go-mode
  :hook
  (go-mode . (lambda ()
               (setq tab-width 4)
               (setq indent-tabs-mode 1)
               (add-hook 'before-save-hook 'gofmt-before-save nil t))))

;; Python

(use-package blacken
  :commands blacken-buffer)

(use-package ein
  :commands
  (ein:notebooklist-login
   ein:jupyter-server-start))

(use-package pip-requirements
  :custom
  (pip-requirements-index-url nil))

(use-package py-isort
  :commands py-isort-before-save)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook
  (python-mode . highlight-indentation-mode)
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))

(use-package python-pytest
  :bind
  ("C-c t" . python-pytest)
  ("C-x t" . python-pytest-popup)
  :custom
  (python-pytest-executable "python -m pytest"))

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator nil)
  (pyvenv-mode))

(provide 'config-programming)
