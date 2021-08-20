;; General

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package cmake-mode)

(use-package cmake-font-lock
  :after cmake-mode
  :hook
  (cmake-mode . cmake-font-lock-activate))

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
  highlight-indentation-mode)

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
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-pyright)

(use-package platformio-mode
  :commands
  (platformio-mode
   platformio-conditionally-enable))

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

(use-package which-func
  :config
  (which-function-mode 1))

;; C/C++

(use-package clang-format
  :commands
  (clang-format-region
   clang-format-buffer))

(use-package modern-cpp-font-lock
  :commands modern-c++-font-lock-mode)

;; Cojure

(use-package cider)

(use-package clojure-mode)

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
