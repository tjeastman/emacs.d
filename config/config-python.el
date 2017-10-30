;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:
(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup))

; enable access to rope refactoring library for Python through Pymacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-guess-project t)
(setq ropemacs-enable-autoimport t)
(setq ropemacs-autoimport-modules '("os" "sys"))

(add-hook 'python-mode-hook (lambda () (subword-mode +1)))

(use-package virtualenvwrapper)

(provide 'config-python)
;;; config-python.el ends here
