;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:
(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup))

(add-hook 'python-mode-hook (lambda () (subword-mode +1)))

(use-package virtualenvwrapper)

(use-package cython-mode)
(use-package nose)

(provide 'config-python)
;;; config-python.el ends here
