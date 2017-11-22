;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:
;; (use-package jedi
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup))

(use-package elpy
  :config
  (elpy-enable)
  (elpy-use-ipython))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook (lambda () (subword-mode +1)))

;; (use-package virtualenvwrapper)
;; (use-package cython-mode)
;; (use-package nose)

(use-package pip-requirements)

(provide 'config-python)
;;; config-python.el ends here
