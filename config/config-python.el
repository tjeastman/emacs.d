;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:
;; (use-package jedi
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup))

(use-package pyvenv
  :bind ("C-c w" . pyvenv-workon))

(use-package elpy
  :demand
  :diminish highlight-indentation-mode
  :bind ("C-c ," . elpy-multiedit)      ; FIX set for Python only?
  :config
  (elpy-enable)
  (elpy-use-ipython))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (use-package virtualenvwrapper)
;; (use-package cython-mode)
;; (use-package nose)

(use-package pip-requirements)

;; FIX: diminish highlight indentation minor mode (indicator ||)

(provide 'config-python)
;;; config-python.el ends here
