;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator nil)
  (pyvenv-mode))

(use-package blacken
  :commands blacken-buffer)

(use-package py-isort
  :commands py-isort-before-save)

(use-package python
  :delight "Py"
  :mode ("\\.py\\'" . python-mode)
  :hook
  ((python-mode . electric-operator-mode)
   (python-mode . highlight-indentation-mode))
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))

(use-package python-pytest
  :bind
  ("C-c t" . python-pytest)
  ("C-x t" . python-pytest-popup)
  :custom
  (python-pytest-executable "python -m pytest"))

(use-package ein
  :commands
  (ein:notebooklist-login
   ein:jupyter-server-start))

(use-package eglot)

(provide 'config-python)
;;; config-python.el ends here
