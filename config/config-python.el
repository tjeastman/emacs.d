;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package pipenv
  :delight
  :commands pipenv-mode
  :custom
  (pipenv-executable
   (expand-file-name
    (concat (file-name-as-directory "scripts") "pipenv") user-emacs-directory))
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-default))

(use-package pyvenv
  :commands
  (pyvenv-activate
   pyvenv-workon))

(use-package blacken
  :delight
  :commands blacken-mode)

(use-package py-isort
  :commands
  (py-isort-region
   py-isort-buffer
   py-isort-before-save))

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
  :commands
  (python-pytest
   python-pytest-popup)
  :bind
  ("C-c t" . python-pytest)
  :custom
  (python-pytest-executable "python -m pytest"))

(use-package ein
  :commands
  (ein:notebooklist-login
   ein:jupyter-server-start))

(use-package lsp-mode)
(use-package lsp-python-ms
  :after lsp-mode
  :load-path "contrib/lsp-python-ms/")

(provide 'config-python)
;;; config-python.el ends here
