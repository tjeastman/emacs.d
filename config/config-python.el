;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package pyvenv
  :bind
  (("C-c w" . pyvenv-workon)
   ("C-c a" . pyvenv-activate)))

(use-package highlight-indentation
  :diminish highlight-indentation-mode)

(use-package company-jedi
  :hook
  (python-mode . (lambda () (progn (jedi:setup) (add-to-list 'company-backends 'company-jedi))))
  :custom
  (jedi:complete-on-dot t)
  (jedi:setup-keys t))

(use-package python
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))

(use-package elpy
  :custom
  (elpy-rpc-backend "jedi")
  :config
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package python-pytest)

(use-package virtualenvwrapper)
(use-package cython-mode)
(use-package nose)

(use-package pip-requirements)

(use-package ein
  :commands ein:notebooklist-open)

(provide 'config-python)
;;; config-python.el ends here
