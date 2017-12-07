;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package pyvenv
  :ensure t
  :bind
  (("C-c w" . pyvenv-workon)
   ("C-c a" . pyvenv-activate)))

(use-package highlight-indentation
  :ensure t
  :defer t
  :diminish highlight-indentation-mode)

(use-package company-jedi
  :ensure t
  :defer t
  :hook
  (python-mode . (lambda () (progn (jedi:setup) (add-to-list 'company-backends 'company-jedi))))
  :custom
  (jedi:complete-on-dot t)
  (jedi:setup-keys t))

(use-package python
  :defer t
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))

(use-package elpy
  :ensure t
  :custom
  (elpy-rpc-backend "jedi")
  :config
  (elpy-enable)
  (elpy-use-ipython)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; (use-package virtualenvwrapper)
;; (use-package cython-mode)
;; (use-package nose)

(use-package pip-requirements
  :ensure t)

(use-package ein
  :ensure t
  :commands ein:notebooklist-open)

(provide 'config-python)
;;; config-python.el ends here
