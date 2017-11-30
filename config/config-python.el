;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package pyvenv
  :bind ("C-c w" . pyvenv-workon))

(use-package highlight-indentation
  :defer t
  :diminish highlight-indentation-mode)

(use-package company-jedi
  :ensure t
  :defer t
  :custom
  (jedi:complete-on-dot t))

(use-package elpy
  :demand
  :bind ("C-c ," . elpy-multiedit)      ; FIX set for Python only?
  :custom
  (elpy-rpc-backend "jedi")
  :config
  (elpy-enable)
  (elpy-use-ipython)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-to-list 'company-backends 'company-jedi))

;; (use-package virtualenvwrapper)
;; (use-package cython-mode)
;; (use-package nose)

(use-package pip-requirements)

(use-package ein
  :commands ein:notebooklist-open)

(provide 'config-python)
;;; config-python.el ends here
