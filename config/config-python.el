;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package pipenv
  :delight
  :commands pipenv-mode
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-default))

(use-package blacken
  :commands blacken-mode)

(use-package python
  :delight "Py"
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  :config
  (add-hook 'python-mode-hook #'pipenv-mode)
  (add-hook 'python-mode-hook #'blacken-mode)
  (add-hook 'python-mode-hook #'highlight-indentation-mode)
  (add-hook 'python-mode-hook #'electric-operator-mode))

(provide 'config-python)
;;; config-python.el ends here
