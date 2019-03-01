;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package python
  :delight "Py"
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  :config
  (add-hook 'python-mode-hook #'highlight-indentation-mode)
  (add-hook 'python-mode-hook #'electric-operator-mode))

(use-package pipenv
  :delight
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-default))

(provide 'config-python)
;;; config-python.el ends here
