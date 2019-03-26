;;; config-python.el -- Python configuration

;;; Commentary:

;;; Code:

(use-package pipenv
  :delight
  :hook (python-mode . pipenv-mode)
  :custom
  (pipenv-executable
   (expand-file-name
    (concat (file-name-as-directory "scripts") "pipenv") user-emacs-directory))
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-default))

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
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))

(use-package ein)

(provide 'config-python)
;;; config-python.el ends here
