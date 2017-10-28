; enable flycheck in python-mode
(require 'flycheck)
(add-hook 'python-mode-hook (lambda () (flycheck-mode)))

; enable access to rope refactoring library for Python through Pymacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-guess-project t)
(setq ropemacs-enable-autoimport t)
(setq ropemacs-autoimport-modules '("os" "sys"))

(add-hook 'python-mode-hook (lambda () (subword-mode +1)))

(provide 'config-python)
