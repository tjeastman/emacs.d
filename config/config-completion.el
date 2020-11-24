(use-package company
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 3)
  (company-tooltip-limit 10)
  (company-tooltip-flip-when-above t)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  :config
  (global-company-mode t))

(provide 'config-completion)
