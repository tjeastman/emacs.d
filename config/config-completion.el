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

(use-package company-lsp
  :after company
  :custom
  (company-lsp-async t)
  (company-lsp-enable-recompletion t)
  :config
  (add-to-list 'company-backends 'company-lsp))

(provide 'config-completion)
