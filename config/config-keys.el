(use-package ns-win
  :straight (:type built-in)
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super))

(provide 'config-keys)
