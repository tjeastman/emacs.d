(global-set-key (kbd "M-O") 'my-switch-to-last-window)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

(use-package ns-win
  :ensure nil
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta))

(provide 'config-keys)
