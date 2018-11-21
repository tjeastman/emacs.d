;;; config-keys.el --- configure keyboard

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-;") 'backward-kill-word)

(global-set-key (kbd "M-O") 'my-switch-to-last-window)

(use-package ns-win
  :ensure nil
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta))

(provide 'config-keys)
;;; config-keys.el ends here
