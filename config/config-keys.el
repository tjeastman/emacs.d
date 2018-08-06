;;; config-keys.el --- configure keyboard

;;; Commentary:

;;; Code:

(use-package ns-win
  :ensure nil
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super))

(provide 'config-keys)
;;; config-keys.el ends here
