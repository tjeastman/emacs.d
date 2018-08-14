;;; config-functions.el --- global function definitions

;;; Commentary:

;;; Code:

(defun my-run-new-shell-always ()
  "Run a shell in a new buffer regardless of how many shells are already running."
  (interactive)
  (let ((shell-buffer-index 0)
        (shell-buffer-name-format "*shell-%d*")
        (shell-buffer-name))
    (while ;; loop until an unused shell buffer name is found
        (progn
          (setq shell-buffer-index (1+ shell-buffer-index))
          (setq shell-buffer-name (format shell-buffer-name-format shell-buffer-index))
          (get-buffer shell-buffer-name)))
    (shell shell-buffer-name)))

(provide 'config-functions)
;;; config-functions.el ends here
