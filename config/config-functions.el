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

(defun my-switch-to-last-window ()
  "Switch to the previously used window if one exist."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (if win
        (let ((frame (window-frame win)))
          (raise-frame frame)
          (select-frame frame)
          (select-window win)))))

(defun my-copy-filename-to-clipboard ()
  "Copy filename corresponding to the current buffer to clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename))))

(provide 'config-functions)
;;; config-functions.el ends here
