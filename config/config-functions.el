(defun my-move-to-beginning-of-line ()
  "Incrementally move point toward the beginning of the line.

Movement stops at the first non-whitespace character of the current line if it is between the point
and the beginning of the line.  If the point is at or before the first non-whitespace character,
then move to the beginning of the line."
  (interactive)
  (let ((original-point-value (point)))
    (back-to-indentation)
    (if (<= original-point-value (point))
	(beginning-of-line))))

(defun my-run-new-shell-always ()
  "Run a shell in a new buffer regardless of how many shells are already running."
  (interactive)
  (let ((shell-buffer-index 0)
        (shell-buffer-name-format "*shell-%d*")
        (shell-buffer-name))
    (while ; loop until an unused shell buffer name is found
        (progn
          (incf shell-buffer-index)
          (setq shell-buffer-name (format shell-buffer-name-format shell-buffer-index))
          (get-buffer shell-buffer-name)))
    (shell shell-buffer-name)))

(provide 'config-functions)
