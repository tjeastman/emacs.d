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


(provide 'config-functions)
