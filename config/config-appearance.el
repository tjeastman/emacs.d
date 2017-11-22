;;; config-appearance.el --- configure appearance of the Emacs UI

;;; Commentary:

;;; Code:

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; disable bold text after loading a theme
(defadvice load-theme (after disable-bold-text activate)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal))
   (face-list)))

(load-theme 'zenburn t)

(provide 'config-appearance)
;;; config-appearance.el ends here
