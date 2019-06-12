;;; config-appearance.el --- configure appearance of the Emacs UI

;;; Commentary:

;;; Code:

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

(if (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
    (menu-bar-mode -1))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(provide 'config-appearance)
;;; config-appearance.el ends here
