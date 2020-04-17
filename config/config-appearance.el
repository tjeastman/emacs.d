(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

(if (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
    (menu-bar-mode -1))

(use-package zenburn-theme
  :if window-system
  :init
  (load-theme 'zenburn t))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package doom-modeline
  :config
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-height 15)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-minor-modes nil)
  :init
  (doom-modeline-mode 1))

(use-package hl-line
  :ensure nil
  :if window-system
  :config
  (global-hl-line-mode t))

(provide 'config-appearance)
