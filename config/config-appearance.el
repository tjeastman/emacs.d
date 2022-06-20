(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 4))

(if (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
    (menu-bar-mode -1))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package dimmer
  :custom
  (dimmer-adjustment-mode :both)
  :init
  (dimmer-mode t))

(use-package doom-modeline
  :custom
  (doom-modeline-bar-width 1)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-env-enable-python t)
  (doom-modeline-height 15)
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes nil)
  :init
  (doom-modeline-mode 1))

(use-package doom-themes
  :if window-system
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-solarized-light t))

(use-package frame
  :ensure nil
  :bind
  ("M-RET" . toggle-frame-fullscreen))

(use-package hl-line
  :ensure nil
  :if window-system
  :config
  (global-hl-line-mode t))

(provide 'config-appearance)
