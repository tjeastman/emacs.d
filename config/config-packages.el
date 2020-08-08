(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir
      (expand-file-name "packages" user-emacs-directory))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :custom
  (use-package-always-ensure t))

(use-package no-littering)

(provide 'config-packages)
