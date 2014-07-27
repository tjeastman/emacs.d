; configure packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-user-dir (expand-file-name "packages" user-emacs-directory))

; install required packages
(package-initialize)
(package-refresh-contents)
(dolist (package-name '(cask pallet))
  (unless (package-installed-p package-name)
    (package-install package-name)))

(provide 'config-packages)
