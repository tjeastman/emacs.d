; configure packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-user-dir
      (expand-file-name "packages" user-emacs-directory))

; declare required packages
(defvar config-required-packages
  '(ido-ubiquitous
    flx-ido
    smex
    expand-region
    magit
    volatile-highlights
    company)
  "List of packages that are automatically installed.")

; install required packages
(package-initialize)
(package-refresh-contents)
(dolist (package-name config-required-packages)
  (unless (package-installed-p package-name)
    (package-install package-name)))

(provide 'config-packages)
