;;; config-packages.el --- package manager configuration

;;; Commentary:

;;; Code:

;; configure packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-user-dir
      (expand-file-name "packages" user-emacs-directory))

(package-initialize)

;; declare required packages
(defvar config-required-packages
  '(solarized-theme
    anti-zenburn-theme
    kaolin-themes
    blackboard-theme
    lush-theme
    doneburn-theme
    paredit
    use-package)
  "List of packages that are automatically installed.")

;; ensure that all required packages are installed
(let ((package-not-installed-p (lambda (pkg) (not (package-installed-p pkg)))))
  (if (delq nil (mapcar package-not-installed-p config-required-packages))
      (progn
        (package-refresh-contents)
        (dolist (package-name config-required-packages)
          (unless (package-installed-p package-name)
            (package-install package-name))))))

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (use-package-always-defer t))

(use-package diminish)

(provide 'config-packages)
;;; config-packages.el ends here
