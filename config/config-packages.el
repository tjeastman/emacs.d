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
  '(ido-ubiquitous
    flx-ido
    smex
    git-timemachine
    yaml-mode
    smartparens
    markdown-mode
    zenburn-theme
    solarized-theme
    anti-zenburn-theme
    readline-complete
    scad-mode
    arduino-mode
    jinja2-mode
    ein
    cider
    paredit
    js2-mode
    scala-mode
    ansible-doc
    peep-dired
    use-package
    counsel
    kaolin-themes)
  "List of packages that are automatically installed.")

;; ensure that all required packages are installed
(let ((package-not-installed-p (lambda (pkg) (not (package-installed-p pkg)))))
  (if (delq nil (mapcar package-not-installed-p config-required-packages))
      (progn
        (package-refresh-contents)
        (dolist (package-name config-required-packages)
          (unless (package-installed-p package-name)
            (package-install package-name))))))

(provide 'config-packages)
;;; config-packages.el ends here
