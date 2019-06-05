;;; config-files.el --- configure modes for specific file formats

;;; Commentary:

;;; Code:

(use-package ansible-doc
  :commands ansible-doc)

(use-package apt-sources-list
  :defer t)

(use-package arduino-mode
  :defer t)

(use-package conf-mode
  :mode
  ((".preseed$" . conf-mode)
   ("pylintrc$" . conf-mode)))

(use-package dockerfile-mode
  :defer t)
(use-package docker-compose-mode
  :defer t)

(use-package dotenv-mode
  :defer t)

(use-package git-modes
  :load-path "contrib/git-modes/"
  :mode
  ("/\\.gitconfig\\'" . gitconfig-mode)
  ("/\\.git/config\\'" . gitconfig-mode)
  ("/\\.gitignore\\'" . gitignore-mode)
  ("/.dockerignore\\'" . gitignore-mode))

(use-package go-mode
  :defer t)

(use-package groovy-mode
  :defer t)

(use-package i3wm-config-mode
  :load-path "contrib/i3wm-Config-Mode/"
  :commands i3wm-config-mode)

(use-package json-mode
  :mode
  ("Pipfile.lock\\'" . json-mode))

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))
(use-package markdown-preview-mode)

(use-package nim-mode)

(use-package pip-requirements
  :defer t)

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook (protobuf-mode . flyspell-prog-mode))

(use-package sh-script
  :mode
  (("\\.zsh$" . sh-mode)
   ("\\.zsh-theme$" . sh-mode)
   ("zshrc$" . sh-mode)
   (".zsh_personal$" . sh-mode)
   ("bashrc$" . sh-mode)
   ("bash_profile$" . sh-mode)
   ("bash_logout$" . sh-mode)
   ("profile$" . sh-mode)))

;; use ssh-specific modes for ssh configuration files
(use-package ssh-config-mode
  :defer t)

(use-package systemd
  :defer t)

(use-package terraform-mode
  :defer t)

(use-package thrift
  :mode ("\\.thrift\\'" . thrift-mode))

(use-package toml-mode
  :mode
  ("Pipfile\\'" . toml-mode))

(use-package yaml-mode
  :after docker-compose-mode
  :bind (:map yaml-mode-map ("C-c h a" . ansible-doc))
  :mode ("\\.yaml\\'" "\\.yml\\'" "group_vars/.+\\'")
  :hook (yaml-mode . flyspell-prog-mode))

(provide 'config-files)
;;; config-files.el ends here
