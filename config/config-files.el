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

(use-package protobuf-mode
  :hook
  ((protobuf-mode . flyspell-prog-mode)
   (protobuf-mode . electric-operator-mode))
  :config
  (electric-operator-add-rules-for-mode 'protobuf-mode (cons "=" " = ")))

(use-package sh-script
  :mode
  (("zshrc\\'" . sh-mode)
   (".zsh_personal\\'" . sh-mode)
   ("\\.zsh-theme\\'" . sh-mode)))

;; use ssh-specific modes for ssh configuration files
(use-package ssh-config-mode
  :defer t)

(use-package systemd
  :defer t)

(use-package terraform-mode
  :defer t)

(use-package toml-mode
  :mode
  ("Pipfile\\'" . toml-mode)
  :hook
  (toml-mode . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode 'toml-mode (cons "=" " = ")))

(use-package yaml-mode
  :after docker-compose-mode
  :bind (:map yaml-mode-map ("C-c h a" . ansible-doc))
  :mode ("\\.yaml\\'" "\\.yml\\'" "group_vars/.+\\'")
  :hook (yaml-mode . flyspell-prog-mode))

(provide 'config-files)
