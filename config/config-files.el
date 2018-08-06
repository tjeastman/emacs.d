;;; config-files.el --- configure modes for specific file formats

;;; Commentary:

;;; Code:

;; use ssh-specific modes for ssh configuration files
(use-package ssh-config-mode
  :mode
  ((".ssh/config$" . ssh-config-mode)
   ("sshd_config$" . ssh-config-mode)
   ("ssh_config$" . ssh-config-mode)
   ("known_hosts$" . ssh-known-hosts-mode)
   ("authorized_keys$" . ssh-authorized-keys-mode)))

(use-package markdown-mode
  :commands
  (markdown-mode
   gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))
(use-package markdown-preview-mode)

(use-package thrift
  :mode "\\.thrift\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook (protobuf-mode . flyspell-prog-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package ansible-doc
  :commands ansible-doc)

(use-package yaml-mode
  :bind (:map yaml-mode-map ("C-c h a" . ansible-doc))
  :mode ("\\.yaml\\'" "\\.yml\\'" "group_vars/.+\\'")
  :hook (yaml-mode . flyspell-prog-mode)
  :config)

(use-package docker-compose-mode
  :mode "docker-compose.yml")

(use-package sh-script
  :mode
  (("\\.zsh$" . sh-mode)          ; standard bash and zsh files
   ("\\.zsh-theme$" . sh-mode)
   ("zshrc$" . sh-mode)
   (".zsh_personal$" . sh-mode)
   ("bashrc$" . sh-mode)
   ("bash_profile$" . sh-mode)
   ("bash_logout$" . sh-mode)
   ("profile$" . sh-mode)))

(use-package conf-mode
  :mode
  ((".preseed$" . conf-mode)            ; Debian preseed files
   ("pylintrc$" . conf-mode)))

(use-package arduino-mode
  :mode "\\.ino\\'")

(use-package systemd)

(use-package dotenv-mode)

(use-package apt-sources-list)

(use-package json-mode)

(use-package cuda-mode)

(use-package go-mode)

(use-package git-modes
  :load-path "contrib/git-modes/"
  :mode
  ("/\\.gitconfig\\'" . gitconfig-mode)
  ("/\\.git/config\\'" . gitconfig-mode)
  ("/\\.gitignore\\'" . gitignore-mode)
  ("/.dockerignore\\'" . gitignore-mode))

(provide 'config-files)
;;; config-files.el ends here
