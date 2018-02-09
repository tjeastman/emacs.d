;;; config-files.el --- configure modes for specific file formats

;;; Commentary:

;;; Code:

;; use ssh-specific modes for ssh configuration files
(use-package ssh-config-mode
  :ensure t
  :mode
  ((".ssh/config$" . ssh-config-mode)
   ("sshd_config$" . ssh-config-mode)
   ("ssh_config$" . ssh-config-mode)
   ("known_hosts$" . ssh-known-hosts-mode)
   ("authorized_keys$" . ssh-authorized-keys-mode)))

(use-package markdown-mode
  :ensure t
  :commands
  (markdown-mode
   gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))
(use-package markdown-preview-mode
  :ensure t)

(use-package thrift
  :ensure t
  :mode "\\.thrift\\'")

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'"
  :hook (protobuf-mode . flyspell-prog-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package ansible-doc
  :ensure t
  :defer t
  :commands ansible-doc)

(use-package yaml-mode
  :ensure t
  :bind (:map yaml-mode-map ("C-c h a" . ansible-doc))
  :mode ("\\.yaml\\'" "\\.yml\\'" "group_vars/.+\\'")
  :hook (yaml-mode . flyspell-prog-mode)
  :config)

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.yml")

(use-package sh-script
  :ensure t
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
  :ensure t
  :mode
  ((".preseed$" . conf-mode)            ; Debian preseed files
   ("pylintrc$" . conf-mode)))

(use-package arduino-mode
  :ensure t
  :mode "\\.ino\\'")

(use-package systemd
  :ensure t)

(use-package dotenv-mode
  :ensure t)

(use-package apt-sources-list
  :ensure t)

(use-package json-mode
  :ensure t)

(provide 'config-files)
;;; config-files.el ends here
