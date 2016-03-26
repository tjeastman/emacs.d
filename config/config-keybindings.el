(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x y") 'browse-kill-ring)

(global-set-key (kbd "C-;") 'backward-kill-word)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key [remap move-beginning-of-line]
                'my-move-to-beginning-of-line)

; lookup Ansible module documentation in YAML mode
(eval-after-load 'yaml-mode
  '(define-key yaml-mode-map (kbd "C-c h a") #'ansible-doc))

(provide 'config-keybindings)
