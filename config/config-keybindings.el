(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-;") 'backward-kill-word)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key [remap move-beginning-of-line]
                'my-move-to-beginning-of-line)

(provide 'config-keybindings)
