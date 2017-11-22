;;; config-keybindings.el --- configure global keybindings

;;; Commentary:

;;; Code:
(global-set-key (kbd "C-x y") 'browse-kill-ring)

(global-set-key (kbd "C-;") 'backward-kill-word)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key [remap move-beginning-of-line]
                'my-move-to-beginning-of-line)

;; lookup Ansible module documentation in YAML mode
(eval-after-load 'yaml-mode
  '(define-key yaml-mode-map (kbd "C-c h a") #'ansible-doc))

(provide 'config-keybindings)
;;; config-keybindings.el ends here
