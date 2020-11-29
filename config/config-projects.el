(use-package git-commit
  :custom
  (git-commit-summary-max-length 72))

(use-package git-gutter
  :commands git-gutter-mode)

(use-package git-messenger
  :bind
  ("C-x G" . git-messenger:popup-message)
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t))

(use-package git-timemachine
  :commands
  (git-timemachine
   git-timemachine-toggle))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook (lambda ()
                            (ibuffer-vc-set-filter-groups-by-vc-root)
                            (unless (eq ibuffer-sorting-mode 'alphabetic)
                              (ibuffer-do-sort-by-alphabetic)))))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ;; :hook
  ;; (magit-status-mode . magit-filenotify-mode)
  :custom
  (magit-repository-directories '(("~/projects/" . 1)))
  (magit-save-repository-buffers 'dontask)
  (magit-section-visibility-indicator nil)
  (magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package projectile
  :demand
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t)
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien)
  (projectile-project-search-path '("~/projects/"))
  (projectile-sort-order 'recentf)
  :config
  (projectile-mode +1))

(provide 'config-projects)
